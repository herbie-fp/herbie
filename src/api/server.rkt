#lang racket

(require openssl/sha1)

(require "sandbox.rkt"
         "../config.rkt"
         "../syntax/read.rkt"
         "../syntax/load-plugin.rkt")
(require (submod "../utils/timeline.rkt" debug))

(provide completed-job?
         make-path
         get-results-for
         get-improve-job-data
         job-count
         is-server-up
         create-job
         start-job
         is-job-finished
         wait-for-job
         start-job-server)

#| Job Server Public API section |#
; computes the path used for server URLs
(define (make-path id)
  (format "~a.~a" id *herbie-commit*))

; Helers to isolated *completed-jobs*
(define (completed-job? id)
  (hash-has-key? *completed-jobs* id))

; Returns #f is now job exsist for the given job-id
(define (get-results-for id)
  (hash-ref *completed-jobs* id #f))

; I don't like how specific this function is but it keeps the API boundary.
(define (get-improve-job-data)
  (for/list ([(k v) (in-hash *completed-jobs*)] #:when (equal? (job-result-command v) 'improve))
    (get-table-data v (make-path k))))

(define (job-count)
  (hash-count *job-status*))

(define (is-server-up)
  #t)

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

;; Starts a job for a given command object|
(define (start-job command)
  (define job-id (compute-job-id command))
  (if (already-computed? job-id) job-id (start-work command)))

(define (is-job-finished job-id)
  (hash-ref *job-status* job-id #f))

(define (wait-for-job job-id)
  (if (already-computed? job-id) (hash-ref *completed-jobs* job-id) (internal-wait-for-job job-id)))

(define (start-job-server config global-demo global-output)
  (build-worker-pool 4)
  ;; Pass along local global values
  ;; TODO can I pull these out of config or not need ot pass them along.
  (set! *demo?* global-demo)
  (set! *demo-output* global-output))

#| End Job Server Public API section |#

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:prefab)

; Private globals
; TODO I'm sure these can encapslated some how.
(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))
(define *completed-jobs* (make-hash))
(define *job-status* (make-hash))
(define *job-sema* (make-hash))

;; place channels in progress. "workers"
(define *inprogress* (make-hash))
(define *ready-workers* (list))

(define (work-on command)
  (define job-id (compute-job-id command))
  ;; Pop off 1st ready worker
  (define worker (first *ready-workers*))
  (set! *ready-workers* (cdr *ready-workers*))
  ;; Send work to worker
  (place-channel-put worker `(apply ,worker ,command ,job-id))
  (hash-set! *inprogress* job-id worker)
  (eprintf "Work started: ~a\n" job-id)
  job-id)

(define (build-worker-pool number-of-workers)
  (define workers
    (for/list ([wid (in-range number-of-workers)])
      (make-worker)))
  (for/list ([worker workers])
    (place-dead-evt worker))
  (set! *ready-workers* workers))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))
(define (make-worker)
  (place/context*
   ch
   #:parameters (*flags* *num-iterations*
                         *num-points*
                         *timeout*
                         *reeval-pts*
                         *node-limit*
                         *max-find-range-depth*
                         *pareto-mode*
                         *platform-name*
                         *loose-plugins*)
   (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
     (load-herbie-plugins))
   (for ([_ (in-naturals)])
     (match (place-channel-get ch)
       [(list 'apply self command id)
        (hash-set! *job-status* id (*timeline*))
        (eprintf "Job [~a] being worked on.\n" id)
        (define herbie-result (wrapper-run-herbie command id))
        (match-define (job-result kind test status time _ _ backend) herbie-result)
        (define out-result
          (match kind
            ['alternatives #f]
            ['evaluate #f]
            ['cost #f]
            ['errors #f]
            ['exacts #f]
            ['improve (eprintf "HERE:~a\n" herbie-result)]
            ['local-error #f]
            ['sample #f]
            [_ (error 'compute-result "unknown command ~a" kind)]))
        (hash-set! *job-status* id #f)
        (place-channel-put ch (list 'done out-result))]
       [(list 'check id)
        (eprintf "checking ~a\n" id)
        (place-channel-put ch (list 'done (list 'inprogress (hash-ref *job-status* id #f))))]))))

(define (already-computed? job-id)
  (or (hash-has-key? *completed-jobs* job-id)
      (and (*demo-output*) (directory-exists? (build-path (*demo-output*) (make-path job-id))))))

(define (internal-wait-for-job job-id)
  (eprintf "Waiting for job\n")
  (define sema (hash-ref *job-sema* job-id))
  (semaphore-wait sema)
  (hash-remove! *job-sema* job-id)
  (hash-ref *completed-jobs* job-id))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

; Encapsulates semaphores and async part of jobs.
(define (start-work job)
  (define job-id (compute-job-id job))
  (work-on job)
  ; (hash-set! *job-status* job-id (*timeline*))
  ; (define sema (make-semaphore))
  ; (hash-set! *job-sema* job-id sema)
  ; (thread-send *worker-thread* (work job-id job sema))
  job-id)

; Handles semaphore and async part of a job
(struct work (id job sema))

(define (run-job job-info)
  (match-define (work job-id info sema) job-info)
  (define path (make-path job-id))
  (cond ;; Check caches if job as already been completed
    [(hash-has-key? *completed-jobs* job-id) (semaphore-post sema)]
    [(and (*demo-output*) (directory-exists? (build-path (*demo-output*) path)))
     (semaphore-post sema)]
    [else
     (wrapper-run-herbie info job-id)
     (hash-remove! *job-status* job-id)
     (semaphore-post sema)])
  (hash-remove! *job-sema* job-id))

(define (wrapper-run-herbie cmd job-id)
  (print-job-message (herbie-command-command cmd) job-id (test-name (herbie-command-test cmd)))
  (define result
    (run-herbie (herbie-command-command cmd)
                (herbie-command-test cmd)
                #:seed (herbie-command-seed cmd)
                #:pcontext (herbie-command-pcontext cmd)
                #:profile? (herbie-command-profile? cmd)
                #:timeline-disabled? (herbie-command-timeline-disabled? cmd)))
  (hash-set! *completed-jobs* job-id result)
  (eprintf "Job ~a complete\n" job-id)
  result)

(define (print-job-message command job-id job-str)
  (define job-label
    (match command
      ['alternatives "Alternatives"]
      ['evaluate "Evaluation"]
      ['cost "Computing"]
      ['errors "Analyze"]
      ['exacts "Ground truth"]
      ['improve "Improve"]
      ['local-error "Local error"]
      ['sample "Sampling"]
      [_ (error 'compute-result "unknown command ~a" command)]))
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label (symbol->string command) job-id job-str))