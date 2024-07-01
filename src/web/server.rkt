#lang racket

(require openssl/sha1)

(require "../sandbox.rkt" "../syntax/read.rkt" 
 "../load-plugin.rkt" "../common.rkt" "pages.rkt" "../datafile.rkt")
(require (submod "../timeline.rkt" debug))
(require racket/place)

(provide completed-job? get-results-for get-improve-job-data job-count
  is-server-up create-job start-job is-job-finished wait-for-job
  start-job-server)

#| Job Server Public API section |#

; Helers to isolated *completed-jobs*
(define (completed-job? id) 
  (hash-has-key? *completed-jobs* id))

; Returns #f is now job exsist for the given job-id
(define (get-results-for id) 
  (hash-ref *completed-jobs* id #f))

; I don't like how specific this function is but it keeps the API boundary.
(define (get-improve-job-data)
  (for/list ([(k v) (in-hash *completed-jobs*)]
    #:when (equal? (job-result-command v) 'improve))
    (get-table-data v (format "~a.~a" k *herbie-commit*))))

(define (job-count)
  (hash-count *job-status*))

(define (is-server-up)
  (thread-running? *worker-thread*))

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command test 
                    #:seed [seed #f] 
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

;; Starts a job for a given command object
(define (start-job command)
;  (work-on command)
 (define job-id (compute-job-id command))
 (if (already-computed? job-id) job-id (start-work command)))

(define (is-job-finished job-id)
  (hash-ref *job-status* job-id #f))

(define (wait-for-job job-id)
  (if (already-computed? job-id) 
      (hash-ref *completed-jobs* job-id) 
      (internal-wait-for-job job-id)))

(define (start-job-server config global-demo global-output)
 ;; Pass along local global values
 ;; TODO can I pull these out of config or not need ot pass them along.
 (build-worker-pool 4)
 (eprintf "workers: ~a\n" *ready-workers*)
 (set! *demo?* global-demo)
 (set! *demo-output* global-output)
 (thread-send *worker-thread* config))

#| End Job Server Public API section |#

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command 
  (command test seed pcontext profile? timeline-disabled?) #:transparent)
 
; Private globals
; TODO I'm sure these can encapslated some how.
(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))
(define *completed-jobs* (make-hash))
(define *job-status* (make-hash))
(define *job-sema* (make-hash))

(define (already-computed? job-id)
 (or (hash-has-key? *completed-jobs* job-id)
     (and (*demo-output*)
          (directory-exists? (build-path (*demo-output*) 
          (format "~a.~a" job-id *herbie-commit*))))))

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
  (hash-set! *job-status* job-id (*timeline*))
  (define sema (make-semaphore))
  (hash-set! *job-sema* job-id sema)
  (thread-send *worker-thread* (work job-id job sema))
  job-id)

; Handles semaphore and async part of a job
(struct work (id job sema))

(define (run-job job-info)
  (match-define (work job-id info sema) job-info)
  (define path (format "~a.~a" job-id *herbie-commit*))
  (cond ;; Check caches if job as already been completed
   [(hash-has-key? *completed-jobs* job-id)
    (semaphore-post sema)]
   [(and (*demo-output*) (directory-exists? (build-path (*demo-output*) path)))
    (semaphore-post sema)]
   [else (wrapper-run-herbie info job-id)
    (hash-remove! *job-status* job-id)
    (semaphore-post sema)])
  (hash-remove! *job-sema* job-id))

(define (wrapper-run-herbie cmd job-id)
  (print-job-message (herbie-command-command cmd) job-id 
    (herbie-command-test cmd))
  (define result (run-herbie 
    (herbie-command-command cmd)
    (herbie-command-test cmd)
    #:seed (herbie-command-seed cmd)
    #:pcontext (herbie-command-pcontext cmd)
    #:profile? (herbie-command-profile? cmd)
    #:timeline-disabled? (herbie-command-timeline-disabled? cmd)))
  (hash-set! *completed-jobs* job-id result)
  (eprintf "Job ~a complete\n" job-id))

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
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label 
    (symbol->string command) job-id job-str))

(define *worker-thread*
 (thread
  (λ ()
   (let loop ([seed #f])
    (match (thread-receive)
     [`(init rand ,vec flags ,flag-table num-iters ,iterations points ,points
        timeout ,timeout output-dir ,output reeval ,reeval demo? ,demo?)
       (set! seed vec)
       (*flags* flag-table)
       (*num-iterations* iterations)
       (*num-points* points)
       (*timeout* timeout)
       (*demo-output* output)
       (*reeval-pts* reeval)
       (*demo?* demo?)]
     [job-info (run-job job-info)])
    (loop seed)))))


(define *ready-workers* (list))
(define *working-workers* (list))

(define (work-on command)
  (eprintf "working on: ~a\n" command)
  ;; Pop off 1st ready worker
  (eprintf "~a\n" (length *ready-workers*))
  (define worker (first *ready-workers*))
  (set! *ready-workers* (cdr *ready-workers*))
  (set! *working-workers* (cons *working-workers* worker))
  ;; Send work to worker
  (place-channel-put worker `(apply ,worker ,command)))

(define (build-worker-pool number-of-workers)
  (define workers
    (for/list ([wid (in-range number-of-workers)])
      (make-worker)))
  (for/list ([worker workers])
    (place-dead-evt worker))
  (set! *ready-workers* workers))

(define (make-worker)
  (place/context* ch
    #:parameters (*flags* *num-iterations* *num-points* *timeout* *reeval-pts* *node-limit*
                  *max-find-range-depth* *pareto-mode* *platform-name* *loose-plugins*)
    (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
      (load-herbie-plugins))
    (for ([_ (in-naturals)])
      (match-define (list 'apply self command) (place-channel-get ch))
      (define result "MAKE WORKER HERE")
      (eprintf "[~a] working on: ~a\n" self command)
      (place-channel-put ch result))))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name (parameterize ([params fresh] ...) body ...))))]))