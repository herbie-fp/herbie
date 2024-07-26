#lang racket

(require openssl/sha1)

(require "sandbox.rkt"
         "../config.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/read.rkt")
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
  (for/list ([(k v) (in-hash *completed-jobs*)]
             #:when (equal? (job-result-command v) 'improve))
    (get-table-data v (make-path k))))

(define (job-count)
  (hash-count *job-status*))

(define (is-server-up)
  (thread-running? *worker-thread*))

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

(define *job-queue* (list))

;; Starts a job for a given command object|
(define (start-job command)
  (set! *job-queue* (append *job-queue* (list command)))
  (eprintf "~a added Q:~a\n" command (length *job-queue*))
  (check-q-for-work)
  (define job-id (compute-job-id command))
  (if (already-computed? job-id) job-id (start-work command)))

(define (check-q-for-work)
  (if (and (> (length *job-queue*) 0) (> (length *idel-workers*) 0))
      (assign-work)
      (eprintf "~a <= 0\n" (length *job-queue*))))

;; To be called when the Q is not empty
(define (assign-work)
  (eprintf "assigning work: from ~a jobs\n" (length *job-queue*))
  (define job (first *job-queue*))
  (set! *job-queue* (rest *job-queue*))
  (define job-id (compute-job-id job))
  (define worker (first *idel-workers*))
  (set! *idel-workers* (rest *idel-workers*))
  (place-channel-put worker (list 'apply worker job job-id))
  (update-worker-queues)
  (eprintf "works updated\n"))

(define (update-worker-queues)
  (let loop ([out '()])
    (with-handlers ([exn:break? (λ (_)
                                  (eprintf "Terminating after ~a problem~a!\n"
                                           (length out)
                                           (if (= (length out) 1) "" "s"))
                                  out)])
      (match (apply sync (append *busy-workers* *dead-events*))
        [(list 'ready self) (set! *idel-workers* (append *idel-workers* (list self)))]
        [(list 'done self result)
         (hash-set! *completed-jobs* result)
         (set! *idel-workers* (append *idel-workers* (list self)))]
        ; In this case it is a place-dead-event
        [(? evt?) (error "Thread crashed. Unrecoverable. Terminating immediately.")])))
      (eprintf "HERE!\n"))

(define (is-job-finished job-id)
  (hash-ref *job-status* job-id #f))

(define (wait-for-job job-id)
  (if (already-computed? job-id) (hash-ref *completed-jobs* job-id) (internal-wait-for-job job-id)))

(define (start-job-server config global-demo global-output)
  (build-worker-pool (processor-count))
  (check-q-for-work)
  ;; Pass along local global values
  ;; TODO can I pull these out of config or not need ot pass them along.
  (set! *demo?* global-demo)
  (set! *demo-output* global-output)
  (thread-send *worker-thread* config))

(define *busy-workers* (list))
(define *idel-workers* (list))
(define *dead-events* (list)) ;; don't fully understand this.
(define (build-worker-pool number-of-workers)
  (define workers
    (for/list ([wid (in-range number-of-workers)])
      (make-worker wid)))
  (define dead-workers
    (for/list ([worker workers])
      (place-dead-evt worker)))
  (set! *busy-workers* workers)
  (set! *dead-events* dead-workers)
  (for ([worker *busy-workers*])
    (place-channel-put worker (list 'start worker)))
  (eprintf "workers ready\n")
  (update-worker-queues))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))
(define (make-worker wid)
  (place/context* ch
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
                       (eprintf "[~a] working on [~a].\n" wid id)
                       ; (define herbie-result (wrapper-run-herbie command id))
                       ; (match-define (job-result kind test status time _ _ backend) herbie-result)
                       ; (define out-result
                       ;   (match kind
                       ;     ['alternatives (make-alternatives-result herbie-result test id)]
                       ;     ['evaluate (make-calculate-result herbie-result id)]
                       ;     ['cost (make-cost-result herbie-result id)]
                       ;     ['errors (make-error-result herbie-result id)]
                       ;     ['exacts (make-exacts-result herbie-result id)]
                       ;     ['improve (make-improve-result herbie-result test id)]
                       ;     ['local-error (make-local-error-result herbie-result test id)]
                       ;     ['sample (make-sample-result herbie-result id test)]
                       ;     [_ (error 'compute-result "unknown command ~a" kind)]))
                       ; (hash-set! *job-status* id #f)
                       ; (place-channel-put ch (list 'done out-result))]
                       (eprintf "~a working on job:~a\n" wid id)
                       (define result id) ;; finnish work
                       (place-channel-put self ('done self result))]
                      [(list 'start self) (place-channel-put self (list 'ready self))]
                      [(list 'ready self) (place-channel-put self (list 'ready self))]))))

#| End Job Server Public API section |#

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:transparent)

; Private globals
; TODO I'm sure these can encapslated some how.
(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))
(define *completed-jobs* (make-hash))
(define *job-status* (make-hash))
(define *job-sema* (make-hash))

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
  (hash-set! *job-status* job-id (*timeline*))
  (define sema (make-semaphore))
  (hash-set! *job-sema* job-id sema)
  (thread-send *worker-thread* (work job-id job sema))
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
      ['explanations "Explanations"]
      ['sample "Sampling"]
      [_ (error 'compute-result "unknown command ~a" command)]))
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label (symbol->string command) job-id job-str))

(define *worker-thread*
  (thread (λ ()
            (let loop ([seed #f])
              (match (thread-receive)
                [`(init rand
                        ,vec
                        flags
                        ,flag-table
                        num-iters
                        ,iterations
                        points
                        ,points
                        timeout
                        ,timeout
                        output-dir
                        ,output
                        reeval
                        ,reeval
                        demo?
                        ,demo?)
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
