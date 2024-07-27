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
(define (completed-job? job-id)
  (define-values (a b) (place-channel))
  (place-channel-put receptionist (list 'check job-id b))
  (eprintf "Checking current job count\n")
  (define count (place-channel-get a))
  count)

; Returns #f is now job exsist for the given job-id
(define (get-results-for id)
  (hash-ref *completed-jobs* id #f))

; I don't like how specific this function is but it keeps the API boundary.
(define (get-improve-job-data)
  (for/list ([(k v) (in-hash *completed-jobs*)]
             #:when (equal? (job-result-command v) 'improve))
    (get-table-data v (make-path k))))

(define (job-count)
  (define-values (a b) (place-channel))
  (place-channel-put receptionist (list 'count b))
  (eprintf "Checking current job count\n")
  (define count (place-channel-get a))
  count)

(define (is-server-up)
  (place? receptionist))

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

(define (start-job-server config global-demo global-output)
  (set! receptionist (make-receptionist)))

;; Starts a job for a given command object|
(define (start-job command)
  (define job-id (compute-job-id command))
  (place-channel-put receptionist (list 'start receptionist command job-id))
  (eprintf "Job ~a, Qed up for program: ~a\n" job-id (test-name (herbie-command-test command)))
  job-id)

(define (is-job-finished job-id)
  (hash-ref *job-status* job-id #f))

(define (wait-for-job job-id)
  (define-values (a b) (place-channel))
  (place-channel-put receptionist (list 'wait job-id b))
  (define finished-result (place-channel-get a))
  (eprintf "Done waiting for: ~a\n" job-id)
  finished-result)

(define (make-receptionist)
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
   ; not sure if the above code is actaully needed.
   (define completed-work (make-hash))
   (define workers (make-hash))
   (define waiting (make-hash))
   (eprintf "Receptionist waiting for work.\n")
   (for ([i (in-naturals)])
     ;  (eprintf "Receptionist msg ~a handled\n" i)
     (match (place-channel-get ch)
       [(list 'count handler) (place-channel-put handler (hash-count workers))]
       [(list 'start self command job-id)
        (if (hash-has-key? completed-work job-id)
            (hash-ref completed-work job-id)
            (let ([worker (make-worker job-id)])
              (hash-set! workers job-id worker)
              (eprintf "Starting worker [~a] on [~a].\n"
                       job-id
                       (test-name (herbie-command-test command)))
              (place-channel-put worker (list 'apply self command job-id))))]
       [(list 'finished job-id result)
        (eprintf "Job ~a finished, saving result.\n" job-id)
        ; Notifed job has been completed, save the result.
        ; let GC collect worker 🤞.
        (hash-set! completed-work job-id result)
        (hash-remove! workers job-id)
        (define maybe-waiting (hash-ref waiting job-id #t))
        (when maybe-waiting
          (eprintf "waiting job ~a completed\n" job-id)
          (place-channel-put maybe-waiting result))]
       ; check if work is completed.
       [(list 'check job-id handler) (place-channel-put handler (hash-ref completed-work job-id #f))]
       [(list 'wait job-id handler)
        ; BUG, Hmm I can dead lock this if I fire 10 of the same jobs pretty quickly.
        (define result (hash-ref completed-work job-id #f))
        (eprintf "waiting for request for ~a: ~a\n" job-id result)
        (if (false? result) (hash-set! waiting job-id handler) result)]))))

(define receptionist #f)

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))

(define (make-worker job-id)
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
       [(list 'apply receptionist command job-id)
        (eprintf "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command)))
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
        (define result job-id) ;; finnish work
        (eprintf "Job: ~a finished, returning work to receptionist\n" job-id)
        (place-channel-put receptionist (list 'finished job-id result))]))))

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
