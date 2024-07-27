#lang racket

(require openssl/sha1)
(require (only-in xml write-xexpr)
         json)

(require "sandbox.rkt"
         "../config.rkt"
         "../core/preprocess.rkt"
         "../core/points.rkt"
         "../reports/history.rkt"
         "../reports/plot.rkt"
         "../reports/common.rkt"
         "../syntax/types.rkt"
         "../syntax/read.rkt"
         "../syntax/sugar.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/platform.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/float.rkt")
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

(define (completed-job? job-id)
  (define-values (a b) (place-channel))
  (place-channel-put receptionist (list 'check job-id b))
  (when verbose
    (eprintf "Checking if job ~a is completed.\n" job-id))
  (place-channel-get a))

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
  (when verbose
    (eprintf "Checking current job count\n"))
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
  (when verbose
    (eprintf "Job ~a, Qed up for program: ~a\n" job-id (test-name (herbie-command-test command))))
  job-id)

(define (is-job-finished job-id)
  (hash-ref *job-status* job-id #f))

; verbose logging for debugging
(define verbose #t) ; Maybe change to log-level and use 'verbose?
(define (log msg)
  (when verbose
    ;; TODO fix string interpolation
    (eprintf "~a\n" msg)))

(define (wait-for-job job-id)
  (define-values (a b) (place-channel))
  (place-channel-put receptionist (list 'wait job-id b))
  (define finished-result (place-channel-get a))
  (when verbose
    (eprintf "Done waiting for: ~a\n" job-id))
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
   (when verbose
     (eprintf "Receptionist waiting for work.\n"))
   (for ([i (in-naturals)])
     ;  (eprintf "Receptionist msg ~a handled\n" i)
     (match (place-channel-get ch)
       [(list 'count handler) (place-channel-put handler (hash-count workers))]
       [(list 'start self command job-id)
        (if (hash-has-key? completed-work job-id)
            (place-channel-put self (list 'finished job-id (hash-ref completed-work job-id)))
            (let ([worker (make-worker job-id)])
              (hash-set! workers job-id worker)
              (when verbose
                (eprintf "Starting worker [~a] on [~a].\n"
                         job-id
                         (test-name (herbie-command-test command))))
              (place-channel-put worker (list 'apply self command job-id))))]
       [(list 'finished job-id result)
        (when verbose
          (eprintf "Job ~a finished, saving result.\n" job-id))
        ; Notifed job has been completed, save the result.
        ; let GC collect worker 🤞.
        (hash-set! completed-work job-id result)
        (hash-remove! workers job-id)
        (define maybe-handler (hash-ref waiting job-id #f))
        (when maybe-handler
          (when verbose
            (eprintf "waiting job ~a completed\n" job-id))
          (place-channel-put maybe-handler result)
          (hash-remove! waiting job-id))]
       ; check if work is completed.
       [(list 'check job-id handler) (place-channel-put handler (hash-ref completed-work job-id #f))]
       [(list 'wait job-id handler)
        ; BUG, Hmm I can dead lock this if I fire 10 of the same jobs pretty quickly.
        ; I don't think I dead lock I just don't think I'm caching right.
        (define result (hash-ref completed-work job-id #f))
        (when verbose
          (eprintf "Waiting for job: ~a\n" job-id))
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
        (when verbose
          (eprintf "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command))))
        (define herbie-result (wrapper-run-herbie command job-id))
        (match-define (job-result kind test status time _ _ backend) herbie-result)
        (define out-result
          (match kind
            ['alternatives (make-alternatives-result herbie-result test job-id)]
            ['evaluate (make-calculate-result herbie-result job-id)]
            ['cost (make-cost-result herbie-result job-id)]
            ['errors (make-error-result herbie-result job-id)]
            ['exacts (make-exacts-result herbie-result job-id)]
            ['improve (make-improve-result herbie-result test job-id)]
            ['local-error (make-local-error-result herbie-result test job-id)]
            ['sample (make-sample-result herbie-result test job-id)]
            [_ (error 'compute-result "unknown command ~a" kind)]))
        (when verbose
          (eprintf "Job: ~a finished, returning work to receptionist\n" job-id))
        (place-channel-put receptionist (list 'finished job-id out-result))]))))

#| End Job Server Public API section |#

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:prefab)

(define (make-local-error-result herbie-result test job-id)
  (define expr (prog->fpcore (test-input test)))
  (define local-error (job-result-backend herbie-result))
  ;; TODO: potentially unsafe if resugaring changes the AST
  (define tree
    (let loop ([expr expr] [err local-error])
      (match expr
        [(list op args ...)
         ;; err => (List (listof Integer) List ...)
         (hasheq 'e
                 (~a op)
                 'avg-error
                 (format-bits (errors-score (first err)))
                 'children
                 (map loop args (rest err)))]
        ;; err => (List (listof Integer))
        [_ (hasheq 'e (~a expr) 'avg-error (format-bits (errors-score (first err))) 'children '())])))
  (hasheq 'command (get-command herbie-result) 'tree tree 'job job-id 'path (make-path job-id)))

(define (make-sample-result herbie-result test job-id)
  (define pctx (job-result-backend herbie-result))
  (define repr (context-repr (test-context test)))
  (hasheq 'command
          (get-command herbie-result)
          'points
          (pcontext->json pctx repr)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-calculate-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'points
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-cost-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'cost
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-error-result herbie-result job-id)
  (define errs
    (for/list ([pt&err (job-result-backend herbie-result)])
      (define pt (first pt&err))
      (define err (second pt&err))
      (list pt (format-bits (ulps->bits err)))))
  (hasheq 'command (get-command herbie-result) 'points errs 'job job-id 'path (make-path job-id)))

(define (make-exacts-result herbie-result job-id)
  (hasheq 'command
          (get-command herbie-result)
          'points
          (job-result-backend herbie-result)
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-improve-result herbie-result test job-id)
  (define ctx (ctx-hash-table (test-context test)))
  (define backend (job-result-backend herbie-result))
  (define job-time (job-result-time herbie-result))
  (define warnings (job-result-warnings herbie-result))
  (define timeline (job-result-timeline herbie-result))

  (define repr (test-output-repr test))

  (define backend-hash (backend-improve-result-hash-table backend repr test))
  (hasheq 'command
          (get-command herbie-result)
          'status
          (job-result-status herbie-result)
          'test
          test
          'ctx
          ctx
          'time
          job-time
          'warnings
          warnings
          'timeline
          timeline
          'backend
          backend-hash
          'job
          job-id
          'path
          (make-path job-id)))

(define (backend-improve-result-hash-table backend repr test)
  (define pcontext (improve-result-pctxs backend))

  (define preprocessing (improve-result-preprocess backend))
  (define end-hash-table (end-hash (improve-result-end backend) repr preprocessing pcontext test))

  (hasheq 'preprocessing
          preprocessing
          'pctxs
          pcontext
          'start
          (improve-result-start backend)
          'target
          (improve-result-target backend)
          'end
          end-hash-table
          'bogosity
          (improve-result-bogosity backend)))

(define (end-hash end repr preprocessing pcontexts test)
  (define ctx (test-context test))
  (define-values (processed test-pctx)
    (for/lists (l1 l2)
               ([pctx pcontexts])
               (define-values (train-pcontext test-pcontext) (partition-pcontext pctx))
               (values (preprocess-pcontext ctx test-pcontext preprocessing) test-pcontext)))
  (define-values (end-alts train-errors end-errors end-costs)
    (for/lists (l1 l2 l3 l4)
               ([analysis end])
               (match-define (alt-analysis alt train-errors test-errs) analysis)
               (values alt train-errors test-errs (alt-cost alt repr))))
  (define fpcores
    (for/list ([altn end-alts])
      (~a (program->fpcore (alt-expr altn) (test-context test)))))
  (define alts-histories
    (for/list ([alt end-alts] [ppctx processed] [tpctx test-pctx])
      (render-history alt ppctx tpctx (test-context test))))

  (define vars (test-vars test))
  (define end-alt (alt-analysis-alt (car end)))
  (define splitpoints
    (for/list ([var vars])
      (define split-var? (equal? var (regime-var end-alt)))
      (if split-var?
          (for/list ([val (regime-splitpoints end-alt)])
            (real->ordinal (repr->real val repr) repr))
          '())))

  (hasheq 'end-alts
          fpcores
          'end-histories
          alts-histories
          'end-train-scores
          train-errors
          'end-errors
          end-errors
          'end-costs
          end-costs
          'splitpoints
          splitpoints))

(define (ctx-hash-table ctx)
  (hasheq 'vars (context-vars ctx) 'repr (repr-hash-table (context-repr ctx))))

(define (repr-hash-table repr)
  (hasheq 'name (representation-name repr) 'type (representation-type repr)))

(define (make-alternatives-result herbie-result test job-id)

  (define vars (test-vars test))
  (define repr (test-output-repr test))

  (match-define (list altns test-pcontext processed-pcontext) (job-result-backend herbie-result))
  (define splitpoints
    (for/list ([alt altns])
      (for/list ([var vars])
        (define split-var? (equal? var (regime-var alt)))
        (if split-var?
            (for/list ([val (regime-splitpoints alt)])
              (real->ordinal (repr->real val repr) repr))
            '()))))

  (define fpcores
    (for/list ([altn altns])
      (~a (program->fpcore (alt-expr altn) (test-context test)))))

  (define histories
    (for/list ([altn altns])
      (let ([os (open-output-string)])
        (parameterize ([current-output-port os])
          (write-xexpr
           `(div ([id "history"])
                 (ol ,@(render-history altn processed-pcontext test-pcontext (test-context test)))))
          (get-output-string os)))))
  (define derivations
    (for/list ([altn altns])
      (render-json altn processed-pcontext test-pcontext (test-context test))))
  (hasheq 'command
          (get-command herbie-result)
          'alternatives
          fpcores
          'histories
          histories
          'derivations
          derivations
          'splitpoints
          splitpoints
          'job
          job-id
          'path
          (make-path job-id)))

(define (get-command herbie-result)
  ; force symbol type to string
  (~s (job-result-command herbie-result)))

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
  (eprintf "Herbie completed job: ~a\n" job-id)
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
