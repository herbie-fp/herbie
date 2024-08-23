#lang racket

(require openssl/sha1)
(require (only-in xml write-xexpr)
         json)

(require "sandbox.rkt"
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
         "../utils/errors.rkt"
         "../utils/float.rkt")

(provide make-path
         get-improve-table-data
         make-improve-result
         get-results-for
         job-count
         is-server-up
         create-job
         start-job
         wait-for-job
         start-job-server)

; verbose logging for debugging
(define verbose #f) ; Maybe change to log-level and use 'verbose?
(define (log msg . args)
  (when verbose
    (apply eprintf msg args)))

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command (command test seed pcontext profile? timeline-disabled?) #:prefab)

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command
                    test
                    #:seed [seed #f]
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command test seed pcontext profile? timeline-disabled?))

; computes the path used for server URLs
(define (make-path id)
  (format "~a.~a" id *herbie-commit*))

; Returns #f is now job exsist for the given job-id
(define (get-results-for job-id)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list 'result job-id b))
  (log "Getting result for job: ~a.\n" job-id)
  (place-channel-get a))

(define (get-improve-table-data)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list 'improve b))
  (log "Getting improve results.\n")
  (place-channel-get a))

(define (job-count)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list 'count b))
  (define count (place-channel-get a))
  (log "Current job count: ~a.\n" count)
  count)

;; Starts a job for a given command object|
(define (start-job command)
  (define job-id (compute-job-id command))
  (place-channel-put manager (list 'start manager command job-id))
  (log "Job ~a, Qed up for program: ~a\n" job-id (test-name (herbie-command-test command)))
  job-id)

(define (wait-for-job job-id)
  (define-values (a b) (place-channel))
  (place-channel-put manager (list 'wait manager job-id b))
  (define finished-result (place-channel-get a))
  (log "Done waiting for: ~a\n" job-id)
  finished-result)

; TODO refactor using this helper.
(define (manager-ask msg . args)
  (define-values (a b) (place-channel))
  (place-channel-put manager (cons msg b args))
  (log "Asking manager: ~a, ~a.\n" msg args)
  (place-channel-get a))

(define (is-server-up)
  (not (sync/timeout 0 manager-dead-event)))

(define (start-job-server job-cap)
  (define r (make-manager job-cap))
  (set! manager-dead-event (place-dead-evt r))
  (set! manager r))

(define manager #f)
(define manager-dead-event #f)

(define (get-command herbie-result)
  ; force symbol type to string.
  ; This is a HACK to fix JSON parsing errors that may or may not still happen.
  (~s (job-result-command herbie-result)))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

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

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name
                          (parameterize ([params fresh] ...)
                            body ...))))]))

(struct work-item (command id))

(define (make-manager worker-count)
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
   (define busy-workers (make-hash))
   (define waiting-workers (make-hash))
   (for ([i (in-range worker-count)])
     (hash-set! waiting-workers i (make-worker i)))
   (log "~a workers ready.\n" (hash-count waiting-workers))
   (define waiting (make-hash))
   (define job-queue (list))
   (log "Manager waiting to assign work.\n")
   (for ([i (in-naturals)])
     ;  (eprintf "manager msg ~a handled\n" i)
     (match (place-channel-get ch)
       [(list 'start self command job-id)
        ; Check if the work has been completed already if not assign the work.
        (if (hash-has-key? completed-work job-id)
            (place-channel-put self (list 'send job-id (hash-ref completed-work job-id)))
            (place-channel-put self (list 'queue self job-id command)))]
       [(list 'queue self job-id command)
        (set! job-queue (append job-queue (list (work-item command job-id))))
        (place-channel-put self (list 'assign self))]
       [(list 'assign self)
        (define reassigned (make-hash))
        (for ([(wid worker) (in-hash waiting-workers)]
              [job (in-list job-queue)])
          (log "Starting worker [~a] on [~a].\n"
               (work-item-id job)
               (test-name (herbie-command-test (work-item-command job))))
          (place-channel-put worker (list 'apply self (work-item-command job) (work-item-id job)))
          (hash-set! reassigned wid worker)
          (hash-set! busy-workers wid worker))
        ; remove X many jobs from the Q and update waiting-workers
        (for ([(wid worker) (in-hash reassigned)])
          (hash-remove! waiting-workers wid)
          (set! job-queue (cdr job-queue)))]
       ; Job is finished save work and free worker. Move work to 'send state.
       [(list 'finished self wid job-id result)
        (log "Job ~a finished, saving result.\n" job-id)
        (hash-set! completed-work job-id result)

        ; move worker to waiting list
        (hash-set! waiting-workers wid (hash-ref busy-workers wid))
        (hash-remove! busy-workers wid)

        (log "waiting job ~a completed\n" job-id)
        (place-channel-put self (list 'send job-id result))
        (place-channel-put self (list 'assign self))]
       [(list 'wait self job-id handler)
        (log "Waiting for job: ~a\n" job-id)
        ; first we add the handler to the wait list.
        (hash-update! waiting job-id (curry append (list handler)) '())
        (define result (hash-ref completed-work job-id #f))
        ; check if the job is completed or not.
        (unless (false? result)
          (log "Done waiting for job: ~a\n" job-id)
          ; we have a result to send.
          (place-channel-put self (list 'send job-id result)))]
       [(list 'send job-id result)
        (log "Sending result for ~a.\n" job-id)
        (for ([handle (hash-ref waiting job-id '())])
          (place-channel-put handle result))
        (hash-remove! waiting job-id)]
       ; Get the result for the given id, return false if no work found.
       [(list 'result job-id handler) (place-channel-put handler (hash-ref completed-work job-id #f))]
       ; Returns the current count of working workers.
       [(list 'count handler) (place-channel-put handler (hash-count busy-workers))]
       ; Retreive the improve results for results.json
       [(list 'improve handler)
        (define improved-list
          (for/list ([(job-id result) (in-hash completed-work)]
                     #:when (equal? (hash-ref result 'command) "improve"))
            (get-table-data-from-hash result (make-path job-id))))
        (place-channel-put handler improved-list)]))))

(define (make-worker worker-id)
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
       [(list 'apply manager command job-id)
        (log "[~a] working on [~a].\n" job-id (test-name (herbie-command-test command)))
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
            ['explanations (make-explanation-result herbie-result job-id)]
            ['sample (make-sample-result herbie-result test job-id)]
            [_ (error 'compute-result "unknown command ~a" kind)]))
        (log "Job: ~a finished, returning work to manager\n" job-id)
        (place-channel-put manager (list 'finished manager worker-id job-id out-result))]))))

(define (make-explanation-result herbie-result job-id)
  (define explanations (job-result-backend herbie-result))
  (hasheq 'command
          (get-command herbie-result)
          'explanation
          explanations
          'job
          job-id
          'path
          (make-path job-id)))

(define (make-local-error-result herbie-result test job-id)
  (define expr (prog->fpcore (test-input test)))
  (define local-error (job-result-backend herbie-result))
  ;; TODO: potentially unsafe if resugaring changes the AST
  (define tree
    (let loop ([expr expr]
               [err local-error])
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
  (define ctx (context->json (test-context test)))
  (define backend (job-result-backend herbie-result))
  (define job-time (job-result-time herbie-result))
  (define warnings (job-result-warnings herbie-result))
  (define timeline (job-result-timeline herbie-result))

  (define repr (test-output-repr test))
  (define backend-hash
    (match (job-result-status herbie-result)
      ['success (backend-improve-result-hash-table backend repr test)]
      ['timeout #f]
      ['failure (exception->datum backend)]))

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
  (define end-hash-table (end-hash (improve-result-end backend) repr pcontext test))

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

(define (end-hash end repr pcontexts test)

  ; analysis of output expressions
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (define-values (end-exprs-c end-train-scores-c end-test-scores-c end-costs-c)
    (for/lists (l1 l2 l3 l4)
               ([result end])
               (match-define (alt-analysis alt train-errors test-errors) result)
               (values (alt-expr alt)
                       (errors-score train-errors)
                       (errors-score test-errors)
                       (expr-cost (alt-expr alt) repr))))

  (define-values (end-alts train-errors end-errors end-costs)
    (for/lists (l1 l2 l3 l4)
               ([analysis end])
               (match-define (alt-analysis alt train-errors test-errs) analysis)
               (values alt train-errors test-errs (alt-cost alt repr))))

  (define alts-histories
    (for/list ([alt end-alts])
      (render-history alt (first pcontexts) (second pcontexts) (test-context test))))
  (define vars (test-vars test))
  (define end-alt (alt-analysis-alt (car end)))
  (define splitpoints
    (for/list ([var vars])
      (define split-var? (equal? var (regime-var end-alt)))
      (if split-var?
          (for/list ([val (regime-splitpoints end-alt)])
            (real->ordinal (repr->real val repr) repr))
          '())))

  (hasheq 'end-alts ; wrong
          end-exprs-c
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

(define (context->json ctx)
  (hasheq 'vars (context-vars ctx) 'repr (repr->json (context-repr ctx))))

(define (repr->json repr)
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
