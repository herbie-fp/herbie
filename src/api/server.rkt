#lang racket

(require openssl/sha1)
(require (only-in xml write-xexpr)
         json)
(require "sandbox.rkt"
         "../config.rkt"
         "../core/preprocess.rkt"
         "../reports/history.rkt"
         "../reports/plot.rkt"
         "../reports/common.rkt"
         "../syntax/types.rkt"
         "../syntax/read.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/platform.rkt"
         "../utils/alternative.rkt"
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
  (if (already-computed? job-id)
      (hash-ref *completed-jobs* job-id #f)
      (internal-wait-for-job job-id)))

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
            ['alternatives (make-alternatives-result herbie-result test id)]
            ['evaluate (make-calculate-result herbie-result id)]
            ['cost (make-cost-result herbie-result id)]
            ['errors #f]
            ['exacts (make-exacts-result herbie-result id)]
            ['improve (make-improve-result herbie-result)]
            ['local-error #f]
            ['sample #f]
            [_ (error 'compute-result "unknown command ~a" kind)]))
        (hash-set! *job-status* id #f)
        (place-channel-put ch (list 'done out-result))]
       [(list 'check id)
        (eprintf "checking ~a\n" id)
        (place-channel-put ch (list 'done (list 'inprogress (hash-ref *job-status* id #f))))]))))

(define (make-exacts-result herbie-result id)
  (hasheq 'points (job-result-backend herbie-result) 'job id 'path (make-path id)))

(define (make-calculate-result herbie-result id)
  (hasheq 'points (job-result-backend herbie-result) 'job id 'path (make-path id)))

(define (make-cost-result herbie-result id)
  (define cost (job-result-backend herbie-result))
  (hasheq 'cost cost 'job id 'path (make-path id)))

(define (make-alternatives-result herbie-result test id)

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
  (hasheq 'alternatives
          fpcores
          'histories
          histories
          'derivations
          derivations
          'splitpoints
          splitpoints
          'job
          id
          'path
          (make-path id)))

(define (make-improve-result result)
  (define test (job-result-test result))
  (define ctx (ctx-hash-table (test-context test)))
  (define backend (job-result-backend result))
  (define job-time (job-result-time result))
  (define warnings (job-result-warnings result))
  (define timeline (job-result-timeline result))

  (define repr (test-output-repr test))

  (define backend-hash (backend-improve-result-hash-table backend repr test))

  (hasheq 'status
          (job-result-status result)
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
          backend-hash))

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
  (define-values (processed test-pctx)
    (for/lists (l1 l2)
               ([pctx pcontexts])
               (define-values (train-pcontext test-pcontext) (partition-pcontext pctx))
               (values (preprocess-pcontext (*context*) test-pcontext preprocessing) test-pcontext)))
  (define-values (end-alts end-errors end-costs)
    (for/lists (l1 l2 l3)
               ([analysis end])
               (match-define (alt-analysis alt _ test-errs) analysis)
               (values alt test-errs (alt-cost alt repr))))
  (define sendable-alts
    (for/list ([alt end-alts] [ppctx processed] [tpctx test-pctx])
      (render-json alt ppctx tpctx (test-context test))))
  (define alt (hash-ref (first sendable-alts) 'program))
  ; (eprintf "alt:~a\n" alt)
  (define syn (read-syntax 'web (open-input-string alt)))
  (define t (parse-test syn))
  ; (eprintf "SEND:~a\n" t)
  (define alts-histories
    (for/list ([alt end-alts] [ppctx processed] [tpctx test-pctx])
      (render-history alt ppctx tpctx (test-context test))))
  (hasheq 'end-alts
          sendable-alts
          'end-histories
          alts-histories
          'end-errors
          end-errors
          'end-costs
          end-costs))

(define (ctx-hash-table ctx)
  (hasheq 'vars (context-vars ctx) 'repr (repr-hash-table (context-repr ctx))))

(define (repr-hash-table repr)
  (hasheq 'name (representation-name repr) 'type (representation-type repr)))

(define (already-computed? job-id)
  (or (hash-has-key? *completed-jobs* job-id)
      (and (*demo-output*) (directory-exists? (build-path (*demo-output*) (make-path job-id))))))

(define (internal-wait-for-job job-id)
  (eprintf "Waiting for job\n")
  (define ch (hash-ref *inprogress* job-id #f))
  (match (place-channel-get ch)
    [(list 'done result) result]
    [else #f]))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

; Encapsulates semaphores and async part of jobs.
(define (start-work job)
  (define job-id (compute-job-id job))
  (work-on job)
  job-id)

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
