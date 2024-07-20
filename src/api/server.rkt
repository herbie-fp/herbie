#lang racket

(require openssl/sha1)

(require "sandbox.rkt"
         "../config.rkt"
         "../syntax/read.rkt"
         "../syntax/types.rkt"
         "../syntax/load-plugin.rkt"
         "../utils/alternative.rkt"
         "../reports/history.rkt"
         "../core/preprocess.rkt")
(require (submod "../utils/timeline.rkt" debug))
(require racket/place)
(require json)

(provide completed-job? get-results-for job-count
  is-server-up create-job start-job is-job-finished wait-for-job
  start-job-server)

#| Job Server Public API section |#

; Helers to isolated *completed-jobs*
(define (completed-job? id) 
  (hash-has-key? *completed-jobs* id))

; Returns #f is now job exsist for the given job-id
(define (get-results-for id) 
  (hash-ref *completed-jobs* id #f))

(define (job-count)
  (hash-count *job-status*))

; Not sure this makes sense anymore.
(define (is-server-up) #t)

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
  (define job-id (compute-job-id command))
  (if (already-computed? job-id) job-id (work-on command)))

;; Used by check-status to poll the current status of a job.
(define (is-job-finished job-id)
  (define maybe-channel (hash-ref *inprogress* job-id #f))
  ;; TODO handle #f case for maybe-channel
  (if (equal? maybe-channel #f)
    #f
    (get-job-data maybe-channel job-id)))

(define (get-job-data maybe-channel job-id)
  (eprintf "sending check\n")
  (place-channel-put maybe-channel (list 'check job-id))
  (match (place-channel-get maybe-channel)
  [(list 'done data) 
    (hash-remove! *inprogress* job-id)
    (hash-set! *completed-jobs* job-id data)
    data]
  [(list 'inprogress data) data]))

(define (wait-for-job job-id)
  ; (if (already-computed? job-id) 
  (eprintf "waiting for job: ~a\n" job-id)
  (define job-channel (hash-ref *inprogress* job-id))
  (define result (place-channel-get job-channel))
  (hash-set! *completed-jobs* job-id result)
  result)

(define (start-job-server config global-demo global-output)
  ;; Pass along local global values
  ;; TODO can I pull these out of config or not need ot pass them along.
  (build-worker-pool 4)
  (set! *demo?* global-demo)
  (set! *demo-output* global-output))

#| End Job Server Public API section |#

;; Job object, What herbie excepts as input for a new job.
(struct herbie-command 
  (command test seed pcontext profile? timeline-disabled?) #:prefab)
 
; Private globals
; TODO I'm sure these can encapslated some how.
(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))
(define *completed-jobs* (make-hash))
(define *job-status* (make-hash))

;; place channels in progress. "workers"
(define *inprogress* (make-hash))

(define (already-computed? job-id)
 (or (hash-has-key? *completed-jobs* job-id)
     (and (*demo-output*)
          (directory-exists? (build-path (*demo-output*) 
          (format "~a.~a" job-id *herbie-commit*))))))

(define (compute-job-id job-info)
  (sha1 (open-input-string (~s job-info))))

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
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label 
    (symbol->string command) job-id job-str))

(define *ready-workers* (list))

(define (work-on command)
  (define job-id (compute-job-id command))
  ;; Pop off 1st ready worker
  (define worker (first *ready-workers*))
  (set! *ready-workers* (cdr *ready-workers*))
  ;; Send work to worker
  (place-channel-put worker `(apply ,worker ,command, job-id))
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

(define (make-worker)
  (place/context* ch
    #:parameters (*flags* *num-iterations* *num-points* *timeout* *reeval-pts* *node-limit*
                  *max-find-range-depth* *pareto-mode* *platform-name* *loose-plugins*)
    (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
      (load-herbie-plugins))
    (for ([_ (in-naturals)])
      (match (place-channel-get ch)
        [(list 'apply self command id) 
          (hash-set! *job-status* id (*timeline*))
          (eprintf "Job [~a] being worked on.\n" id)
          (define herbie-result (wrapper-run-herbie command id))
          (match-define 
            (job-result kind test status time _ _ backend) herbie-result)
          (define out-result
            (match kind 
              ['alternatives #f]
              ['evaluate #f]
              ['cost #f]
              ['errors #f]
              ['exacts #f]
              ['improve (improve-result-hash herbie-result)]
              ['local-error #f]
              ['sample #f]
              [_ (error 'compute-result "unknown command ~a" kind)]))
          (hash-set! *job-status* id #f)
          (place-channel-put ch (list 'done out-result))]
       [(list 'check id) 
        (eprintf "checking ~a\n" id)
        (place-channel-put ch 
          (list 'done (list 'inprogress (hash-ref *job-status* id #f))))]))))

(define (improve-result-hash result)
  (define test (job-result-test result))
  (define ctx (ctx-hash-table (test-context test)))
  (define backend (job-result-backend result))
  (define job-time (job-result-time result))
  (define warnings (job-result-warnings result))
  (define timeline (job-result-timeline result))

  (define repr (test-output-repr test)) 
  
  (define backend-hash (backend-improve-result-hash-table backend repr test))

  (hasheq 'status (job-result-status result)
          'test test
          'ctx ctx
          'time job-time
          'warnings warnings
          'timeline timeline
          'backend backend-hash))

(define (backend-improve-result-hash-table backend repr test)
  (define pcontext (improve-result-pctxs backend))
  (define preprocessing (improve-result-preprocess backend))
  (define end-hash-table (end-hash (improve-result-end backend) repr preprocessing pcontext test))

  (hasheq 'preprocessing preprocessing
          'pctxs pcontext
          'start (improve-result-start backend)
          'target (improve-result-target backend)
          'end end-hash-table
          'bogosity (improve-result-bogosity backend)))

(define (end-hash end repr preprocessing pcontexts test)
  (define-values (processed test-pctx)
    (for/lists (l1 l2) ([pctx pcontexts])
    (define-values (train-pcontext test-pcontext) (partition-pcontext pctx))
    (values 
      (preprocess-pcontext (*context*) test-pcontext preprocessing) test-pcontext)))
  (define-values (end-alts end-errors end-costs)
    (for/lists (l1 l2 l3) ([analysis end])
      (match-define (alt-analysis alt _ test-errs) analysis)
      (values alt test-errs (alt-cost alt repr))))
  (define sendable-alts 
    (for/list ([alt end-alts] [ppctx processed] [tpctx test-pctx])
      (render-json alt ppctx tpctx (test-context test))))
  (define alts-histories 
    (for/list ([alt end-alts] [ppctx processed] [tpctx test-pctx])
      (render-history alt ppctx tpctx (test-context test))))
  (hasheq 'end-alts sendable-alts
          'end-histories alts-histories
          'end-errors end-errors
          'end-costs end-costs))

(define (ctx-hash-table ctx)
  (hasheq 'vars (context-vars ctx)
          'repr (repr-hash-table (context-repr ctx))))

(define (repr-hash-table repr)
  (hasheq 'name (representation-name repr)
          'type (representation-type repr)))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name (parameterize ([params fresh] ...) body ...))))]))