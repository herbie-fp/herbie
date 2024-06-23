#lang racket

(require json)
(require racket/exn)
(require openssl/sha1 (rename-in xml [location? xml-location?]))
(require web-server/servlet web-server/servlet-env web-server/dispatch
         web-server/dispatchers/dispatch web-server/dispatch/extend
         web-server/http/bindings web-server/configuration/responders
         web-server/managers/none)

(require "../common.rkt" "../config.rkt" "../syntax/read.rkt" "../errors.rkt")
(require "../syntax/syntax-check.rkt" "../syntax/type-check.rkt" "../syntax/types.rkt"
         "../syntax/sugar.rkt" "../alternative.rkt" "../points.rkt"
         "../programs.rkt" "../sandbox.rkt" "../float.rkt")
(require "../datafile.rkt" "pages.rkt" "make-report.rkt"
         "common.rkt" "core2mathjs.rkt" "history.rkt" "plot.rkt")
(require (submod "../timeline.rkt" debug))


(provide helper helper2 helper3 job-count is-server-up create-job start-job is-job-finished wait-for-job start-job-server)
#| 
Job Server Public API section

This section just servers as a place for us to create the API's but give a
|#

;; Job object, What herbie excepts as input for a new job.

(struct herbie-command 
 (command formula seed pcontext profile? timeline-disabled?) #:transparent)

;; Creates a command object to be passed to start-job server.
;; TODO contract?
(define (create-job command formula 
                    #:seed [seed #f] 
                    #:pcontext [pcontext #f]
                    #:profile? [profile? #f]
                    #:timeline-disabled? [timeline-disabled? #f])
  (herbie-command command formula seed pcontext profile? timeline-disabled?))

;; Starts a job for a given command object|
#|
(define (start-job command)
 (define id (compute-job-id command))
 (unless already-computed? id
  (eprintf "Job ~a not cached" id)
  (start-work command))
 id)
|#
(define (start-job command)
  (start-work command))
#| 
Not ready for this API yet as i'm not sure how syncing with this abstraction will work. I could try using semaphores as the current server does.
|#
(define (wait-for-job job-id)
  #| Where should we store job ids
  How does access control in Racket work?
  |#
  (eprintf "Waiting for job\n")
  (define sema (hash-ref *job-semma* job-id))
  (semaphore-wait sema)
  (hash-remove! *job-semma* job-id)
  (hash-ref *completed-jobs* job-id))

(define (is-job-finished job-id)
#| Not really sure what this should return yet. s|#
 (hash-ref *job-status* job-id #f))

(define (job-count)
 (hash-count *job-status*))

(define (start-job-server config global-demo global-output)
 ;; Pass along local global values
 (set! *demo?* global-demo)
 (set! *demo-output* global-output)
 (thread-send *worker-thread* config))
(define (is-server-up)
 (thread-running? *worker-thread*))

; Helers to isolated *completed-jobs*
(define (helper id) 
 (hash-has-key? *completed-jobs* id))

; TODO remove this as it returns the job outside it's scope
(define (helper2 id) 
 (hash-ref *completed-jobs* id))
(define (helper3)
  (in-hash *completed-jobs*))

(define (already-computed? job-id)
  (or (hash-has-key? *completed-jobs* job-id)
      (and (*demo-output*)
           (directory-exists? (build-path (*demo-output*) (format "~a.~a" job-id *herbie-commit*))))))

(define (compute-job-id job-info)
 (sha1 (open-input-string (~s job-info))))

; public but not sure how to make private yet
(define *demo?* (make-parameter false))
(define *demo-output* (make-parameter false))
;; Private 
; globals
; TODO I'm sure these can encapslated some how.
(define *completed-jobs* (make-hash))
(define *job-status* (make-hash))
(define *job-semma* (make-hash))

;; Helpers 
; Handles semaphore and async part of a job
(struct work (id job sema))

; Encapsulates semaphores and async part of jobs.
(define (start-work job)
 (define job-id (compute-job-id job))
 (hash-set! *job-status* job-id (*timeline*))
 (define sema (make-semaphore))
 (hash-set! *job-semma* job-id sema)
 (thread-send *worker-thread* (work job-id job sema))
 job-id)

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
 (hash-remove! *job-semma* job-id))

(define (wrapper-run-herbie cmd job-id)
  (print-job-message (herbie-command-command cmd) job-id (syntax->datum (herbie-command-formula cmd)))
  (define result (run-herbie 
   (herbie-command-command cmd)
   (parse-test (herbie-command-formula cmd))
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
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label (symbol->string command) job-id job-str))