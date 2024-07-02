#lang racket

(require "../common.rkt"
         "../programs.rkt"
         "../timeline.rkt"
         "../syntax/rules.rkt"
         "egg-herbie.rkt"
         "matcher.rkt")

(provide rewrite-expressions)

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

;;
;;  Non-recursive rewriter
;;

;; Applies rules verbatim
(define (rewrite-once expr ctx #:rules rules)
  ;; actually match
  (define rule-apps (make-hash))
  (define expr-repr (repr-of expr ctx))
  (define changelists
    (reap [sow]
      (for ([rule rules] #:when (equal? expr-repr (rule-otype rule)))
        (let* ([result (rule-apply rule expr)])
          (when result
            (hash-update! rule-apps (rule-name rule) add1 0)
            (sow (list (car result) rule)))))))
  ;; rule statistics
  (for ([(name count) (in-hash rule-apps)])
    (when (> count 0) (timeline-push! 'rules (~a name) count)))
  changelists)

;;
;;  Egg recursive rewriter
;;

(define (rewrite-expressions exprs reprs schedule ctx)
  (timeline-push! 'method "batch-egg-rewrite")
  (timeline-push! 'inputs (map ~a exprs))
  (define extractor
      (typed-egg-extractor
        (if (*egraph-platform-cost*)
            platform-egg-cost-proc
            default-egg-cost-proc)))
  (define e-input
    (make-egg-runner exprs
                    reprs
                    schedule
                    #:context ctx
                    #:extractor extractor))
  (match-define (cons variantss _) (run-egg e-input #t))

  (define out
    (for/list ([variants variantss])
      (for/list ([variant (remove-duplicates variants)])
          (list variant e-input))))

  (timeline-push! 'outputs (map ~a (apply append variantss)))
  out)
  

(module+ test
  (require rackunit)
  (require "../syntax/types.rkt" "../load-plugin.rkt")
  (load-herbie-builtins)
  (*context* (make-debug-context '(x)))

  (define rules (platform-impl-rules (*rules*) #:expansive? #t))

  (let ([chngs (rewrite-once '(+.f64 x x) (*context*) #:rules rules)])
    (check-equal? (length chngs) 13 (format "rewrites ~a" chngs)))

  (let ([chngs (rewrite-once '(*.f64 x x) (*context*) #:rules rules)])
    (check-equal? (length chngs) 11 (format "rewrites ~a" chngs)))

  (void))
