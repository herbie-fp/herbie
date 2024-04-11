#lang racket

(require "../syntax/rules.rkt"
         "../common.rkt"
         "../programs.rkt"
         "../timeline.rkt"
         "egg-herbie.rkt"
         "matcher.rkt")

(provide rewrite-expressions)

;;
;;  Non-recursive rewriter
;;

(define (rewrite-once expr ctx #:rules rules)
  ;; we want rules over representations
  (define canon (make-hash))
  (define rules*
    (for/fold ([rules* '()]) ([r (in-list rules)])
      (define impl-rules (rule->impl-rules r))
      (for ([r* (in-list impl-rules)])
        (hash-set! canon (rule-name r*) (rule-name r)))
      (append impl-rules rules*)))
  ;; actually match
  (define rule-apps (make-hash))
  (define expr-repr (repr-of expr ctx))
  (define changelists
    (reap [sow]
      (for ([rule rules*] #:when (equal? expr-repr (rule-otype rule)))
        (let* ([result (rule-apply rule expr)])
          (when result
            (define canon-name (hash-ref canon (rule-name rule)))
            (hash-update! rule-apps canon-name add1 1)
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
  (define e-input
    (make-egg-query exprs
                    reprs
                    schedule
                    #:context ctx
                    #:extractor (typed-egg-extractor platform-egg-cost-proc)))
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

  (let ([chngs (rewrite-once '(+.f64 x x) (*context*) #:rules (*rules*))])
    (check-equal? (length chngs) 13 (format "rewrites ~a" chngs)))

  (let ([chngs (rewrite-once '(*.f64 x x) (*context*) #:rules (*rules*))])
    (check-equal? (length chngs) 11 (format "rewrites ~a" chngs)))

  (void))
