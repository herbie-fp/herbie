#lang racket

(require "../common.rkt" "../programs.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/rules.rkt" "../alternative.rkt" "../config.rkt" "../syntax/types.rkt" "../egglog/egraph-conversion.rkt" "../egglog/run-egglog.rkt"
         "../points.rkt")

(provide simplify-batch get-proof
         make-simplification-combinations
         (struct-out simplify-input))

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-plugins))

;; The input and output of simplify- simplify is re-run when proofs are needed
(struct simplify-input (exprs proofs rules precompute?))

;; given an alt, locations, and a hash from expr to simplification options
;; make all combinations of the alt using the simplification options available
(define (make-simplification-combinations child locs simplify-hash input)
  ;; use this for simplify streaming
  ;; (define location-options
  ;;   (apply cartesian-product
  ;;    (for/list ([loc locs])
  ;;      (hash-ref simplify-hash (location-get loc (alt-program child)))))))
  (define location-options
    (apply cartesian-product
           (for/list ([loc locs])
             (list (last (hash-ref simplify-hash (location-get loc (alt-program child))))))))

  (define options
    (for/list ([option location-options])
      (for/fold ([child child]) ([replacement option] [loc locs])
        (define child* (location-do loc (alt-program child) (lambda (_) replacement)))
        (if (not (equal? (alt-program child) child*))
            (alt child* `(simplify ,loc ,input #f #f) (list child))
            child))))

  ; Simplify-streaming lite
  (for/fold ([all '()] #:result (reverse all)) ([option (in-list options)])
    (if (alt-equal? option child)
        (cons option all)
        (append (list option child) all))))

;; for each expression, returns a list of simplified versions corresponding to egraph iterations
;; the last expression is the simplest unless something went wrong due to unsoundness
;; if the input specifies proofs, it instead returns proofs for these expressions
(define/contract (simplify-batch ctx pctx input)
  (->* (context?
        pcontext?
        (struct/c simplify-input
                  (listof expr?)
                  (listof (cons/c expr? expr?))
                  (listof rule?)
                  boolean?))
       (listof (listof expr?)))

  (timeline-push! 'inputs (map ~a (simplify-input-exprs input)))

  (define results
    (run-simplify-input
     ctx
     pctx
     input))

  (define out
    (for/list ([result results] [expr (simplify-input-exprs input)])
      (remove-duplicates (list expr result))))
  (timeline-push! 'outputs (map ~a (apply append out)))

  out)

(define (run-simplify-input ctx pctx input)
  (define exprs (simplify-input-exprs input))
  (for ([expr exprs])
    (when (unsound-expr? expr)
      (error (format "Unsound expression: ~a" expr))))
  (define precompute? (simplify-input-precompute? input))
  (define proofs (simplify-input-proofs input))
  (define rules (simplify-input-rules input))

  (timeline-push! 'method "egglog")
  (map first (run-egglog ctx pctx (simplify-input-exprs input))))


(define (stop-reason->string sr)
  (match sr
    ['saturated  "saturated"]
    ['iter-limit "iter limit"]
    ['node-limit "node limit"]
    ['unsound    "unsound"]))

(define (get-proof input start end)
  empty
  #;(run-simplify-input
   input
   (lambda (egg-graph node-ids iter-data)
     (define proof (egraph-get-proof egg-graph start end))
     (when (null? proof)
       (error (format "Failed to produce proof for ~a to ~a" start end)))
     proof)))

(module+ test
  (require "../syntax/types.rkt" "../syntax/rules.rkt")

  ;; set parameters
  (define vars '(x a b c))
  (*context* (make-debug-context vars))
  (*needed-reprs* (list (get-representation 'binary64)
                        (get-representation 'binary32)))
  (define all-simplify-rules (*simplify-rules*))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (not (symbol? (rule-input rule)))
     (string-append "Rule failed: " (symbol->string (rule-name rule)))))

  (define ctx (context (list 'x 'y 'z) 'binary64 (list 'binary64 'binary64 'binary64)))

  (define (test-simplify . args)
    (map last (simplify-batch
               ctx
               (mk-pcontext empty empty)
               empty
               (simplify-input args empty (*simplify-rules*) true))))

  (define test-exprs
    #hash([1 . 1]
          [0 . 0]
          [(+.f64 1 0) . 1]
          [(+.f64 1 5) . 6]
          [(+.f64 x 0) . x]
          [(-.f64 x 0) . x]
          [(*.f64 x 1) . x]
          [(/.f64 x 1) . x]
          [(-.f64 (*.f64 1 x) (*.f64 (+.f64 x 1) 1)) . -1]
          [(-.f64 (+.f64 x 1) x) . 1]
          [(-.f64 (+.f64 x 1) 1) . x]
          [(/.f64 (*.f64 x 3) x) . 3]
          [(-.f64 (*.f64 (sqrt.f64 (+.f64 x 1)) (sqrt.f64 (+.f64 x 1)))
                  (*.f64 (sqrt.f64 x) (sqrt.f64 x))) . 1]
          [(+.f64 1/5 3/10) . 1/2]
          [(cos.f64 (PI.f64)) . -1]
          [(pow.f64 (E.f64) 1) . (E.f64)]
          ;; this test is problematic and runs out of nodes currently
          ;[(/ 1 (- (/ (+ 1 (sqrt 5)) 2) (/ (- 1 (sqrt 5)) 2))) . (/ 1 (sqrt 5))]
          ))

  (*timeline-disabled* true)
  (define outputs (apply test-simplify (hash-keys test-exprs)))
  (for ([(original target) test-exprs] [output outputs])
    (with-check-info (['original original])
      (check-equal? output target)))

  (check set-member? '((*.f64 x 6) (*.f64 6 x))
         (first (test-simplify '(+.f64 (+.f64 (+.f64 (+.f64 (+.f64 x x) x) x) x) x))))

  (define no-crash-exprs
    '((exp.f64 (/.f64 (/.f64 (*.f64 (*.f64 c a) 4)
                             (-.f64 (neg.f64 b) (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 4 (*.f64 a c)))))) (*.f64 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
      (check-not-exn (λ () (test-simplify expr))))))
