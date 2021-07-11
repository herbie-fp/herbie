#lang racket

(require pkg/lib)
(require "../common.rkt" "../programs.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/rules.rkt" "../alternative.rkt")

(provide simplify-expr simplify-batch make-simplification-combinations)
(module+ test (require rackunit))

;; One module to rule them all, the great simplify. It uses egg-herbie
;; to simplify an expression as much as possible without making
;; unnecessary changes. We do this by creating an egraph, saturating
;; it partially, then extracting the simplest expression from it.
;;
;; If egg-herbie is not available, simplify uses regraph instead.
;;
;; Simplify makes only one guarantee: that the input is mathematically
;; equivalent to the output. For any exact x, evaluating the input on
;; x will yield the same expression as evaluating the output on x.

;; fall back on herbie-egraph if egg-herbie is unavailable
(define use-egg-math?
  (or
   (hash-has-key? (installed-pkg-table) "egg-herbie")
   (hash-has-key? (installed-pkg-table) "egg-herbie-windows")
   (hash-has-key? (installed-pkg-table) "egg-herbie-osx")
   (hash-has-key? (installed-pkg-table) "egg-herbie-linux")))

;; prefab struct used to send rules to egg-herbie
(struct irule (name input output) #:prefab)

(define (rules->irules rules)
  (if use-egg-math?
      (for/list [(rule rules)]
        (irule (rule-name rule) (rule-input rule) (rule-output rule)))
      (list)))


;; given an alt, locations, and a hash from expr to simplification options
;; make all combinations of the alt using the simplification options available
(define (make-simplification-combinations child locs simplify-hash)
  ;; use this for simplify streaming
  #;(define location-options
    (apply cartesian-product
     (for/list ([loc locs])
       (hash-ref simplify-hash (location-get loc (alt-program child))))))
  (define location-options
    (apply cartesian-product
     (for/list ([loc locs])
       (list (last (hash-ref simplify-hash (location-get loc (alt-program child))))))))
  
  (define options
    (for/list ([option location-options])
              (for/fold ([child child]) ([replacement option] [loc locs])
                        (define child* (location-do loc (alt-program child) (lambda (expr) replacement)))
                        (if (not (equal? (alt-program child) child*))
                            (alt child* (list 'simplify loc) (list child))
                            child))))
  ;; omit the original expression
  (filter (lambda (option) (not (alt-equal? option child))) options))

(define/contract (simplify-expr expr #:rules rls #:precompute [precompute? false])
  (->* (expr? #:rules (listof rule?)) (#:precompute boolean?) expr?)
  (last (first (simplify-batch (list expr) #:rules rls #:precompute precompute?))))

;; for each expression, returns a list of simplified versions corresponding to egraph iterations
;; the last expression is the simplest unless something went wrong due to unsoundness
(define/contract (simplify-batch exprs #:rules rls #:precompute [precompute? false])
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof (listof expr?)))

  (define driver
    (cond
     [use-egg-math?
      simplify-batch-egg]
     [else
      (warn 'simplify #:url "faq.html#egg-herbie"
            "Falling back on regraph because egg-herbie package not installed")
      simplify-batch-regraph]))

  (debug #:from 'simplify "Simplifying using" driver ":\n " (string-join (map ~a exprs) "\n  "))
  (define resulting-lists (driver exprs #:rules rls #:precompute precompute?))
  (define out
    (for/list ([results resulting-lists] [expr exprs])
             (remove-duplicates (cons expr results))))
  (debug #:from 'simplify "Simplified to:\n " (string-join (map ~a (map last out)) "\n  "))
    
  out)

(define-syntax-rule (regraph method)
  (dynamic-require 'regraph 'method))

(define/contract (simplify-batch-regraph exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof (listof expr?)))
  (timeline-push! 'method "regraph")

  (define start-time (current-inexact-milliseconds))
  (define (log rg iter)
    (define cnt ((regraph regraph-count) rg))
    (define cost ((regraph regraph-cost) rg))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " cnt " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time)))

  (define rg ((regraph make-regraph) (map munge exprs) #:limit (*node-limit*)))

  (define phases
    (list ((regraph rule-phase) (map (compose munge rule-input) rls)
                                (map (compose munge rule-output) rls))
          (and precompute? ((regraph precompute-phase) eval-application))
          (regraph prune-phase)
          (regraph extractor-phase)))

  (for/and ([iter (in-naturals 0)])
    (log rg iter)
    (define initial-cnt ((regraph regraph-count) rg))
    ;; Iterates the egraph by applying each of the given rules to the egraph
    (for ([phase phases] #:when phase) (phase rg))
    (and (< initial-cnt ((regraph regraph-count) rg) (*node-limit*))))

  (log rg "done")
  (map list (map unmunge ((regraph regraph-extract) rg))))

(define-syntax-rule (egg method)
  (dynamic-require 'egg-herbie 'method))

(define/contract (simplify-batch-egg exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof (listof expr?)))
  (timeline-push! 'method "egg-herbie")
  (define irules (rules->irules rls))

  ((egg with-egraph)
   (lambda (egg-graph)
     ((egg egraph-add-exprs)
      egg-graph
      exprs
      (lambda (node-ids)
        (define iter-data (egg-run-rules egg-graph (*node-limit*) irules node-ids (and precompute? true)))
        
        (when ((egg egraph-is-unsound-detected) egg-graph)
          (warn 'unsound-rules #:url "faq.html#unsound-rules"
               "Unsound rule application detected in e-graph. Results from simplify may not be sound."))
        
        (for ([rule rls])
          (define count ((egg egraph-get-times-applied) egg-graph (rule-name rule)))
          (when (> count 0)
            (timeline-push! 'rules (~a (rule-name rule)) count)))
        
        (map
         (lambda (id)
           (for/list ([iter (in-range (length iter-data))])
                      ((egg egg-expr->expr)
                       ((egg egraph-get-simplest) egg-graph id iter)
                       egg-graph)))
         node-ids))))))

(define (egg-run-rules egg-graph node-limit irules node-ids precompute?)
  (define ffi-rules ((egg make-ffi-rules) irules))
  (define start-time (current-inexact-milliseconds))

  #;(define (timeline-cost iter)
    
    (define cnt ((egg egraph-get-size) egg-graph))
    
    (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time)))
  
  (define iteration-data ((egg egraph-run) egg-graph node-limit ffi-rules precompute?))

  (let loop
    ([iter iteration-data] [counter 0] [time 0])
    (cond
      [(empty? iter)
       void]
      [else
       (define cnt ((egg iteration-data-num-nodes) (first iter)))
       (define cost
           (apply +
                  (map (lambda (node-id) ((egg egraph-get-cost) egg-graph node-id counter)) node-ids)))
       (debug #:from 'simplify #:depth 2 "iteration " counter ": " cnt " enodes " "(cost " cost ")")
       (define new-time (+ time ((egg iteration-data-time) (first iter))))
       (timeline-push! 'egraph counter cnt cost new-time)
       (loop (rest iter) (+ counter 1) new-time)]))

  ((egg free-ffi-rules) ffi-rules)
  iteration-data)

(define (munge expr)
  ;; Despite the name, `expr` might be an expression OR a pattern
  (match expr
    [(? number?) expr]
    [(? constant?) (list expr)]
    [(? variable?) expr]
    [(list head subs ...)
     (cons head (map munge subs))]))

(define (unmunge expr)
  ;; Despite the name, `expr` might be an expression OR a pattern
  (match expr
    [(list constant)
     constant]
    [(list head subs ...)
     (cons head (map unmunge subs))]
    [_ expr]))

(module+ test
  (require "../interface.rkt" "../syntax/rules.rkt")
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(x a b c)))
  (*needed-reprs* (list (get-representation 'binary64) (get-representation 'binary32)))
  (define all-simplify-rules (*simplify-rules*))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (or
      (not (symbol? (rule-input rule)))
      (constant? (rule-input rule)))
     (string-append "Rule failed: " (symbol->string (rule-name rule)))))
  
  (define (test-simplify . args)
    (map last (simplify-batch args #:rules (*simplify-rules*) #:precompute true)))

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
          [(cos.f64 PI.f64) . -1]
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
