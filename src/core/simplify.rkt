#lang racket

(require egg-herbie)
(require "../common.rkt" "../programs.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/rules.rkt" "../alternative.rkt")

(provide simplify-expr simplify-batch
         make-simplification-combinations
         rules->irules egg-run-rules)

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-plugins))

;; One module to rule them all, the great simplify. It uses egg-herbie
;; to simplify an expression as much as possible without making
;; unnecessary changes. We do this by creating an egraph, saturating
;; it partially, then extracting the simplest expression from it.
;;
;; Simplify makes only one guarantee: that the input is mathematically
;; equivalent to the output. For any exact x, evaluating the input on
;; x will yield the same expression as evaluating the output on x.

;; prefab struct used to send rules to egg-herbie
(struct irule (name input output) #:prefab)

(define (rules->irules rules)
  (for/list ([rule rules])
    (irule (rule-name rule) (rule-input rule) (rule-output rule))))

;; given an alt, locations, and a hash from expr to simplification options
;; make all combinations of the alt using the simplification options available
(define (make-simplification-combinations child locs simplify-hash)
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
            (alt child* (list 'simplify loc) (list child))
            child))))

  ; Simplify-streaming lite
  (for/fold ([all '()] #:result (reverse all)) ([option (in-list options)])
    (if (alt-equal? option child)
        (cons option all)
        (append (list option child) all))))

(define/contract (simplify-expr expr #:rules rls #:precompute [precompute? false])
  (->* (expr? #:rules (listof rule?)) (#:precompute boolean?) expr?)
  (last (first (simplify-batch (list expr) #:rules rls #:precompute precompute?))))

;; for each expression, returns a list of simplified versions corresponding to egraph iterations
;; the last expression is the simplest unless something went wrong due to unsoundness
(define/contract (simplify-batch exprs #:rules rls #:precompute [precompute? false])
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof (listof expr?)))

  (define driver simplify-batch-egg)
  (timeline-push! 'inputs (map ~a exprs))
  (define resulting-lists (driver exprs #:rules rls #:precompute precompute?))
  (define out
    (for/list ([results resulting-lists] [expr exprs])
      (remove-duplicates (cons expr results))))
  (timeline-push! 'outputs (map ~a (apply append out)))
  out)

(define/contract (simplify-batch-egg exprs #:rules rls #:precompute precompute?)
  (-> (listof expr?) #:rules (listof rule?) #:precompute boolean? (listof (listof expr?)))
  (timeline-push! 'method "egg-herbie")
  (define irules (rules->irules rls))

  (with-egraph
   (lambda (egg-graph)
     (define node-ids (map (curry egraph-add-expr egg-graph) exprs))
     (define iter-data (egg-run-rules egg-graph (*node-limit*) irules node-ids (and precompute? true)))
        
     (when (egraph-is-unsound-detected egg-graph)
       (warn 'unsound-rules #:url "faq.html#unsound-rules"
             "Unsound rule application detected in e-graph. Results from simplify may not be sound."))
        
     (for ([rule rls])
       (define count (egraph-get-times-applied egg-graph (rule-name rule)))
       (when (> count 0)
         (timeline-push! 'rules (~a (rule-name rule)) count)))
        
     (for/list ([id node-ids])
        (for/list ([iter (in-range (length iter-data))])
          (egg-expr->expr (egraph-get-simplest egg-graph id iter) egg-graph))))))

(define (stop-reason->string sr)
  (match sr
   ['saturated  "saturated"]
   ['iter-limit "iter limit"]
   ['node-limit "node limit"]
   ['unsound    "unsound"]))

(define (egg-run-rules egg-graph node-limit irules node-ids precompute? #:limit [iter-limit #f])
  (define ffi-rules (make-ffi-rules irules))
  (define start-time (current-inexact-milliseconds))

  #;(define (timeline-cost iter)
      (define cnt (egraph-get-size egg-graph)) 
      (timeline-push! 'egraph iter cnt cost (- (current-inexact-milliseconds) start-time)))
  
  (define iteration-data (egraph-run egg-graph node-limit ffi-rules precompute? iter-limit))
  (let loop ([iter iteration-data] [counter 0] [time 0])
    (unless (null? iter)
      (define cnt (iteration-data-num-nodes (first iter)))
      (define cost (apply + (map (λ (node-id) (egraph-get-cost egg-graph node-id counter)) node-ids)))
      (define new-time (+ time (iteration-data-time (first iter))))
      (timeline-push! 'egraph counter cnt cost new-time)
      (loop (rest iter) (+ counter 1) new-time)))

  (define sr (egraph-stop-reason egg-graph))
  (timeline-push! 'stop (stop-reason->string sr) 1)
  
  (free-ffi-rules ffi-rules)
  iteration-data)

(module+ test
  (require "../syntax/types.rkt" "../syntax/rules.rkt")
  (*needed-reprs* (list (get-representation 'binary64) (get-representation 'binary32)))
  (define all-simplify-rules (*simplify-rules*))

  ;; check that no rules in simplify match on bare variables
  ;; this would be bad because we don't want to match type-specific operators on a value of a different type
  (for ([rule all-simplify-rules])
    (check-true
     (not (symbol? (rule-input rule)))
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
          [(cos.f64 (PI.f64)) . -1]
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
