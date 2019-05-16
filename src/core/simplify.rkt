#lang racket

(require "../common.rkt" "../programs.rkt" "../float.rkt" "../timeline.rkt")
(require "../syntax/rules.rkt" "../syntax/types.rkt")
(require "enode.rkt" "egraph.rkt" "ematch.rkt")
(provide simplify-expr simplify-batch)
(module+ test (require rackunit))

;;################################################################################;;
;;# One module to rule them all, the great simplify. This makes use of the other
;;# modules in this directory to simplify an expression as much as possible without
;;# making unecessary changes. We do this by creating an egraph, saturating it
;;# partially, then extracting the simplest expression from it.
;;#
;;# Simplify attempts to make only one strong guarantee:
;;# that the input is mathematically equivalent to the output; that is, for any
;;# exact x, evalutating the input on x will yield the same expression as evaluating
;;# the output on x.
;;#
;;################################################################################;;

;; Cap the maximum size of an egraph
(define *node-limit* (make-parameter 2000))

(define/contract (simplify-expr expr #:rules rls)
  (-> expr? #:rules (listof rule?) expr?)
  (first (simplify-batch (list expr) #:rules rls)))

(define/contract (simplify-batch exprs #:rules rls)
  (-> (listof expr?) #:rules (listof rule?) (listof expr?))
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))

  (iterate-egraph! eg #:rules rls)

  (define out (apply extract-smallest eg ens))
  (debug #:from 'simplify (format "Simplified to:\n  ~a" (string-join (map ~a out) "\n  ")))
  out)

(define (iterate-egraph! eg #:rules [rls (*simplify-rules*)])
  (let loop ([iter 1])
    (define start-cnt (egraph-cnt eg))
    (debug #:from 'simplify #:depth 2 (format "iteration ~a: (~a enodes)" iter start-cnt))
    (one-iter eg rls)
    (if (< start-cnt (egraph-cnt eg) (*node-limit*))
        (loop (+ iter 1))
        (timeline-push! 'iters iter (egraph-cnt eg)))))

(define (rule-applicable? rl en)
  (or (not (variable? (rule-input rl)))
      (equal? (dict-ref (rule-itypes rl) (rule-input rl)) (enode-type en))))

;; Tries to match the rules against the given enodes, and returns a
;; list of matches found. Matches are of the form:
;; 
;; (rule enode . bindings)
;;
;; where bindings is a list of different matches between the rule and
;; the enode.

(define (find-matches ens rls)
  (reap [sow]
        (for* ([rl rls] [en ens]
               #:when (rule-applicable? rl en)
               #:unless (rule-applied? en rl))
          (define bindings (match-e (rule-input rl) en))
          (unless (null? bindings)
            (sow (list* rl en bindings))))))

(define (apply-match match eg)
  (match-define (list rl en bindings ...) match)

  ;; These next two lines are here because an earlier match
  ;; application may have pruned the tree, invalidating the this
  ;; one. Luckily, a pruned enode will still point to it's old
  ;; leader, so we just get the leader, and then double check the
  ;; bindings to make sure our match hasn't changed.
  
  (define en* (pack-leader en))
  (define bindings-set (apply set bindings))
  (define bindings* (apply set (match-e (rule-input rl) en*)))
  (define valid-bindings (set-intersect bindings-set bindings*))

  (for ([binding valid-bindings])
    (merge-egraph-nodes! eg en (substitute-e eg (rule-output rl) binding)))
  ;; Prune the enode if we can
  (unless (null? valid-bindings)
    ;; If one of the variations of the enode is a single variable or
    ;; constant, reduce to that.
    (reduce-to-single! eg en))
  ;; Mark this node as having this rule applied so that we don't try
  ;; to apply it again.
  (when (subset? bindings-set valid-bindings) (rule-applied! en rl)))

;; Iterates the egraph by applying each of the given rules in parallel
;; to the egraph nodes.
(define (one-iter eg rls)
  (for ([m (find-matches (egraph-leaders eg) rls)])
    (apply-match m eg))
  (for-each (curry set-precompute! eg) (egraph-leaders eg)))

(define (set-precompute! eg en)
  (define type (enode-type en))
  (define simplified? false)
  (for ([var (enode-vars en)] #:when (list? var))
    (define constexpr
      (cons (car var)
            (map (compose (curry setfindf constant?) enode-vars) (cdr var))))
    (when (andmap identity constexpr)
      (with-handlers ([exn:fail:contract:divide-by-zero? void])
        (define res (eval-const-expr constexpr))
        (when (and ((value-of type) res) (exact-value? type res))
          (merge-egraph-nodes! eg en (mk-enode-rec! eg (val-to-type type res)))
          (set! simplified? true)))))
  (when simplified? (reduce-to-single! eg en)))

(define (extract-smallest eg . ens)
  ;; The work list maps enodes to a pair (cost . expr) of that node's
  ;; cheapest representation and its cost. If the cost is #f, the expr
  ;; is also #f, and in this case no expression is yet known for that
  ;; enode.
  (define work-list (make-hash))
  (for ([en ens])
    (hash-set! work-list (pack-leader en) (cons #f #f)))

  ;; Extracting the smallest expression means iterating, until
  ;; fixedpoint, either discovering new relevant expressions or
  ;; cheaper expressions for some expression.
  (let loop ([iter 0])
    (define changed? #f)
    (debug #:from 'simplify #:depth 2
           (format "Extracting #~a: cost ~a inf + ~a"
                   iter (count (compose not car) (hash-values work-list))
                   (apply + (filter identity (map car (hash-values work-list))))))
    (for ([leader (in-list (hash-keys work-list))])
      (define vars (enode-vars leader))
      (define vars*
        (filter identity
                (for/list ([var vars])
                  (match var
                    [(list op args ...)
                     (define args*
                       (for/list ([en args])
                         (define subleader (pack-leader en))
                         (match (hash-ref work-list subleader (cons #f #t))
                           [(cons (? number? cost) best-arg)
                            best-arg]
                           [(cons #f not-in-hash?)
                            (hash-set! work-list subleader (cons #f #f))
                            (set! changed? (or changed? not-in-hash?))
                            #f])))
                     (if (andmap identity args*)
                         (cons op args*)
                         #f)]
                    [_
                     var]))))
      (match vars*
        ['() #f]
        [_
         (define best-resolution (argmin expression-cost vars*))
         (define cost (expression-cost best-resolution))
         (define old-cost (car (hash-ref work-list leader)))
         (when (or (not old-cost) (< cost old-cost))
           (hash-set! work-list leader (cons cost best-resolution))
           (set! changed? #t))]))
    (if changed?
        (loop (+ iter 1))
        (for/list ([en ens])
          (cdr (hash-ref work-list (pack-leader en)))))))

(module+ test
  (define test-exprs
    #hash([1 . 1]
          [0 . 0]
          [(+ 1 0) . 1]
          [(+ 1 5) . 6]
          [(+ x 0) . x]
          [(- x 0) . x]
          [(* x 1) . x]
          [(/ x 1) . x]
          [(- (* 1 x) (* (+ x 1) 1)) . -1]
          [(- (+ x 1) x) . 1]
          [(- (+ x 1) 1) . x]
          [(/ (* x 3) x) . 3]
          [(- (* (sqrt (+ x 1)) (sqrt (+ x 1)))
              (* (sqrt x) (sqrt x))) . 1]
          [(re (complex a b)) . a]))

  (for ([(original target) test-exprs])
    (with-check-info (['original original])
       (check-equal? (simplify-expr original #:rules (*simplify-rules*)) target)))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (- b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (simplify-expr expr #:rules (*simplify-rules*)))))))
