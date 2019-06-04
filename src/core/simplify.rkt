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

(define/contract (simplify-expr expr #:rules rls)
  (-> expr? #:rules (listof rule?) expr?)
  (first (simplify-batch (list expr) #:rules rls)))

(define/contract (simplify-batch exprs #:rules rls)
  (-> (listof expr?) #:rules (listof rule?) (listof expr?))
  (debug #:from 'simplify (format "Simplifying:\n  ~a" (string-join (map ~a exprs) "\n  ")))

  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))

  (for/and ([iter (in-naturals 0)])
    (debug #:from 'simplify #:depth 2 (format "iteration ~a: ~a enodes" iter (egraph-cnt eg)))
    (timeline-push! 'egraph iter (egraph-cnt eg))
    (one-iter eg rls))
  (debug #:from 'simplify #:depth 2 (format "iteration complete: ~a enodes" (egraph-cnt eg)))
  (timeline-push! 'egraph "done" (egraph-cnt eg))

  (define out (apply extract-smallest eg ens))
  (debug #:from 'simplify (format "Simplified to:\n  ~a" (string-join (map ~a out) "\n  ")))
  out)

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

(define (apply-match eg rl en bindings)

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
  ;; Mark this node as having this rule applied so that we don't try
  ;; to apply it again.
  (when (subset? bindings-set valid-bindings) (rule-applied! en rl)))

;; Iterates the egraph by applying each of the given rules in parallel
;; to the egraph nodes.
(define (one-iter eg rls)
  (define change? #f)
  (define (run-phase f . args)
    (define old-cnt (egraph-cnt eg))
    (apply f args)
    (define changed? (> (egraph-cnt eg) old-cnt))
    (set! change? (or changed? change?))
    changed?)

  (for ([m (find-matches (egraph-leaders eg) rls)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (match-define (list rl en bindings ...) m)
    ;; The loop here ensures that we don't pass the node limit just
    ;; because the bindings are too long. This is pretty ugly.
    (let loop ([bindings bindings])
      (cond
       [(>= (egraph-cnt eg) (*node-limit*))
        (void)]
       [(<= (+ (length bindings) (egraph-cnt eg)) (*node-limit*))
        (when (run-phase apply-match eg rl en bindings)
          (reduce-to-single! eg en))]
       [else
        (let-values ([(head tail) (split-at bindings (- (*node-limit*) (egraph-cnt eg)))])
          (loop head)
          (loop tail))])))
  (for ([en (egraph-leaders eg)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (when (run-phase set-precompute! eg en)
      (reduce-to-single! eg en)))
  (and change? (< (egraph-cnt eg) (*node-limit*))))

(define (set-precompute! eg en)
  (define type (enode-type en))
  (for ([var (enode-vars en)] #:when (list? var))
    (define constexpr
      (cons (car var)
            (map (compose (curry setfindf constant?) enode-vars) (cdr var))))
    (when (andmap identity constexpr)
      (with-handlers ([exn:fail:contract:divide-by-zero? void])
        (define res (eval-const-expr constexpr))
        (when (and ((value-of type) res) (exact-value? type res))
          (define en* (mk-enode-rec! eg (val-to-type type res)))
          (merge-egraph-nodes! eg en en*))))))
  

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

  (check set-member?
         '((* x 6) (* 6 x))
         (simplify-expr '(+ (+ (+ (+ (+ x x) x) x) x) x) #:rules (*simplify-rules*)))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (- b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (simplify-expr expr #:rules (*simplify-rules*)))))))
