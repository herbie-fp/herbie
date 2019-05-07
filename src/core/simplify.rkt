#lang racket

(require "../common.rkt")
(require "../programs.rkt")
(require "../syntax/rules.rkt")
(require "egraph.rkt")
(require "ematch.rkt")
(require "enode.rkt")

(provide simplify-expr simplify-batch *max-egraph-iters*)

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

;; Cap the number of iterations to try at this.
(define *max-egraph-iters* (make-parameter 6))
(define *node-limit* (make-parameter 500))

(define/contract (simplify-expr expr #:rules rls)
  (-> expr? #:rules (listof rule?) expr?)
  (debug #:from 'simplify (format "Simplifying ~a" expr))
  (if (has-nan? expr) +nan.0
      (let* ([iters (min (*max-egraph-iters*) (iters-needed expr))]
	     [eg (mk-egraph)]
             [en (mk-enode-rec! eg expr)])
	(iterate-egraph! eg iters #:rules rls)
	(define out (extract-smallest eg en))
        (debug #:from 'simplify (format "Simplified to ~a" out))
        out)))

(define (simplify-batch exprs #:rules rls)
  (debug #:from 'simplify (format "Simplifying ~a" (string-join (map ~a exprs) ", ")))
  (let* ([iters (min (*max-egraph-iters*) (apply max (map iters-needed exprs)))]
	 [eg (mk-egraph)]
         [ens (for/list ([expr exprs]) (mk-enode-rec! eg expr))])
    (parameterize ([*node-limit* 1000 #;(* (length exprs) (*node-limit*))])
      (iterate-egraph! eg iters #:rules rls))
    (define out (for/list ([en ens]) (extract-smallest eg en))) ; TODO: batch extract
    (debug #:from 'simplify (format "Simplified to ~a" (string-join (map ~a out) ", ")))
    out))

(define (has-nan? expr)
  (or (and (number? expr) (nan? expr))
      (and (list? expr)
	   (ormap has-nan? (cdr expr)))))

;; Returns the worst-case iterations needed to simplify this expression
(define (iters-needed expr)
  (if (not (list? expr)) 0
      (let ([sub-iters-needed (apply max (map iters-needed (cdr expr)))])
	(if (let ([op (car expr)]) (or (eq? op '*) (eq? op '+) (eq? op '-) (eq? op '/)))
	    (+ 2 sub-iters-needed)
	    (+ 1 sub-iters-needed)))))

(define (iterate-egraph! eg iters #:rules [rls (*simplify-rules*)])
  (let ([start-cnt (egraph-cnt eg)])
    (debug #:from 'simplify #:depth 2 (format "iters left: ~a (~a enodes)" iters start-cnt))
    (one-iter eg rls)
    (when (and (> (egraph-cnt eg) start-cnt)
	       (> iters 1)
	       (< (egraph-cnt eg) (*node-limit*)))
      (iterate-egraph! eg (sub1 iters) #:rules rls))))

;; Iterates the egraph by applying each of the given rules in parallel
;; to the egraph nodes.
(define (one-iter eg rls)

  ;; Tries to match the rules against the given enodes, and returns a
  ;; list of matches found. Matches are of the form:
  ;; 
  ;; (rule enode . bindings)
  ;;
  ;; where bindings is a list of different matches between the rule
  ;; and the enode.
  (define (find-matches ens)
    (filter (negate null?)
	    (for*/list ([rl rls]
			[en ens]
                        #:when (or (not (variable? (rule-input rl)))
                                   (equal? (dict-ref (rule-itypes rl) (rule-input rl)) (enode-type en))))
	      (if (rule-applied? en rl) '()
		  (let ([bindings (match-e (rule-input rl) en)])
		    (if (null? bindings) '()
			(list* rl en bindings)))))))

  (define (apply-match match)
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
    (unless (null? valid-bindings) (try-prune-enode en))
    ;; Mark this node as having this rule applied so that we don't try
    ;; to apply it again.
    (when (subset? bindings-set valid-bindings) (rule-applied! en rl)))

  (define (try-prune-enode en)
    ;; If one of the variations of the enode is a single variable or
    ;; constant, reduce to that.
    (reduce-to-single! eg en)
    ;; If one of the variations of the enode chains back to itself,
    ;; prune it away. Loops in the egraph coorespond to identity
    ;; functions.
    #;(elim-enode-loops! eg en))

  (for ([m (find-matches (egraph-leaders eg))])
    (apply-match m))
  (map-enodes (curry set-precompute! eg) eg)
  (void))

(define-syntax-rule (matches? expr pattern)
  (match expr
    [pattern #t]
    [_ #f]))

(define (exact-value? type val)
  (match type
    ['real (exact? val)]
    ['complex (exact? val)]
    ['boolean true]))

(define/match (val-of-type type val)
  [('real    (? real?))    true]
  [('complex (? complex?)) true]
  [('boolean (? boolean?)) true]
  [(_ _) false])

(define (val-to-type type val)
  (match type
    ['real val]
    ['complex `(complex ,(real-part val) ,(imag-part val))]
    ['boolean (if val 'TRUE 'FALSE)]))

(define (set-precompute! eg en)
  (define type (enode-type en))
  (for ([var (enode-vars en)])
    (when (list? var)
      (let ([constexpr
	     (cons (car var)
		   (map (compose (curry setfindf constant?) enode-vars)
			(cdr var)))])
	(when (and (not (matches? constexpr `(/ ,a 0)))
		   (not (matches? constexpr `(log 0)))
		   (not (matches? constexpr `(/ 0)))
		   (andmap real? (cdr constexpr)))
	  (let ([res (eval-const-expr constexpr)])
	    (when (and (val-of-type type res) (exact-value? type res))
	      (reduce-to-new! eg en (val-to-type type res)))))))))

(define (hash-set*+ hash assocs)
  (for/fold ([h hash]) ([assoc assocs])
    (hash-set h (car assoc) (cdr assoc))))

(define (extract-smallest eg en)
  ;; The work list maps enodes to a pair (cost . expr) of that node's
  ;; cheapest representation and its cost. If the cost is #f, the expr
  ;; is also #f, and in this case no expression is yet known for that
  ;; enode.
  (define work-list (make-hash))
  (hash-set! work-list (pack-leader en) (cons #f #f))

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
        (cdr (hash-ref work-list (pack-leader en))))))

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
