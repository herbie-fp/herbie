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
  (equal? (rule-otype rl) (enode-type en)))

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
               #:when (rule-applicable? rl en))
          (define bindings (match-e (rule-input rl) en))
          (unless (null? bindings)
            (sow (list* rl en bindings))))))

;; Iterates the egraph by applying each of the given rules in parallel
;; to the egraph nodes.
(define (one-iter eg rls)
  (define initial-cnt (egraph-cnt eg))
  (for ([m (find-matches (egraph-leaders eg) rls)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (match-define (list rl en bindings ...) m)
    (for ([binding bindings] #:break (>= (egraph-cnt eg) (*node-limit*)))
      (merge-egraph-nodes! eg en (substitute-e eg (rule-output rl) binding))))
  (for ([en (egraph-leaders eg)]
        #:break (>= (egraph-cnt eg) (*node-limit*)))
    (set-precompute! eg en))
  (for ([en (egraph-leaders eg)] #:break (>= (egraph-cnt eg) (*node-limit*)))
    (reduce-to-single! eg en))
  (< initial-cnt (egraph-cnt eg) (*node-limit*)))

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
                            (cons cost best-arg)]
                           [(cons #f not-in-hash?)
                            (hash-set! work-list subleader (cons #f #f))
                            (set! changed? (or changed? not-in-hash?))
                            #f])))
                     (if (andmap identity args*)
                         (cons (apply + (operator-info op 'cost) (map car args*))
                               (cons op (map cdr args*)))
                         #f)]
                    [_
                     (cons 1 var)]))))
      (match vars*
        ['() #f]
        [_
         (define best-resolution (argmin car vars*))
         (define cost (car best-resolution))
         (define old-cost (car (hash-ref work-list leader)))
         (when (or (not old-cost) (< cost old-cost))
           (hash-set! work-list leader best-resolution)
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

  (*timeline-disabled* true)
  (define outputs (simplify-batch (hash-keys test-exprs) #:rules (*simplify-rules*)))
  (for ([(original target) test-exprs] [output outputs])
    (with-check-info (['original original])
       (check-equal? output target)))

  (check set-member?
         '((* x 6) (* 6 x))
         (simplify-expr '(+ (+ (+ (+ (+ x x) x) x) x) x) #:rules (*simplify-rules*)))

  (define no-crash-exprs
    '((exp (/ (/ (* (* c a) 4) (- (- b) (sqrt (- (* b b) (* 4 (* a c)))))) (* 2 a)))))

  (for ([expr no-crash-exprs])
    (with-check-info (['original expr])
       (check-not-exn (λ () (simplify-expr expr #:rules (*simplify-rules*)))))))
