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

  (define start-time (current-inexact-milliseconds))
  (define eg (mk-egraph))
  (define ens (for/list ([expr exprs]) (mk-enode-rec! eg expr)))
  (define ex (apply mk-extractor ens))

  (for/and ([iter (in-naturals 0)])
    (extractor-iterate ex)
    (define cost (apply + (map car (apply extractor-extract ex ens))))
    (debug #:from 'simplify #:depth 2 "iteration " iter ": " (egraph-cnt eg) " enodes " "(cost " cost ")")
    (timeline-push! 'egraph iter (egraph-cnt eg) cost (- (current-inexact-milliseconds) start-time))
    (one-iter eg rls))
  (extractor-iterate ex)
  (define cost (apply + (map car (apply extractor-extract ex ens))))
  (debug #:from 'simplify #:depth 2
         "iteration done: " (egraph-cnt eg) " enodes " "(cost " cost ")")
  (timeline-push! 'egraph "done" (egraph-cnt eg) cost (- (current-inexact-milliseconds) start-time))

  (define out (map cdr (apply extractor-extract ex ens)))
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
  

;; The work list maps enodes to a pair (cost . expr) of that node's
;; cheapest representation and its cost. If the cost is #f, the expr
;; is also #f, and in this case no expression is yet known for that
;; enode.

(define (mk-extractor . ens)
  (define work-list (make-hash))
  (for ([en ens])
    (hash-set! work-list (pack-leader en) (cons #f #f)))
  work-list)

(define (extractor-cost work-list)
  (for/fold ([infs 0] [cost 0]) ([val (in-hash-values work-list)])
    (if (not (car val))
        (values (+ 1 infs) cost)
        (values infs (+ (car val) cost)))))

;; Extracting the smallest expression means iterating, until
;; fixedpoint, either discovering new relevant expressions or
;; cheaper expressions for some expression.
(define (extractor-iterate work-list)
  (let loop ([iter 0])
    (define changed? #f)
    (define-values (infs cost) (extractor-cost work-list))
    (debug #:from 'simplify #:depth 2 "Extracting #" iter ": cost " infs " inf + " cost)
    (for ([en (in-list (hash-keys work-list))]) ;; in-list to avoid mutating the iterator
      (define leader (pack-leader en))
      (when (not (eq? en leader))
        (hash-set! work-list leader (hash-ref work-list en))
        (hash-remove! work-list en))
      (define vars (enode-vars leader))
      (define vars*
        (filter identity
                (for/list ([var vars])
                  (match var
                    [(list op args ...)
                     (define args*
                       (for/list ([suben args])
                         (define subleader (pack-leader suben))
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
    (when changed?
      (loop (+ iter 1)))))

(define (extractor-extract work-list . ens)
  (for/list ([en ens])
    (hash-ref work-list (pack-leader en))))

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
