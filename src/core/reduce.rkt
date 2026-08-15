#lang racket

(require "../syntax/types.rkt"
         "../syntax/block.rkt"
         "../utils/common.rkt"
         "programs.rkt")

(provide block-reduce)

(define global-block (make-parameter #f))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (block-eval-application block)
  (define exact-value? (conjoin number? exact?))
  (define (eval-application v recurse)
    (match (val-def v)
      [(? exact-value? val)
       val] ;; this part is not naive in rewriting. should be considered for the future
      [(list '+ (app recurse (? exact-value? as)) ...) (apply + as)]
      [(list '- (app recurse (? exact-value? as)) ...) (apply - as)]
      [(list '* (app recurse (? exact-value? as)) ...) (apply * as)]
      [(list '/ (app recurse (? exact-value? num)) (app recurse (? exact-value? den)))
       (and (not (zero? den)) (/ num den))]
      [(list 'neg (app recurse (? exact-value? arg))) (- arg)]
      [(list 'pow (app recurse (? exact-value? a)) (app recurse (? exact-value? b)))
       (cond
         [(and (zero? b) (not (zero? a))) 1]
         [(and (zero? a) (positive? b)) 0]
         [(and (not (zero? a)) (integer? b)) (expt a b)]
         [(= a -1) (if (even? (numerator b)) 1 -1)]
         [(= a 1) 1]
         [else #f])]
      [(list 'sqrt (app recurse (? exact-value? a)))
       (define s1 (sqrt (numerator a)))
       (define s2 (sqrt (denominator a)))
       (and (real? s1) (real? s2) (exact? s1) (exact? s2) (/ s1 s2))]
      [(list 'cbrt (app recurse (? exact-value? a)))
       (define inexact-num (inexact->exact (expt (abs (numerator a)) 1/3)))
       (define inexact-den (inexact->exact (expt (abs (denominator a)) 1/3)))
       (and (real? inexact-num)
            (real? inexact-den)
            (= (expt inexact-num 3) (abs (numerator a)))
            (= (expt inexact-den 3) (abs (denominator a)))
            (* (sgn a) (/ inexact-num inexact-den)))]
      [(list 'fabs (app recurse (? exact-value? a))) (abs a)]
      [(list 'floor (app recurse (? exact-value? a))) (floor a)]
      [(list 'ceil (app recurse (? exact-value? a))) (ceiling a)]
      [(list 'round (app recurse (? exact-value? a))) (round a)]
      [(list 'exp (app recurse 0)) 1]
      [(list 'log (app recurse 1)) 0]
      [_ #f]))
  (block-recurse block eval-application))

(define (block-reduce block)
  ;; Dependencies
  (define eval-application (block-eval-application block))
  (define gather-multiplicative-terms (block-gather-multiplicative-terms block eval-application))

  (letrec ([reduce-node
            (block-recurse
             block
             (lambda (v recurse)
               (define v* (reduce-evaluation v))
               (match (val-def v*)
                 [(? number?) v*]
                 [(? symbol?) v*]
                 [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
                  (make-addition-node (combine-aterms (gather-additive-terms v*)))]
                 [(or `(* ,_ ...)
                      `(/ ,_ ...)
                      `(cbrt ,_)
                      `(pow ,_ ,(app val-def (? (conjoin rational? (negate even-denominator?))))))
                  (make-multiplication-node (combine-mterms (gather-multiplicative-terms v*)))]
                 [(list 'exp (app val-def (list '* c (app val-def (list 'log x)))))
                  (define rewrite (block-add! block `(pow ,x ,c)))
                  (recurse rewrite)]
                 [else (reduce-inverses v*)])))]
           [gather-additive-terms
            (block-recurse
             block
             (lambda (v recurse)
               (match (val-def v)
                 [(? number? n) `((,n ,(block-push! block 1)))]
                 [(? symbol?) `((1 ,v))]
                 [`(+ ,args ...) (append-map recurse args)]
                 [`(neg ,arg) (map negate-term (recurse arg))]
                 [`(- ,arg ,args ...)
                  (append (recurse arg) (map negate-term (append-map recurse args)))]
                 ; Prevent fall-through to the next case
                 [`(/ ,arg) `((1 ,v))]
                 [`(/ ,arg ,args ...)
                  (for/list ([term (recurse arg)])
                    (list (car term) (reduce-node (block-add! block (list* '/ (cadr term) args)))))]
                 [else `((1 ,v))])))])

    ;; Actual code
    (define (reduce v recurse)
      (parameterize ([global-block block])
        (define node (val-def v))
        (match node
          [(? number?) v]
          [(? symbol?) v]
          [`(,op ,args ...)
           (define args* (map recurse args))
           (define v* (block-add! block (list* op args*)))
           (define val (eval-application v*))
           (when val ;; convert to val if result is not #f
             (set! val (block-push! block val)))
           (or val (reduce-node v*))])))
    (block-recurse block reduce)))

(define (reduce-evaluation v)
  (define block (val-block v))
  (define (pi-multiple expr)
    (match expr
      [`(PI) 1]
      [`(* ,(app val-def (? rational? coeff)) ,(app val-def '(PI))) coeff]
      [`(* ,(app val-def '(PI)) ,(app val-def (? rational? coeff))) coeff]
      [`(/ ,(app val-def '(PI)) ,(app val-def (? rational? denom))) (/ denom)]
      [_ #f]))
  (define node*
    (match (val-def v)
      [(list 'sin (app val-def 0)) 0]
      [(list 'cos (app val-def 0)) 1]
      [(list 'sin (app val-def (app pi-multiple 1))) 0]
      [(list 'cos (app val-def (app pi-multiple 1))) -1]
      [(list 'exp (app val-def 1)) '(E)]
      [(list 'tan (app val-def 0)) 0]
      [(list 'sinh (app val-def 0)) 0]
      [(list 'log (app val-def (list 'E))) 1]
      [(list 'exp (app val-def 0)) 1]
      [(list 'tan (app val-def (app pi-multiple 1))) 0]
      [(list 'cosh (app val-def 0)) 1]
      [(list 'cos (app val-def (app pi-multiple 1/6))) '(/ (sqrt 3) 2)]
      [(list 'tan (app val-def (app pi-multiple 1/3))) '(sqrt 3)]
      [(list 'tan (app val-def (app pi-multiple 1/4))) 1]
      [(list 'cos (app val-def (app pi-multiple 1/2))) 0]
      [(list 'tan (app val-def (app pi-multiple 1/6))) '(/ 1 (sqrt 3))]
      [(list 'sin (app val-def (app pi-multiple 1/3))) '(/ (sqrt 3) 2)]
      [(list 'sin (app val-def (app pi-multiple 1/6))) 1/2]
      [(list 'sin (app val-def (app pi-multiple 1/4))) '(/ (sqrt 2) 2)]
      [(list 'sin (app val-def (app pi-multiple 1/2))) 1]
      [(list 'cos (app val-def (app pi-multiple 1/3))) 1/2]
      [(list 'cos (app val-def (app pi-multiple 1/4))) '(/ (sqrt 2) 2)]
      [node node]))
  (block-add! block node*))

(define (reduce-inverses v)
  (match (val-def v)
    [(list 'tanh (app val-def (list 'atanh x))) x]
    [(list 'cosh (app val-def (list 'acosh x))) x]
    [(list 'sinh (app val-def (list 'asinh x))) x]
    [(list 'acos (app val-def (list 'cos x))) x]
    [(list 'asin (app val-def (list 'sin x))) x]
    [(list 'atan (app val-def (list 'tan x))) x]
    [(list 'tan (app val-def (list 'atan x))) x]
    [(list 'cos (app val-def (list 'acos x))) x]
    [(list 'sin (app val-def (list 'asin x))) x]
    [(list 'pow x (app val-def 1)) x]
    [(list 'log (app val-def (list 'exp x))) x]
    [(list 'exp (app val-def (list 'log x))) x]
    [(list 'cbrt (app val-def (list 'pow x (app val-def 3)))) x]
    [(list 'pow (app val-def (list 'cbrt x)) (app val-def 3)) x]
    [_ v]))

(define (negate-term term)
  (cons (- (car term)) (cdr term)))

(define (even-denominator? x)
  (even? (denominator x)))

(define (block-gather-multiplicative-terms block eval-application)
  (define (nan-term)
    `(+nan.0 . ((1 . ,(block-push! block 1)))))
  (define (gather-multiplicative-terms v recurse)
    (match (val-def v)
      [+nan.0 (nan-term)]
      [(? number? n) (list n)]
      [(? symbol?) `(1 . ((1 . ,v)))]
      [`(neg ,arg)
       (define terms (recurse arg))
       (if (eq? (car terms) +nan.0)
           (nan-term)
           (negate-term terms))]
      [`(* ,args ...)
       (define terms (map recurse args))
       (if (ormap (curry eq? +nan.0) (map car terms))
           (nan-term)
           (cons (apply * (map car terms)) (append-map cdr terms)))]
      [`(/ ,arg)
       (define term (recurse arg))
       (if (member (car term) '(0 +nan.0))
           (nan-term)
           (cons (/ (car term)) (map negate-term (cdr term))))]
      [`(/ ,arg ,args ...)
       (define num (recurse arg))
       (define dens (map recurse args))
       (if (or (eq? (car num) +nan.0) (ormap (compose (curryr member '(0 +nan.0)) car) dens))
           (nan-term)
           (cons (apply / (car num) (map car dens))
                 (append (cdr num) (map negate-term (append-map cdr dens)))))]
      [`(cbrt ,arg)
       (define terms (recurse arg))
       (cond
         [(equal? (car terms) +nan.0) (nan-term)]
         [else
          (define exact-cbrt (eval-application (block-add! block (list 'cbrt (car terms)))))
          (if exact-cbrt
              (cons exact-cbrt
                    (for/list ([term (cdr terms)])
                      (cons (/ (car term) 3) (cdr term))))
              (list* 1
                     (cons 1 (block-add! block `(cbrt ,(car terms))))
                     (for/list ([term (cdr terms)])
                       (cons (/ (car term) 3) (cdr term)))))])]
      [`(pow ,arg ,(app val-def 0))
       (define terms (recurse arg))
       (if (equal? (car terms) +nan.0)
           (nan-term)
           `(1 . ()))]
      [`(pow ,arg ,(app val-def (? (conjoin rational? (negate even-denominator?)) a)))
       (define terms (recurse arg))
       (define exact-pow
         (match (car terms)
           [+nan.0 +nan.0]
           [x (eval-application (block-add! block (list 'pow x a)))]))
       (if exact-pow
           (cons exact-pow
                 (for/list ([term (cdr terms)])
                   (cons (* a (car term)) (cdr term))))
           (list* 1
                  (cons a (block-push! block (car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (* a (car term)) (cdr term)))))]
      [_ `(1 . ((1 . ,v)))]))
  (block-recurse block gather-multiplicative-terms))

(define (combine-aterms terms)
  (define h (make-hash))
  (for ([term terms])
    (hash-update! h (cadr term) (λ (sum) (+ (car term) sum)) 0))
  (sort (reap [sow]
              (for ([(k v) (in-hash h)]
                    #:when (not (= v 0)))
                (sow (cons v k))))
        expr<?
        #:key cdr))

(define (combine-mterms terms)
  (cons (car terms)
        (let ([h (make-hash)])
          (for ([term (cdr terms)])
            (hash-update! h (cdr term) (λ (sum) (+ (car term) sum)) 0))
          (sort (reap [sow]
                      (for ([(k v) (in-hash h)]
                            #:unless (= v 0))
                        (sow (cons v k))))
                expr<?
                #:key cdr))))

(define (aterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(,x . ,(app val-def 1)) (block-push! (global-block) x)]
    [`(-1 . ,x) (block-add! (global-block) `(neg ,x))]
    [`(,coeff . ,x) (block-add! (global-block) `(* ,coeff ,x))]))

(define (make-addition-node terms)
  (define-values (pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms))
  (cond
    [(and (null? pos) (null? neg)) (block-push! (global-block) 0)]
    [(null? pos) (block-add! (global-block) `(neg ,(make-addition-node* (map negate-term neg))))]
    [(null? neg) (make-addition-node* pos)]
    [else
     (block-add! (global-block)
                 `(- ,(make-addition-node* pos) ,(make-addition-node* (map negate-term neg))))]))

(define (make-addition-node* terms)
  (match terms
    ['() (block-push! (global-block) 0)]
    [`(,term) (aterm->expr term)]
    [`(,term ,terms ...)
     (block-add! (global-block) `(+ ,(aterm->expr term) ,(make-addition-node terms)))]))

(define (make-multiplication-node term)
  (match (cons (car term) (make-multiplication-subnode (cdr term)))
    [(cons +nan.0 e) (block-push! (global-block) '(NAN))]
    [(cons 0 e) (block-push! (global-block) 0)]
    [(cons 1 '()) (block-push! (global-block) 1)]
    [(cons 1 e) e]
    [(cons a (app val-def 1)) (block-push! (global-block) a)]
    [(cons a (app val-def (list '/ (app val-def 1) denom)))
     (block-add! (global-block) `(/ ,a ,denom))]
    [(cons a '()) (block-push! (global-block) a)]
    [(cons a e) (block-add! (global-block) `(* ,a ,e))]))

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (list (cons 1 (mterm->expr (cons 1 (make-multiplication-subsubnode terms)))))))

(define (make-multiplication-subsubnode terms)
  (define-values (pos neg) (partition (compose positive? car) terms))
  (cond
    [(and (null? pos) (null? neg)) (block-push! (global-block) 1)]
    [(null? pos)
     (block-add! (global-block) `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg))))]
    [(null? neg) (make-multiplication-subsubsubnode pos)]
    [else
     (block-add! (global-block)
                 `(/ ,(make-multiplication-subsubsubnode pos)
                     ,(make-multiplication-subsubsubnode (map negate-term neg))))]))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() (block-push! (global-block) 1)]
    [`(,term) (mterm->expr term)]
    [`(,term ,terms ...)
     (block-add! (global-block)
                 `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms)))]))

(define (mterm->expr term)
  (match term
    [(cons (? exact-integer? power) x)
     (cond
       [(zero? power) (block-push! (global-block) 1)]
       [(= power 1) x]
       [(negative? power) (block-add! (global-block) `(/ 1 ,(mterm->expr (cons (- power) x))))]
       [(even? power)
        (define factor (mterm->expr (cons (/ power 2) x)))
        (block-add! (global-block) `(* ,factor ,factor))]
       [else (block-add! (global-block) `(* ,x ,(mterm->expr (cons (sub1 power) x))))])]
    [(cons (? rational? power) x)
     (match (denominator power)
       [2 (mterm->expr (cons (numerator power) (block-add! (global-block) `(sqrt ,x))))]
       [3 (mterm->expr (cons (numerator power) (block-add! (global-block) `(cbrt ,x))))]
       [_ (block-add! (global-block) `(pow ,x ,power))])]
    [(cons power x) (block-add! (global-block) `(pow ,x ,power))]))

(module+ test
  (require rackunit)
  (define block (block-empty (context '() #f '())))
  (define evaluator (block-eval-application block))
  (define (evaluator-results expr)
    (evaluator (block-add! block expr)))

  ;; Checks for block-eval-application
  (check-equal? (evaluator-results '(+ 1 1)) 2)
  (check-equal? (evaluator-results '(+)) 0)
  (check-equal? (evaluator-results '(/ 1 0)) #f) ; Not valid
  (check-equal? (evaluator-results '(cbrt 1)) 1)
  (check-equal? (evaluator-results '(log 1)) 0)
  (check-equal? (evaluator-results '(exp 2)) #f) ; Not exact

  ;; Checks for block-reduce-evaluation
  (define (reducer-results expr)
    ((block-exprs block) (reduce-evaluation (block-add! block expr))))
  (check-equal? (reducer-results '(cos (/ (PI) 6))) '(/ (sqrt 3) 2))
  (check-equal? (reducer-results '(sin (/ (PI) 4))) '(/ (sqrt 2) 2))
  (check-equal? (reducer-results '(cos (PI))) -1)
  (check-equal? (reducer-results '(exp 1)) '(E))

  ;; Checks for block-reduce-inverses
  (define (inverse-reducer-results expr)
    ((block-exprs block) (reduce-inverses (block-add! block expr))))
  (check-equal? (inverse-reducer-results '(cosh (acosh x))) 'x)
  (check-equal? (inverse-reducer-results '(tanh (atanh x))) 'x)
  (check-equal? (inverse-reducer-results '(sinh (asinh x))) 'x)
  (check-equal? (inverse-reducer-results '(acos (cos x))) 'x)
  (check-equal? (inverse-reducer-results '(asin (sin x))) 'x)
  (check-equal? (inverse-reducer-results '(asin (sin x))) 'x)
  (check-equal? (inverse-reducer-results '(atan (tan x))) 'x)
  (check-equal? (inverse-reducer-results '(tan (atan x))) 'x)
  (check-equal? (inverse-reducer-results '(cos (acos x))) 'x)
  (check-equal? (inverse-reducer-results '(sin (asin x))) 'x)
  (check-equal? (inverse-reducer-results '(pow x 1)) 'x)
  (check-equal? (inverse-reducer-results '(log (exp x))) 'x)
  (check-equal? (inverse-reducer-results '(exp (log x))) 'x)
  (check-equal? (inverse-reducer-results '(cbrt (pow x 3))) 'x)
  (check-equal? (inverse-reducer-results '(pow (cbrt x) 3)) 'x)

  ;; Checks for block-reduce
  (define reduce (block-reduce block))
  (define (reduce-results expr)
    ((block-exprs block) (reduce (block-add! block expr))))
  (check-equal? '(- (* (+ 1 x) (+ 1 x)) 1) (reduce-results '(- (* (+ x 1) (+ x 1)) 1)))
  (check-equal? '(neg (* 2 (/ 1 x))) (reduce-results '(+ (/ 1 (neg x)) (/ 1 (neg x)))))
  (check-equal? '(- (* (- 1 (/ 1 x)) (- 1 (/ 1 x))) 1)
                (reduce-results '(- (* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) 1)))
  (check-equal? '(* (- 1 (/ 1 x)) (- 1 (/ 1 x)))
                (reduce-results '(* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1))))
  (check-equal? '(+ (* 2 (/ 1 x)) (/ 1 (* x x)))
                (reduce-results '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x)))))
  (check-equal? '(+ (* 2 (/ 1 x)) (/ 1 (* x x)))
                (reduce-results '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x)))))
  (check-equal? '(* (cbrt x) (* (* (cbrt x) (cbrt x)) (* (cbrt x) (cbrt x))))
                (reduce-results '(* x (cbrt x) (cbrt x))))
  (check-equal? '(/ 1 (* (cbrt x) (* (* (cbrt x) (cbrt x)) (* (cbrt x) (cbrt x)))))
                (reduce-results '(/ 1 (* x (cbrt x) (cbrt x)))))
  (check-equal? '(/ 1 (* (cbrt 2) (cbrt a))) (reduce-results '(pow (+ a a) -1/3))))
