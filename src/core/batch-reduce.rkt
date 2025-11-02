#lang racket

(require "batch.rkt"
         "../utils/common.rkt"
         "programs.rkt")

(provide batch-reduce)

(define global-batch (make-parameter #f))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (batch-eval-application batch)
  (define exact-value? (conjoin number? exact?))
  (define (eval-application brf recurse)
    (match (deref brf)
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
  (batch-recurse batch eval-application))

(define (batch-reduce batch)
  ;; Dependencies
  (define eval-application (batch-eval-application batch))
  (define gather-multiplicative-terms (batch-gather-multiplicative-terms batch eval-application))

  (letrec ([reduce-node
            (batch-recurse
             batch
             (lambda (brf recurse)
               (define brf* (reduce-evaluation brf))
               (match (deref brf*)
                 [(? number?) brf*]
                 [(? symbol?) brf*]
                 [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
                  (make-addition-node (combine-aterms (gather-additive-terms brf*)))]
                 [(or `(* ,_ ...)
                      `(/ ,_ ...)
                      `(cbrt ,_)
                      `(pow ,_ ,(app deref (? (conjoin rational? (negate even-denominator?))))))
                  (make-multiplication-node (combine-mterms (gather-multiplicative-terms brf*)))]
                 [(list 'exp (app deref (list '* c (app deref (list 'log x)))))
                  (define rewrite (batch-add! batch `(pow ,x ,c)))
                  (recurse rewrite)]
                 [else (reduce-inverses brf*)])))]
           [gather-additive-terms
            (batch-recurse
             batch
             (lambda (brf recurse)
               (match (deref brf)
                 [(? number? n) `((,n ,(batch-push! batch 1)))]
                 [(? symbol?) `((1 ,brf))]
                 [`(+ ,args ...) (append-map recurse args)]
                 [`(neg ,arg) (map negate-term (recurse arg))]
                 [`(- ,arg ,args ...)
                  (append (recurse arg) (map negate-term (append-map recurse args)))]
                 ; Prevent fall-through to the next case
                 [`(/ ,arg) `((1 ,brf))]
                 [`(/ ,arg ,args ...)
                  (for/list ([term (recurse arg)])
                    (list (car term) (reduce-node (batch-add! batch (list* '/ (cadr term) args)))))]
                 [else `((1 ,brf))])))])

    ;; Actual code
    (define (reduce brf recurse)
      (parameterize ([global-batch batch])
        (define node (deref brf))
        (match node
          [(? number?) brf]
          [(? symbol?) brf]
          [`(,op ,args ...)
           (define args* (map recurse args))
           (define brf* (batch-add! batch (list* op args*)))
           (define val (eval-application brf*))
           (when val ;; convert to batchref if result is not #f
             (set! val (batch-push! batch val)))
           (or val (reduce-node brf*))])))
    (batch-recurse batch reduce)))

(define (reduce-evaluation brf)
  (define batch (batchref-batch brf))
  (define node*
    (match (deref brf)
      [(list 'sin (app deref 0)) 0]
      [(list 'cos (app deref 0)) 1]
      [(list 'sin (app deref (list 'PI))) 0]
      [(list 'cos (app deref (list 'PI))) -1]
      [(list 'exp (app deref 1)) '(E)]
      [(list 'tan (app deref 0)) 0]
      [(list 'sinh (app deref 0)) 0]
      [(list 'log (app deref (list 'E))) 1]
      [(list 'exp (app deref 0)) 1]
      [(list 'tan (app deref (list 'PI))) 0]
      [(list 'cosh (app deref 0)) 1]
      [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 6)))) '(/ (sqrt 3) 2)]
      [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 3)))) '(sqrt 3)]
      [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 4)))) 1]
      [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 2)))) 0]
      [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 6)))) '(/ 1 (sqrt 3))]
      [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 3)))) '(/ (sqrt 3) 2)]
      [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 6)))) 1/2]
      [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 4)))) '(/ (sqrt 2) 2)]
      [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 2)))) 1]
      [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 3)))) 1/2]
      [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 4)))) '(/ (sqrt 2) 2)]
      [node node]))
  (batch-add! batch node*))

(define (reduce-inverses brf)
  (match (deref brf)
    [(list 'tanh (app deref (list 'atanh x))) x]
    [(list 'cosh (app deref (list 'acosh x))) x]
    [(list 'sinh (app deref (list 'asinh x))) x]
    [(list 'acos (app deref (list 'cos x))) x]
    [(list 'asin (app deref (list 'sin x))) x]
    [(list 'atan (app deref (list 'tan x))) x]
    [(list 'tan (app deref (list 'atan x))) x]
    [(list 'cos (app deref (list 'acos x))) x]
    [(list 'sin (app deref (list 'asin x))) x]
    [(list 'pow x (app deref 1)) x]
    [(list 'log (app deref (list 'exp x))) x]
    [(list 'exp (app deref (list 'log x))) x]
    [(list 'cbrt (app deref (list 'pow x (app deref 3)))) x]
    [(list 'pow (app deref (list 'cbrt x)) (app deref 3)) x]
    [_ brf]))

(define (negate-term term)
  (cons (- (car term)) (cdr term)))

(define (even-denominator? x)
  (even? (denominator x)))

(define (batch-gather-multiplicative-terms batch eval-application)
  (define (nan-term)
    `(+nan.0 . ((1 . ,(batch-push! batch 1)))))
  (define (gather-multiplicative-terms brf recurse)
    (match (deref brf)
      [+nan.0 (nan-term)]
      [(? number? n) (list n)]
      [(? symbol?) `(1 . ((1 . ,brf)))]
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
          (define exact-cbrt (eval-application (batch-add! batch (list 'cbrt (car terms)))))
          (if exact-cbrt
              (cons exact-cbrt
                    (for/list ([term (cdr terms)])
                      (cons (/ (car term) 3) (cdr term))))
              (list* 1
                     (cons 1 (batch-add! batch `(cbrt ,(car terms))))
                     (for/list ([term (cdr terms)])
                       (cons (/ (car term) 3) (cdr term)))))])]
      [`(pow ,arg ,(app deref 0))
       (define terms (recurse arg))
       (if (equal? (car terms) +nan.0)
           (nan-term)
           `(1 . ()))]
      [`(pow ,arg ,(app deref (? (conjoin rational? (negate even-denominator?)) a)))
       (define terms (recurse arg))
       (define exact-pow
         (match (car terms)
           [+nan.0 +nan.0]
           [x (eval-application (batch-add! batch (list 'pow x a)))]))
       (if exact-pow
           (cons exact-pow
                 (for/list ([term (cdr terms)])
                   (cons (* a (car term)) (cdr term))))
           (list* 1
                  (cons a (batch-push! batch (car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (* a (car term)) (cdr term)))))]
      [_ `(1 . ((1 . ,brf)))]))
  (batch-recurse batch gather-multiplicative-terms))

(define (combine-aterms terms)
  (define h (make-hash))
  (for ([term terms])
    (define sum (hash-ref! h (cadr term) 0))
    (hash-set! h (cadr term) (+ (car term) sum)))
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
            (define sum (hash-ref! h (cdr term) 0))
            (hash-set! h (cdr term) (+ (car term) sum)))
          (sort (reap [sow]
                      (for ([(k v) (in-hash h)]
                            #:unless (= v 0))
                        (sow (cons v k))))
                expr<?
                #:key cdr))))

(define (aterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(,x . ,(app deref 1)) (batch-push! (global-batch) x)]
    [`(-1 . ,x) (batch-add! (global-batch) `(neg ,x))]
    [`(,coeff . ,x) (batch-add! (global-batch) `(* ,coeff ,x))]))

(define (make-addition-node terms)
  (define-values (pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms))
  (cond
    [(and (null? pos) (null? neg)) (batch-push! (global-batch) 0)]
    [(null? pos) (batch-add! (global-batch) `(neg ,(make-addition-node* (map negate-term neg))))]
    [(null? neg) (make-addition-node* pos)]
    [else
     (batch-add! (global-batch)
                 `(- ,(make-addition-node* pos) ,(make-addition-node* (map negate-term neg))))]))

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node* terms)
  (match terms
    ['() (batch-push! (global-batch) 0)]
    [`(,term) (aterm->expr term)]
    [`(,term ,terms ...)
     (batch-add! (global-batch) `(+ ,(aterm->expr term) ,(make-addition-node terms)))]))

(define (make-multiplication-node term)
  (match (cons (car term) (make-multiplication-subnode (cdr term)))
    [(cons +nan.0 e) (batch-push! (global-batch) '(NAN))]
    [(cons 0 e) (batch-push! (global-batch) 0)]
    [(cons 1 '()) (batch-push! (global-batch) 1)]
    [(cons 1 e) e]
    [(cons a (app deref 1)) (batch-push! (global-batch) a)]
    [(cons a (app deref (list '/ (app deref 1) denom))) (batch-add! (global-batch) `(/ ,a ,denom))]
    [(cons a '()) (batch-push! (global-batch) a)]
    [(cons a e) (batch-add! (global-batch) `(* ,a ,e))]))

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (list (cons 1 (mterm->expr (cons 1 (make-multiplication-subsubnode terms)))))))

(define (make-multiplication-subsubnode terms)
  (define-values (pos neg) (partition (compose positive? car) terms))
  (cond
    [(and (null? pos) (null? neg)) (batch-push! (global-batch) 1)]
    [(null? pos)
     (batch-add! (global-batch) `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg))))]
    [(null? neg) (make-multiplication-subsubsubnode pos)]
    [else
     (batch-add! (global-batch)
                 `(/ ,(make-multiplication-subsubsubnode pos)
                     ,(make-multiplication-subsubsubnode (map negate-term neg))))]))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() (batch-push! (global-batch) 1)]
    [`(,term) (mterm->expr term)]
    [`(,term ,terms ...)
     (batch-add! (global-batch)
                 `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms)))]))

(define (mterm->expr term)
  (match term
    [(cons 1 x) x]
    [(cons -1 x) (batch-add! (global-batch) `(/ 1 ,x))]
    [(cons 1/2 x) (batch-add! (global-batch) `(sqrt ,x))]
    [(cons -1/2 x) (batch-add! (global-batch) `(/ 1 (sqrt ,x)))]
    [(cons 1/3 x) (batch-add! (global-batch) `(cbrt ,x))]
    [(cons -1/3 x) (batch-add! (global-batch) `(/ 1 (cbrt ,x)))]
    [(cons power x) (batch-add! (global-batch) `(pow ,x ,power))]))

(module+ test
  (require rackunit)
  (define batch (batch-empty))
  (define evaluator (batch-eval-application batch))
  (define (evaluator-results expr)
    (evaluator (batch-add! batch expr)))

  ;; Checks for batch-eval-application
  (check-equal? (evaluator-results '(+ 1 1)) 2)
  (check-equal? (evaluator-results '(+)) 0)
  (check-equal? (evaluator-results '(/ 1 0)) #f) ; Not valid
  (check-equal? (evaluator-results '(cbrt 1)) 1)
  (check-equal? (evaluator-results '(log 1)) 0)
  (check-equal? (evaluator-results '(exp 2)) #f) ; Not exact

  ;; Checks for batch-reduce-evaluation
  (define (reducer-results expr)
    (batch-pull (reduce-evaluation (batch-add! batch expr))))
  (check-equal? (reducer-results '(cos (/ (PI) 6))) '(/ (sqrt 3) 2))
  (check-equal? (reducer-results '(sin (/ (PI) 4))) '(/ (sqrt 2) 2))
  (check-equal? (reducer-results '(cos (PI))) -1)
  (check-equal? (reducer-results '(exp 1)) '(E))

  ;; Checks for batch-reduce-inverses
  (define (inverse-reducer-results expr)
    (batch-pull (reduce-inverses (batch-add! batch expr))))
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

  ;; Checks for batch-reduce
  (define reduce (batch-reduce batch))
  (define (reduce-results expr)
    (batch-pull (reduce (batch-add! batch expr))))
  (check-equal? '(- (pow (+ 1 x) 2) 1) (reduce-results '(- (* (+ x 1) (+ x 1)) 1)))
  (check-equal? '(neg (* 2 (/ 1 x))) (reduce-results '(+ (/ 1 (neg x)) (/ 1 (neg x)))))
  (check-equal? '(- (pow (- 1 (/ 1 x)) 2) 1)
                (reduce-results '(- (* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) 1)))
  (check-equal? '(pow (- 1 (/ 1 x)) 2) (reduce-results '(* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1))))
  (check-equal? '(+ (* 2 (/ 1 x)) (/ 1 (pow x 2)))
                (reduce-results '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x)))))
  (check-equal? '(+ (* 2 (/ 1 x)) (/ 1 (pow x 2)))
                (reduce-results '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x)))))
  (check-equal? '(/ 1 (* (cbrt 2) (cbrt a))) (reduce-results '(pow (+ a a) -1/3))))
