#lang racket

(require "../utils/common.rkt"
         "../syntax/syntax.rkt"
         "programs.rkt")
(provide reduce)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define/reset reduce-cache (make-hash))

(define/reset reduce-node-cache (make-hash))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (eval-application op . args)
  (define exact-value? (conjoin number? exact?))
  (match (cons op args)
    [(list '+ (? exact-value? as) ...) (apply + as)]
    [(list '- (? exact-value? as) ...) (apply - as)]
    [(list '* (? exact-value? as) ...) (apply * as)]
    [(list '/ (? exact-value? num) (? exact-value? den)) (and (not (zero? den)) (/ num den))]
    [(list 'neg (? exact-value? arg)) (- arg)]
    [(list 'pow (? exact-value? a) (? exact-value? b))
     (cond
       [(and (zero? b) (not (zero? a))) 1]
       [(and (zero? a) (positive? b)) 0]
       [(and (not (zero? a)) (integer? b)) (expt a b)]
       [(= a -1) (if (even? (numerator b)) 1 -1)]
       [(= a 1) 1]
       [else #f])]
    [(list 'sqrt (? exact-value? a))
     (define s1 (sqrt (numerator a)))
     (define s2 (sqrt (denominator a)))
     (and (real? s1) (real? s2) (exact? s1) (exact? s2) (/ s1 s2))]
    [(list 'cbrt (? exact-value? a))
     (define inexact-num (inexact->exact (expt (abs (numerator a)) 1/3)))
     (define inexact-den (inexact->exact (expt (abs (denominator a)) 1/3)))
     (and (real? inexact-num)
          (real? inexact-den)
          (= (expt inexact-num 3) (abs (numerator a)))
          (= (expt inexact-den 3) (abs (denominator a)))
          (* (sgn a) (/ inexact-num inexact-den)))]
    [(list 'fabs (? exact-value? a)) (abs a)]
    [(list 'floor (? exact-value? a)) (floor a)]
    [(list 'ceil (? exact-value? a)) (ceiling a)]
    [(list 'round (? exact-value? a)) (round a)]
    ;; Added
    [(list 'exp 0) 1]
    [(list 'log 1) 0]
    [_ #f]))

(module+ test
  (require rackunit)

  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application '+) 0)
  (check-equal? (eval-application '/ 1 0) #f) ; Not valid
  (check-equal? (eval-application 'cbrt 1) 1)
  (check-equal? (eval-application 'log 1) 0)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

(define (reduce expr)
  (hash-ref! (reduce-cache) expr (λ () (reduce* expr))))

(define (reduce* expr)
  (define res
    (match expr
      [(? number?) expr]
      [(? symbol?) expr]
      [`(,op ,args ...)
       (define args* (map reduce args))
       (define val (apply eval-application op args*))
       (or val (reduce-node (list* op args*)))]))
  (printf "reduce* ~a -> ~a\n" expr res)
  res)

(define (reduce-evaluation expr)
  (match expr
    ['(sin 0) 0]
    ['(cos 0) 1]
    ['(sin (PI)) 0]
    ['(cos (PI)) -1]
    ['(exp 1) '(E)]
    ['(tan 0) 0]
    ['(sinh 0) 0]
    ['(log (E)) 1]
    ['(exp 0) 1]
    ['(tan (PI)) 0]
    ['(cosh 0) 1]
    ['(cos (/ (PI) 6)) '(/ (sqrt 3) 2)]
    ['(tan (/ (PI) 3)) '(sqrt 3)]
    ['(tan (/ (PI) 4)) 1]
    ['(cos (/ (PI) 2)) 0]
    ['(tan (/ (PI) 6)) '(/ 1 (sqrt 3))]
    ['(sin (/ (PI) 3)) '(/ (sqrt 3) 2)]
    ['(sin (/ (PI) 6)) 1/2]
    ['(sin (/ (PI) 4)) '(/ (sqrt 2) 2)]
    ['(sin (/ (PI) 2)) 1]
    ['(cos (/ (PI) 3)) 1/2]
    ['(cos (/ (PI) 4)) '(/ (sqrt 2) 2)]
    [_ expr]))

(define (reduce-inverses expr)
  (match expr
    [`(tanh (atanh ,x)) x]
    [`(cosh (acosh ,x)) x]
    [`(sinh (asinh ,x)) x]
    [`(acos (cos ,x)) x]
    [`(asin (sin ,x)) x]
    [`(atan (tan ,x)) x]
    [`(tan (atan ,x)) x]
    [`(cos (acos ,x)) x]
    [`(sin (asin ,x)) x]
    [`(pow ,x 1) x]
    [`(log (exp ,x)) x]
    [`(exp (log ,x)) x]
    [`(cbrt (pow ,x 3)) x]
    [`(pow (cbrt ,x) 3) x]
    [_ expr]))

(define (reduce-node expr)
  (hash-ref! (reduce-node-cache) expr (λ () (reduce-node* expr))))

(define (reduce-node* expr)
  (define res
    (match (reduce-evaluation expr)
      [(? number?) expr]
      [(? symbol?) expr]
      [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
       (make-addition-node (combine-aterms (gather-additive-terms expr)))]
      [(or `(* ,_ ...)
           `(/ ,_ ...)
           `(cbrt ,_)
           `(pow ,_ ,(? (conjoin rational? (negate even-denominator?)))))
       (make-multiplication-node (combine-mterms (gather-multiplicative-terms expr)))]
      [`(exp (* ,c (log ,x))) (reduce-node* `(pow ,x ,c))]
      [else (reduce-inverses expr)]))
  (printf "reduce-node ~a -> ~a\n" expr res)
  res)

(define (negate-term term)
  (cons (- (car term)) (cdr term)))

(define (gather-additive-terms expr #:label [label #f])
  (define (recurse subexpr #:label [label #f])
    (gather-additive-terms subexpr #:label label))

  (define res
    (let ([label (or label expr)])
      (match expr
        [(? number?) `((,expr 1))]
        [(? symbol?) `((1 ,expr))]
        [`(+ ,args ...) (append-map recurse args)]
        [`(neg ,arg) (map negate-term (recurse arg))]
        [`(- ,arg ,args ...) (append (recurse arg) (map negate-term (append-map recurse args)))]

        ; Prevent fall-through to the next case
        [`(/ ,arg) `((1 ,expr))]
        [`(/ ,arg ,args ...)
         (for/list ([term (recurse arg)])
           (list* (car term) (reduce-node (list* '/ (cadr term) args)) (cons label (cddr term))))]

        [`(pow ,arg 1) `((1 1))]
        [else `((1 ,expr))])))
  (printf "gather-additive-terms ~a -> ~a\n" expr res)
  res)

(define (even-denominator? x)
  (even? (denominator x)))

(define (gather-multiplicative-terms expr)
  (define res
    (match expr
      [(? number?) (list expr)]
      ['NAN `(NAN)]
      [(? symbol?) `(1 (1 . ,expr))]
      [`(neg ,arg)
       (define terms (gather-multiplicative-terms arg))
       (if (eq? (car terms) 'NAN)
           '(NAN)
           (cons (- (car terms)) (cdr terms)))]
      [`(* ,args ...)
       (define terms (map gather-multiplicative-terms args))
       (if (ormap (curry eq? 'NAN) (map car terms))
           '(NAN)
           (cons (apply * (map car terms)) (apply append (map cdr terms))))]
      [`(/ ,arg)
       (define terms (gather-multiplicative-terms arg))
       (cons (if (member (car terms) '(0 NAN))
                 'NAN
                 (/ (car terms)))
             (map negate-term (cdr terms)))]
      [`(/ ,arg ,args ...)
       (define num (gather-multiplicative-terms arg))
       (define dens (map gather-multiplicative-terms args))
       (cons (if (or (eq? (car num) 'NAN) (ormap (compose (curryr member '(0 NAN)) car) dens))
                 'NAN
                 (apply / (car num) (map car dens)))
             (append (cdr num) (map negate-term (append-map cdr dens))))]
      [`(cbrt ,arg)
       (define terms (gather-multiplicative-terms arg))
       (define exact-cbrt
         (match (car terms)
           ['NAN 'NAN]
           [x (eval-application 'cbrt (car terms))]))
       (if exact-cbrt
           (cons exact-cbrt
                 (for/list ([term (cdr terms)])
                   (cons (/ (car term) 3) (cdr term))))
           (list* 1
                  (cons 1 `(cbrt ,(car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (/ (car term) 3) (cdr term)))))]
      [`(pow ,arg 0) '(1)]
      [`(pow ,arg ,(? (conjoin rational? (negate even-denominator?)) a))
       (define terms (gather-multiplicative-terms arg))
       (define exact-pow
         (match (car terms)
           ['NAN 'NAN]
           [x (eval-application 'pow (car terms) a)]))
       (if exact-pow
           (cons exact-pow
                 (for/list ([term (cdr terms)])
                   (cons (* a (car term)) (cdr term))))
           (list* 1
                  (cons a (car terms))
                  (for/list ([term (cdr terms)])
                    (cons (* a (car term)) (cdr term)))))]
      [else `(1 (1 . ,expr))]))
  (printf "gather-multiplicative-terms ~a -> ~a\n" expr res)
  res)

(define (combine-aterms terms)
  (define h (make-hash))
  (for ([term terms])
    (define sum (hash-ref! h (cadr term) 0))
    (hash-set! h (cadr term) (+ (car term) sum)))
  (define res
    (sort (reap [sow]
                (for ([(k v) (in-hash h)]
                      #:when (not (= v 0)))
                  (sow (cons v k))))
          expr<?
          #:key cdr))
  (printf "combine-aterms ~a -> ~a\n" terms res)
  res)

(define (combine-mterms terms)
  (define res
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
  (printf "combine-mterms ~a -> ~a\n" terms res)
  res)

(define (aterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(,x . 1) x]
    [`(-1 . ,x) `(neg ,x)]
    [`(,coeff . ,x) `(* ,coeff ,x)]))

(define (make-addition-node terms)
  (define-values (pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms))
  (define res
    (cond
      [(and (null? pos) (null? neg)) 0]
      [(null? pos) `(neg ,(make-addition-node* (map negate-term neg)))]
      [(null? neg) (make-addition-node* pos)]
      [else `(- ,(make-addition-node* pos) ,(make-addition-node* (map negate-term neg)))]))
  (printf "make-addition-node ~a -> ~a\n" terms res)
  res)

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node* terms)
  (match terms
    ['() 0]
    [`(,term) (aterm->expr term)]
    [`(,term ,terms ...) `(+ ,(aterm->expr term) ,(make-addition-node terms))]))

(define (make-multiplication-node term)
  (define res
    (match (cons (car term) (make-multiplication-subnode (cdr term)))
      [`(NAN . ,e) '(NAN)]
      [`(0 . ,e) 0]
      [`(1 . ()) 1]
      [`(1 . ,e) e]
      [`(,a . 1) a]
      [`(,a . (/ 1 ,denom)) `(/ ,a ,denom)]
      [`(,a . ()) a]
      [`(,a . ,e) `(* ,a ,e)]))
  (printf "make-multiplication-node ~a -> ~a\n" term res)
  res)

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (list (cons 1 (mterm->expr (cons 1 (make-multiplication-subsubnode terms)))))))

(define (make-multiplication-subsubnode terms)
  (define-values (pos neg) (partition (compose positive? car) terms))
  (cond
    [(and (null? pos) (null? neg)) 1]
    [(null? pos) `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg)))]
    [(null? neg) (make-multiplication-subsubsubnode pos)]
    [else
     `(/ ,(make-multiplication-subsubsubnode pos)
         ,(make-multiplication-subsubsubnode (map negate-term neg)))]))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() 1]
    [`(,term) (mterm->expr term)]
    [`(,term ,terms ...) `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms))]))

(define (mterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(-1 . ,x) `(/ 1 ,x)]
    [`(1/2 . ,x) `(sqrt ,x)]
    [`(-1/2 . ,x) `(/ 1 (sqrt ,x))]
    [`(1/3 . ,x) `(cbrt ,x)]
    [`(-1/3 . ,x) `(/ 1 (cbrt ,x))]
    [`(,power . ,x) `(pow ,x ,power)]))

(module+ main
  (reduce '(- (* (+ x 1) (+ x 1)) 1))
  (reduce '(+ (/ 1 (neg x)) (/ 1 (neg x))))
  (reduce '(- (* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) 1))
  (reduce '(* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)))
  (reduce '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x)))))
