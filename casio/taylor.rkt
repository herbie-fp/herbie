#lang racket

(require math/number-theory)
(require casio/common)
(require casio/pavel-simplification)

(provide approximate taylor)

(define (approximate expr vars)
  (debug #:from 'taylor "Taking taylor expansion of" expr "in" vars)
  ; TODO : Redo using new "hull" idea
  (simplify
   (cons '+
         (for/list ([var vars])
           (match (taylor var expr)
             [(cons offset coeffs)
              (let* ([n (first-nonzero-exp coeffs)]
                     [exp (- n offset)])
                (cond
                 [(equal? exp 0) (coeffs n)]
                 [(equal? exp 1) `(* ,(coeffs n) ,var)]
                 [(equal? exp 2) `(* ,(coeffs n) (sqr ,var))]
                 [(equal? exp -1) `(/ ,(coeffs n) ,var)]
                 [(equal? exp -2) `(/ ,(coeffs n) (sqr ,var))]
                 [(positive? exp) `(* ,(coeffs n) (expt ,var ,exp))]
                 [(negative? exp) `(/ ,(coeffs n) (expt ,var ,exp))]))])))))

(define (taylor var expr)
  "Return a pair (e, n), such that expr ~= e var^n"
  (match expr
    [(? (curry equal? var))
     (taylor-exact 0 1)]
    [(? symbol?)
     (taylor-exact expr)]
    [(? real?)
     (taylor-exact expr)]
    [`(abs ,arg)
     (taylor-exact expr)]
    [`(+ ,args ...)
     (apply taylor-add (map (curry taylor var) args))]
    [`(- ,arg)
     (taylor-negate ((curry taylor var) arg))]
    [`(- ,arg ,args ...)
     (apply taylor-add ((curry taylor var) arg) (map (compose taylor-negate (curry taylor var)) args))]
    [`(* ,left ,right)
     (taylor-mult (taylor var left) (taylor var right))]
    [`(/ ,arg)
     (taylor-invert (taylor var arg))]
    [`(/ 1 ,arg)
     (taylor-invert (taylor var arg))]
    [`(/ ,num ,den)
     (taylor-quotient (taylor var num) (taylor var den))]
    [`(if ,cond ,btrue ,bfalse)
     (taylor-exact expr)]
    [`(mod ,a ,b)
     (taylor-exact expr)]
    [`(sqr ,a)
     (let ([ta (taylor var a)])
       (taylor-mult ta ta))]
    [`(,(? (curryr member taylor-funcs) f) ,args ...)
     (let ([args* (map (compose normalize-series (curry taylor var)) args)])
       (if (ormap (compose positive? car) args*)
           (taylor-exact expr)
           (apply taylor-func f (map zero-series args*))))]
    [_
     (taylor-exact expr)]))

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

(define (taylor-exact . terms)
  (cons 0
        (λ (n)
           (if (<= (length terms) n)
               0
               (list-ref terms n)))))

(define (first-nonzero-exp f)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (equal? (f n) 0)
        (loop (+ n 1))
        n)))

(define (align-series . serieses)
  (let ([offset* (car (argmax car serieses))])
    (for/list ([series serieses])
      (let ([offset (car series)])
        (cons offset* (compose (cdr series) (curry + (- offset* offset))))))))

(define (taylor-add . terms)
  (match (apply align-series terms)
    [`((,offset . ,serieses) ...)
     (cons (car offset)
           (λ (n) (simplify (cons '+ (for/list ([series serieses]) (series n))))))]))

(define (taylor-negate term)
  (cons (car term) (λ (n) (simplify (list '- ((cdr term) n))))))

(define (taylor-mult left right)
  (cons (+ (car left) (car right))
        (lambda (n)
          (simplify
           (cons '+
                 (for/list ([i (range (+ n 1))])
                   (list '* ((cdr left) i) ((cdr right) (- n i)))))))))

(define (normalize-series series)
  "Fixes up the series to have a non-zero zeroth term,
   allowing a possibly negative offset"
  (match series
    [(cons offset coeffs)
     (let ([slack (first-nonzero-exp coeffs)])
       (cons (- offset slack) (compose coeffs (curry + slack))))]))

(define ((zero-series series) n)
  (if (< n (- (car series))) 0 ((cdr series) (+ n (car series)))))

(define (taylor-invert term)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (normalize-series term)
    [(cons offset b)
     (let ([hash (make-hash)])
       (hash-set! hash 0 `(/ 1 ,(b 0)))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     `(- (+ ,@(for/list ([i (range n)])
                                                `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))])
         (cons (- offset) f)))]))

(define (taylor-quotient num denom)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (cons (normalize-series num) (normalize-series denom))
    [(cons (cons noff a) (cons doff b))
     (let ([hash (make-hash)])
       (hash-set! hash 0 `(/ ,(a 0) ,(b 0)))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     `(- (/ ,(a n) ,(b 0))
                                         (+ ,@(for/list ([i (range n)])
                                                `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))]
                [offset (- noff doff)])
         (cons offset f)))]))

(define (rle l)
  (for/list ([run (multipartition l identity)])
    (cons (length run) (car run))))

(define (partition-list n)
  (define (aux n k)
    (cond
     [(= n 0) '(())]
     [(< n k) '()]
     [else
      (append (map (curry cons k) (aux (- n k) k))
              (aux n (+ k 1)))]))
  (map rle (aux n 1)))

(define taylor-funcs '(exp))

(define (taylor-func func . args)
  (match (cons func args)
    [`(exp ,coeffs)
     (cons 0
           (λ (n)
              (if (= n 0)
                  `(exp ,(coeffs 0))
                  (simplify
                   `(* (exp ,(coeffs 0))
                       (+
                         ,@(for/list ([p (partition-list n)])
                             `(*
                               ,@(for/list ([factor p])
                                   `(/ (expt ,(coeffs (cdr factor)) ,(car factor))
                                       ,(factorial (car factor))))))))))))]))
