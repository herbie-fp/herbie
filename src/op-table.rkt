#lang racket
; an op-table is a hash table from operator to a hash table
; from num of args to a list of function signature
; a function signature have two parts: parameter list and return type
; a paramter list is either list of parameter types in the order they are passed in
; or a * followed by one type, which means it takes in an arbitrary number of paramters
; of that type. A return type is just one type.
(provide get-sigs argtypes->rtype #|get-params get-rt-type|# operator?)

(define op-table
  '#hash((+ . #hash((* . (((* real) real) ((* complex) complex)))))
         (- . #hash((* . (((* real) real) ((* complex) complex)))))
         (* . #hash((* . (((* real) real) ((* complex) complex)))))
         (/ . #hash((* . (((* real) real) ((* complex) complex)))))
         (complex . #hash((2 . (((real real) complex)))))
         (re . #hash((1 . (((complex) real)))))
         (im . #hash((1 . (((complex) real)))))
         (conj . #hash((1 . (((complex) complex)))))
         (sqr . #hash((1 . (((real) real) ((complex) complex)))))
         (cube . #hash((1 . (((real) real)))))
         (acos . #hash((1 . (((real) real)))))
         (acosh . #hash((1 . (((real) real)))))
         (asin . #hash((1 . (((real) real)))))
         (asinh . #hash((1 . (((real) real)))))
         (atan . #hash((1 . (((real) real)))))
         (atan2 . #hash((2 . (((real real) real)))))
         (atanh . #hash((1 . (((real) real)))))
         (cbrt . #hash((1 . (((real) real)))))
         (ceil . #hash((1 . (((real) real)))))
         (copysign . #hash((2 . (((real real) real)))))
         (cos . #hash((1 . (((real) real)))))
         (cosh . #hash((1 . (((real) real)))))
         (erf . #hash((1 . (((real) real)))))
         (efrc . #hash((1 . (((real) real)))))
         (exp . #hash((1 . (((real) real) ((complex) complex)))))
         (exp2 . #hash((1 . (((real) real)))))
         (expm1 . #hash((1 . (((real) real)))))
         (fabs . #hash((1 . (((real) real)))))
         (fdim . #hash((2 . (((real real) real)))))
         (floor . #hash((1 . (((real) real)))))
         (fma . #hash((3 . (((real real real) real)))))
         (fmax . #hash((2 . (((real real) real)))))
         (fmin . #hash((2 . (((real real) real)))))
         (fmod . #hash((2 . (((real real) real)))))
         (hypot . #hash((2 . (((real real) real)))))
         (j0 . #hash((1 . (((real) real)))))
         (j1 . #hash((1 . (((real) real)))))
         (lgamma . #hash((1 . (((real) real)))))
         (log . #hash((1 . (((real) real) ((complex) complex)))))
         (log10 . #hash((1 . (((real) real)))))
         (log1p . #hash((1 . (((real) real)))))
         (log2 . #hash((1 . (((real) real)))))
         (logb . #hash((1 . (((real) real)))))
         (pow . #hash((2 . (((real real) real) ((complex complex) complex)))))
         (remainder . #hash((2 . (((real real) real)))))
         (rin . #hash((1 . (((real) real)))))
         (round . #hash((1 . (((real) real)))))
         (sin . #hash((1 . (((real) real)))))
         (sinh . #hash((1 . (((real) real)))))
         (sqrt . #hash((1 . (((real) real) ((complex) complex)))))
         (tan . #hash((1 . (((real) real)))))
         (tanh . #hash((1 . (((real) real)))))
         (tgamma . #hash((1 . (((real) real)))))
         (trunc . #hash((1 . (((real) real)))))
         (y0 . #hash((1 . (((real) real)))))
         (y1 . #hash((1 . (((real) real)))))
         (== . #hash((* . (((* real) bool)))))
         (!= . #hash((* . (((* real) bool)))))
         (> . #hash((* . (((* real) bool)))))
         (< . #hash((* . (((* real) bool)))))
         (<= . #hash((* . (((* real) bool)))))
         (>= . #hash((* . (((* real) bool)))))
         (!= . #hash((* . (((* real) bool)))))
         (not . #hash((1 . (((bool) bool)))))
         (and . #hash((* . (((* bool) bool)))))
         (or . #hash((* . (((* bool) bool)))))
         ))

(define (get-sigs fun-name num-args)
  (cond
    [(not (hash-has-key? op-table fun-name)) #f]
    [(hash-has-key? (hash-ref op-table fun-name) num-args) (hash-ref (hash-ref op-table fun-name) num-args)]
    [(hash-has-key? (hash-ref op-table fun-name) '*) (hash-ref (hash-ref op-table fun-name) '*)]
    [else #f]))

(define (argtypes->rtype argtypes sig)
  (match sig
    [`((* ,argtype) ,rtype)
     (and (andmap (curry equal? argtype) argtypes) rtype)]
    [`((,expected-types ...) ,rtype)
     (and (andmap equal? argtypes expected-types) rtype)]))

#|
(define (get-params fun-name num-args)
  (and (get-sig fun-name num-args) (car (get-sig fun-name num-args))))

(define (get-rt-type fun-name num-args)
  (and (get-sig fun-name num-args) (cadr (get-sig fun-name num-args))))
|#

(define (operator? name)
  (hash-has-key? op-table name))

(module+ test
  (require rackunit)
  (for* ([(op record) (in-hash op-table)]
         [(num types) (in-hash record)]
         [type types]
         #:unless (equal? num '*))
    (check-equal? num (length (car type)))))
