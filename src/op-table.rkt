#lang racket
; an op-table is a hash table with the name of the functions as keys and (1) either a pair or
; (2) inner hash table as values.
; The value is a list if there is only one way to use this function. Then the first element of
; this pair is the number of arguments and second element is parameter list and third element
; is the return type
; e.g. (hash-ref op-table '+) = '(2 ('real 'real) 'real))
; if there are multiple ways to call this function then the value is an inner hash table
; e.g. (hash-ref op-table '-) = '#hash((1 . (('real)  'real)) (2 . (('real 'real) 'real)))

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
         (sqr . #hash((1 . (((real) real)))))
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
         (exp . #hash((1 . (((real) real)))))
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
         (log . #hash((1 . (((real) real)))))
         (log10 . #hash((1 . (((real) real)))))
         (log1p . #hash((1 . (((real) real)))))
         (log2 . #hash((1 . (((real) real)))))
         (logb . #hash((1 . (((real) real)))))
         (pow . #hash((2 . (((real real) real)))))
         (remainder . #hash((2 . (((real real) real)))))
         (rin . #hash((1 . (((real) real)))))
         (round . #hash((1 . (((real) real)))))
         (sin . #hash((1 . (((real) real)))))
         (sinh . #hash((1 . (((real) real)))))
         (sqrt . #hash((1 . (((real) real)))))
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
  (if (and (hash-has-key? op-table fun-name) (hash-has-key? (hash-ref op-table fun-name) num-args))
      (hash-ref (hash-ref op-table fun-name) num-args)
      (if (hash-has-key? (hash-ref op-table fun-name) '*)
          (hash-ref (hash-ref op-table fun-name) '*)
          #f)))

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
