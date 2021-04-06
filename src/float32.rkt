#lang racket

(require math/bigfloat)

; Racket CS made single-flonums a little confusing
; All single-precision code is here to make things easier

(provide 
  (contract-out
   [single-flonum-supported? (-> boolean?)]
   [float32? (-> any/c boolean?)]
   [->float32 (-> real? float32?)]
   [float32->ordinal (-> float32? exact-integer?)]
   [ordinal->float32 (-> exact-integer? float32?)]
   [bigfloat->float32 (-> bigfloat? float32?)]
   [fl32+ (-> float32? ... float32?)]
   [fl32- (-> float32? float32? ... float32?)]
   [fl32* (-> float32? ... float32?)]
   [fl32/ (-> float32? float32? ... float32?)]))


(define at-least-racket-8?
  (>= (string->number (substring (version) 0 1)) 8))

; Returns true if single flonum is available directly (BC)
; or through emulation (CS, >= 8.0)
(define (single-flonum-supported?)
  (or (single-flonum-available?) at-least-racket-8?))

; Contracts are problematic (BC, < 8.0): values not
; necessarily single flonums. Bug in Herbie?
(define float32?
  (if at-least-racket-8?
      flonum?
      single-flonum?))

; Need a placeholder for < 8.0
(define cast-single
  (let ([flsingle identity])
    (local-require racket/flonum)
    flsingle))

(define (->float32 x)
  (if at-least-racket-8?
      (cast-single (exact->inexact x))
      (real->single-flonum x)))

(define (float32->bit-field x)
  (integer-bytes->integer (real->floating-point-bytes x 4) #f #f))

(define (float32->ordinal x)
  (if (negative? x)
      (- (float32->bit-field (- x)))
      (float32->bit-field (abs x))))

(define (bit-field->float32 x)
  (->float32 (floating-point-bytes->real (integer->integer-bytes x 4 #f #f) #f)))

(define (ordinal->float32 x)
  (if (negative? x)
      (- (bit-field->float32 (- x)))
      (bit-field->float32 x)))

(define (float32-step x n)
  (ordinal->float32 (+ (float32->ordinal x) n)))

(define (bigfloat->float32 x)
  (define loprec (parameterize ([bf-precision 24]) (bf+ 0.bf x)))
  (define y (->float32 (bigfloat->flonum loprec)))
  (define x2 (bf y))
  (match (bf-rounding-mode)
   ['nearest y]
   ['up     (if (bf< x2 x) (float32-step y 1) y)]
   ['down   (if (bf> x2 x) (float32-step y -1) y)]
   ['zero   (if (bf< x 0.bf)
                (if (bf< x2 x) (float32-step y 1) y)
                (if (bf> x2 x) (float32-step y -1) y))]))

(define-syntax-rule (float32-fun name op)
  (define name (compose ->float32 op)))

(define-syntax-rule (float32-funs [name op] ...)
  (begin (float32-fun name op) ...))

(float32-funs
  [fl32+  +]
  [fl32-  -]
  [fl32*  *]
  [fl32/  /])
    
(module+ test
  (require rackunit)
  (check-equal? (fl32+ 1.0 2.0) (->float32 3.0))
  (check-equal? (fl32- 1.0 2.0) (->float32 -1.0))
  (check-equal? (fl32* 1.0 2.0) (->float32 2.0))
  (check-equal? (fl32/ 1.0 2.0) (->float32 0.5)))