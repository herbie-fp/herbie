#lang racket

;; Single-precision floating-point library
;; Racket CS only has double-precision numbers but can do
;; single-precision rounding, so we need to provide some additional emulation

(require math/flonum math/bigfloat)
(provide float32? ->float32 bigfloat->float32
         float32->ordinal ordinal->float32
         fl32+ fl32- fl32* fl32/)

(module+ test (require rackunit))

(define float32? flonum?)

(define (->float32 x)
  (flsingle (exact->inexact x)))

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

(define fl32+ (compose ->float32 +))
(define fl32- (compose ->float32 -))
(define fl32* (compose ->float32 *))
(define fl32/ (compose ->float32 /))
    
(module+ test
  (check-equal? (fl32+ 1.0 2.0) (->float32 3.0))
  (check-equal? (fl32- 1.0 2.0) (->float32 -1.0))
  (check-equal? (fl32* 1.0 2.0) (->float32 2.0))
  (check-equal? (fl32/ 1.0 2.0) (->float32 0.5)))
