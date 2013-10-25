#lang racket

(require racket/match)
(require racket/flonum)

(define prog1 '(/ (- (exp x) 1.0d0) x))

(define (error/one var prog val)
  (let* [(fn (eval `(lambda (,var) ,prog)))
         (double-val (fn (real->double-flonum val)))
         (single-val (fn (real->single-flonum val)))]
    (flabs (fl/ (fl- double-val single-val) double-val))))

(define (random-flonum)
  (expt 2 (- (* 253 (random)) 126)))

(define (error var prog)
         (filter (lambda (x) (not (or (infinite? x) (nan? x))))
                 (build-list 100 (lambda (x) (error/one var prog (random-flonum))))))
