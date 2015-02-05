#lang racket

(require rackunit)
(require math/bigfloat)
(require herbie/config)
(require herbie/programs)
(require herbie/points)
(require herbie/test)
(require herbie/load-tests)

(define tests (load-tests "../bench/hamming"))

(define exacts-test
  (test-suite
   "Test that exact evaluation matches 65536-bit floating point"
   (for ([test tests])
     (bf-precision (*precision-step*))
     (printf ">> ~a" (test-program test))
     (define ctx (prepare-points (test-program test) (test-samplers test)))
     (printf " (at ~a bits)\n" (bf-precision))
     (bf-precision 65536)
     (define f (eval-prog (test-program test) mode:bf))
     (for ([(pt ex) (in-pcontext ctx)])
       (with-check-info
        (['point pt] ['program (test-program test)])
        (when (not (= ex (f pt)))
          (check-equal? ex (f pt))))))))
