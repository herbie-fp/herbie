#lang racket

(require rackunit)
(require math/bigfloat)
(require "../config.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../format/test.rkt")

(define exacts-test
  (test-suite
   "Test that exact evaluation matches 65536-bit floating point"
   (for ([test benchmark-path])
     (bf-precision (*precision-step*))
     (printf ">> ~a" (test-program test))
     (define ctx (prepare-points (test-program test) (test-precondition test)))
     (printf " (at ~a bits)\n" (bf-precision))
     (bf-precision 65536)
     (define f (eval-prog (test-program test) mode:bf))
     (for ([(pt ex) (in-pcontext ctx)])
       (with-check-info
        (['point pt] ['program (test-program test)])
        (when (not (= ex (f pt)))
          (check-equal? ex (f pt))))))))
