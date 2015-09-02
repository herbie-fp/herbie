#lang racket

(require "interact.rkt")
(require "../points.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../config.rkt")

(define (run)
  (define in-expr (read))
  (eprintf "improving ~a...\n" in-expr)
  (define out-alt
    (match in-expr
      [`(herbie-test . ,_)
       (let ([tst (parse-test in-expr)])
         (set! in-expr (test-program tst))
         (run-improve (test-program tst) (*num-iterations*)
                      #:samplers (test-samplers tst)))]
      [`(,(or 'λ 'lambda) ,vars ,body)
       (run-improve in-expr (*num-iterations*))]
      [_ (error "did not recognize input")]))
  (printf "~a\n" (alt-program out-alt)))

(module+ main
  (run))
