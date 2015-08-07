#lang racket

(require "interact.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../config.rkt")

(define (run)
  (define in-expr (read))
  (display (format "improving ~a...~n" in-expr) (current-error-port))
  (define out-prog
    (alt-program
     (match in-expr
       [`(herbie-test . ,_)
        (let ([tst (parse-test in-expr)])
          (run-improve (test-program tst) (*num-iterations*)
                       #:samplers (test-samplers tst)))]
       [`(,(or 'λ 'lambda) ,vars ,body)
        (run-improve in-expr (*num-iterations*))]
       [_ (error "did not recognize input")])))
  (write out-prog)
  (newline))

(run)
