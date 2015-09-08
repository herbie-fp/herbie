#lang racket

(require "interact.rkt")
(require "../points.rkt")
(require "../alternative.rkt")
(require "../test.rkt")
(require "../config.rkt")

(define (run)
  (eprintf "; Seed: ~a\n" (pseudo-random-generator->vector (current-pseudo-random-generator)))
  (define in-expr (read))
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
  (printf "; Input error: ~a\n" (errors-score (alt-errors (make-alt in-expr))))
  (printf "; Output error: ~a\n" (errors-score (alt-errors out-alt)))
  (printf "~a\n" (alt-program out-alt)))

(module+ main
  (command-line
   #:program "herbie/inout.rkt"
   #:once-each
   [("-r" "--seed") rs "The random seed vector to use in point generation"
    (vector->pseudo-random-generator!
     (current-pseudo-random-generator)
     (read (open-input-string rs)))]
   [("--fuel") fu "The amount of 'fuel' to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   #:multi
   [("-o" "--option") tf "Toggle flags, specified in the form category:flag"
    (let ([split-strings (string-split tf ":")])
      (when (not (= 2 (length split-strings)))
        (error "Badly formatted input " tf))
      (toggle-flag! (string->symbol (car split-strings)) (string->symbol (cadr split-strings))))]
   #:args _
   (run)))
