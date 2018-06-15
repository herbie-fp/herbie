#lang racket

(require "../config.rkt")
(require "../common.rkt")
(require "../points.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../formats/test.rkt")
(require "../mainloop.rkt")
(require "../errors.rkt")

(define (run #:print-points [print-points? #f])
  (eprintf "; Seed: ~a\n" (get-seed))
  (define in-expr (read))
  (define out-alt
    (match in-expr
      [`(herbie-test . ,_)
       (let ([tst (parse-test in-expr)])
         (set! in-expr (test-program tst))
         (run-improve (test-program tst) (*num-iterations*)))]
      [`(,(or 'λ 'lambda) ,vars ,body)
       (run-improve in-expr (*num-iterations*))]
      [_ (error "did not recognize input")]))
  (printf "; Input error: ~a\n" (errors-score (alt-errors (make-alt in-expr))))
  (printf "; Output error: ~a\n" (errors-score (alt-errors out-alt)))
  (define in-prog (eval-prog in-expr 'fl))
  (define out-prog (eval-prog (alt-program out-alt) 'fl))
  (when print-points?
    (for ([(pt ex) (in-pcontext (*pcontext*))])
      (let ([in-ans (in-prog pt)] [out-ans (out-prog pt)])
        (when (not (= in-ans out-ans))
          (printf "; sample ~a exact ~a input ~a output ~a improvement ~a\n"
                  pt ex in-ans out-ans
                  (- (bit-difference ex in-ans)
                     (bit-difference ex out-ans)))))))
  (printf "~a\n" (alt-program out-alt)))

(module+ main
  (define print-points #f)
  (command-line
   #:program "herbie/inout.rkt"
   #:once-each
   [("-r" "--seed") rs "The random seed vector to use in point generation"
    (set-seed! (read (open-input-string rs)))]
   [("--fuel") fu "The amount of 'fuel' to use"
    (*num-iterations* (string->number fu))]
   [("--num-points") points "The number of points to use"
    (*num-points* (string->number points))]
   [("--print-points") "Print all sampled points"
    (set! print-points #t)]
   #:multi
   [("-o" "--disable") tf "Disable flag formatted category:name"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply disable-flag! flag)]
   [("+o" "--enable") tf "Enable flag formatted category:name"
    (define flag (parse-flag tf))
    (when (not flag)
      (raise-herbie-error "Invalid flag ~a" tf #:url "options.html"))
    (apply enable-flag! flag)]
   #:args ()
   (run #:print-points print-points)))
