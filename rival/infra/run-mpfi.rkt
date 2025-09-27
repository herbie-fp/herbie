#lang racket

(require biginterval)
(require math/bigfloat)

(require "./interval-evaluate.rkt")
(provide program-body
         program-variables)

(define (program-body prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define (program-variables prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  vars)

(define (run-on-points port output-port point-count)
  (parameterize ([bf-precision 10000])
    (define read-res (read port))
    (when (equal? (modulo point-count 1000) 0)
      (display "Ran on ")
      (display point-count)
      (displayln " points"))
    (flush-output)

    (when #f
      #;(not (equal? read-res eof))
      (match-define (list suite prog pt) read-res)

      (define mpfi-res (interval-evaluate (program-body prog) (program-variables prog) pt #t))

      (when mpfi-res
        (define rival-res (interval-evaluate (program-body prog) (program-variables prog) pt #f))
        (writeln (list suite prog pt rival-res (list (ivleft mpfi-res) (ivright mpfi-res)))
                 output-port))

      (run-on-points port output-port (+ point-count 1)))))

(module+ main
  (command-line
   #:program "run-mpfi"
   #:args (points-file output-file)
   (run-on-points (open-input-file points-file) (open-output-file output-file #:exists 'replace) 0)))
