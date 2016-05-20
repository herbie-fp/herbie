#lang racket

(require "test.rkt")

(provide convert)

; parse old herbie syntax into FPCore
(define (convert expr)
  (define-values (vars* args*)
    (match expr
      [(list 'herbie-test (list vars ...) (? string? name) input)
       (values vars (list '#:name name input))]
      [(list 'herbie-test (list vars ...) (? string? name) input output)
       (values vars (list '#:name name '#:target output input))]
      [(list 'herbie-test (list vars ...) input output)
       (values vars (list '#:name "Unnamed Test" '#:target output input))]
      [(list 'lambda (list vars ...) args ...)
       (values vars args)]
      [(list 'define name (list vars ...) args ...)
       (values vars (list*'#:name name args))]))
  (match-define (list (cons vars samp) ...) (map var&dist vars*))
  (match-define (list body args ...) (args&body args*))

  (define (translate-prop old new)
    (let ([name (get old args #f)])
      (if name (list new name) (list))))

  `(FPCore ,vars
      ,@(translate-prop '#:name ':name)
      ,@(translate-prop '#:expected ':herbie-expected)
      ,@(translate-prop '#:target ':target)
      :herbie-samplers ,samp
      ,body))

(define (get kw target default)
  (let ([rec (assoc kw target)])
    (if rec (cdr rec) default)))

(module+ main
 (command-line
   #:program "herbie-to-fpcore"
   #:args (file)
   (for ([test (in-port read (open-input-file file))])
     (pretty-print (convert test) (current-output-port) 1))))

