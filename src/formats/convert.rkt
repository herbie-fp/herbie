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

  (~a "(FPCore " vars
      (let ([name (get '#:name args #f)])
        (if name (~a " :name " name) ""))
      (~a " :pre (and " (~a samp) ") ")
      body ")"))


(define (get kw target default)
  (let ([rec (assoc kw target)])
    (if rec (cdr rec) default)))
