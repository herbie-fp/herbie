#lang racket

(provide *functions* inline-functions)

;; name -> (vars prec body)
(define *functions* (make-parameter (make-hash)))

(define (replace-vars expr dict)
  (let loop ([expr expr])
    (match expr
     [(list op args ...)
      (cons op (map loop args))]
     [(? (curry dict-has-key? dict))
      (dict-ref dict expr)]
     [_ expr])))

(define (inline-functions expr)
  (let loop ([expr expr])
    (match expr
     [(list (? (curry hash-has-key? (*functions*)) fname) args ...)
      (match-define (list vars _ body) (hash-ref (*functions*) fname))
      (define dict (map cons vars args))
      (replace-vars body dict)]
     [(list op args ...)
      (cons op (map loop args))]
     [_ expr])))
    