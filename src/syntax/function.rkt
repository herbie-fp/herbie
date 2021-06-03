#lang racket

(provide *functions* inline-functions)

(module+ test (require rackunit))

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
    

(module+ test
  ;; Test classic quadp and quadm examples
  (define discr-func-info (list (list 'a 'b 'c) 'binary64 `(sqrt (- (* b b) (* 4 a c)))))
  (hash-set! (*functions*) 'discr discr-func-info)
  (define quadp `(/ (+ (- y) (discr x y z)) (* 2 x)))
  (define quadm `(/ (- (- y) (discr x y z)) (* 2 x)))
  (check-equal? (inline-functions quadp) '(/ (+ (- y) (sqrt (- (* y y) (* 4 x z)))) (* 2 x)))
  (check-equal? (inline-functions quadm) '(/ (- (- y) (sqrt (- (* y y) (* 4 x z)))) (* 2 x)))

  ;; x^5 = x^3 * x^2
  (define sqr-info (list (list 'x) 'binary64 '(* x x)))
  (define cube-info (list (list 'x) 'binary64 '(* x x x)))
  (hash-set! (*functions*) 'sqr sqr-info)
  (hash-set! (*functions*) 'cube cube-info)
  (define fifth '(* (cube a) (sqr a)))
  (check-equal? (inline-functions fifth) '(* (* a a a) (* a a))))

