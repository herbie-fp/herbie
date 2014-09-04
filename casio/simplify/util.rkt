#lang racket

;;################################################################################;;
;;
;; This file contains utility functions, mostly for checking invariants, used by
;; the simplification code.
;;
;;################################################################################;;

(provide (all-defined-out))

(define-syntax assert
  (syntax-rules (=)
    [(assert (= a b) #:loc location)
     (when (not (= a b))
       (error location "~a returned false!~n~a is ~a, but ~a is ~a"
	      (list '= 'a 'b)
	      'a a
	      'b b))]
    [(assert (= a b))
     (when (not (= a b))
       (error 'assert "~a returned false!~n~a is ~a, but ~a is ~a"
	      (list '= 'a 'b)
	      'a a
	      'b b))]
    [(assert pred #:loc location)
     (when (not pred)
       (error location "~a returned false!" 'pred))]
    [(assert pred)
     (when (not pred)
       (error 'assert "~a returned false!" 'pred))]))
