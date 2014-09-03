#lang racket

;;################################################################################;;
;;
;; This file contains utility functions, mostly for checking invariants, used by
;; the simplification code.
;;
;;################################################################################;;

(provide (all-defined-out))

(define-syntax assert
  (syntax-rules ()
    [(assert pred #:loc location)
     (when (not pred)
       (error location "~a returned false!" 'pred))]
    [(assert pred)
     (when (not pred)
       (error 'assert "~a returned false!" 'pred))]))
