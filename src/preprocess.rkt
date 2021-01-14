#lang racket

(provide (struct-out symmetry-group) preprocess->sexp sexp->preprocess *herbie-preprocess*)

;; Tracks list of preprocess structs Herbie decides to apply
(define *herbie-preprocess* (make-parameter empty))

;; Herbie preprocess structs
(struct symmetry-group (variables) #:prefab)


(define (preprocess->sexp preprocess)
  `(sort ,@(symmetry-group-variables preprocess)))

(define (sexp->preprocess sexp)
  (match sexp
    [(list 'sort vars ...) (symmetry-group vars)]
    [else (error (format "unknown preprocess ~a" sexp))]))
