#lang racket

(provide (struct-out egglog-program)
         make-egglog-program
         egglog-program-add!
         egglog-program-add-list!
         get-current-program
         egglog-program-copy)

;; Track the entire Egglog program in one go by "converting" into racket based code
(struct egglog-program ([exprs #:mutable])) ; update using set-egglog-program-exprs!

;; (Listof exprs) to store the program
(define (make-egglog-program)
  (egglog-program '()))

;; Add a single expr to the program (in REVERSE)
(define (egglog-program-add! expr program)
  (set-egglog-program-exprs! program (cons expr (egglog-program-exprs program)))
  (void))

;; Append an expr-list to the program (in REVERSE)
(define (egglog-program-add-list! expr-list program)
  (set-egglog-program-exprs! program (append (reverse expr-list) (egglog-program-exprs program)))
  (void))

;; Get program as (Listof exprs) in the correct order
(define (get-current-program program)
  (reverse (egglog-program-exprs program)))

;; Creates a new egglog program using an existing one
(define (egglog-program-copy program)
  (struct-copy egglog-program program [exprs (egglog-program-exprs program)]))
