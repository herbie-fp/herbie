#lang racket

(require "syntax/types.rkt" "points.rkt" "float.rkt")

(provide preprocess-pcontext *herbie-preprocess* apply-preprocess)

;; Tracks list of preprocess structs Herbie decides to apply
(define *herbie-preprocess* (make-parameter empty))

;; index-value-pairs is a sorted list of (index, value)
(define (list-set-multiple list index-value-pairs)
  (let loop ([current list] [todo index-value-pairs] [index 0])
    (cond
      [(empty? current)
       empty]
      [(and (not (empty? todo)) (equal? (first (first todo)) index))
       (cons (second (first todo)) (loop (rest current) (rest todo) (+ index 1)))]
      [else
       (cons (first current) (loop (rest current) todo (+ index 1)))])))

(define (apply-to-group variables point group-variables group-function)
  (define indicies
    (map (lambda (var) (index-of variables var)) group-variables))
  (define values
    (group-function (map (curry list-ref point) indicies)))
  (define sorted (sort (map list indicies values) (lambda (a b) (< (first a) (first b)))))
  (list-set-multiple point sorted))

(define (apply-preprocess sampled-point preprocess-structs ctx)
  (cond
    [(empty? preprocess-structs)
     sampled-point]
    [else
     ;; (define pt* (sort-group (context-vars ctx) sampled-point (first preprocess-structs) (context-repr ctx)))
     (define variables (context-vars ctx))
     (define repr (context-repr ctx))
     (define pt*
       (match (first preprocess-structs)
         [(list 'sort vars ...)
          (apply-to-group variables sampled-point vars (lambda (group) (sort group (curryr </total repr))))]
         [(list 'abs var)
          (apply-to-group variables sampled-point (list var) (lambda (a) (eprintf "abs of this ~s" a) (abs a)))]))
     (apply-preprocess pt* (rest preprocess-structs) ctx)]))
     ;; (define pt*
     ;; (match (first preprocess-structs)
     ;;   [(list 'sort vars) ...]
     ;; (sort-group (context-vars ctx) sampled-point (first preprocess-structs) (context-repr ctx)))
     ;;   (list 'abs var) ..]
     ;; (apply-preprocess pt* (rest preprocess-structs) ctx)]))

(define (preprocess-pcontext pcontext preprocess-structs ctx)
  (for/pcontext ([(pt ex) pcontext])
    (values (apply-preprocess pt preprocess-structs ctx) ex)))
