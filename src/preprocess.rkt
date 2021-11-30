#lang racket

(require rival math/bigfloat)
(require "interface.rkt" "programs.rkt" "float.rkt" "points.rkt")

(provide (struct-out symmetry-group) preprocess->sexp sexp->preprocess
         preprocess-pcontext
         *herbie-preprocess* apply-preprocess ival-preprocesses)

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

(define (<-repr repr a b)
  (< (repr->real a repr) (repr->real b repr)))

(define (apply-to-group variables point group-variables group-function)
  (define indicies
    (map (lambda (var) (index-of variables var)) group-variables))
  (define values
    (group-function (map (curry list-ref point) indicies)))
  (define sorted (sort (map list indicies values) (lambda (a b) (< (first a) (first b)))))
  (list-set-multiple point sorted))

(define (sort-group variables point preprocess repr)
  (apply-to-group variables point (symmetry-group-variables preprocess)
                  (lambda (group)
                    (sort group (curry <-repr repr)))))

(define (apply-preprocess variables sampled-point preprocess-structs repr)
  (cond
    [(empty? preprocess-structs)
     sampled-point]
    ;; Add more preprocess cases here- for now, only symmetry-group exists
    [else
     (apply-preprocess variables (sort-group variables sampled-point (first preprocess-structs) repr) (rest preprocess-structs) repr)]))

(define (preprocess-pcontext variables pcontext preprocess-structs repr)
  (for/pcontext ([(pt ex) pcontext])
    (values (apply-preprocess variables pt preprocess-structs repr) ex)))
