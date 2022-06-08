#lang racket

(require "interface.rkt" "points.rkt")

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
  (match-define (list 'sort vars ...) preprocess)
  (apply-to-group variables point vars (lambda (group) (sort group (curry <-repr repr)))))

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
