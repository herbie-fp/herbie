#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/analyze-local-error)

(provide improve improve-alt)

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         [filter   . (dedup some-max)]
         [reduce   . (regimes zaching)]
         [loop     . ()])))

(define (flag type f)
  (member f (hash-ref (*flags*) type (λ () (error "Invalid flag type" type)))))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel)
  (debug-reset)
  (let-values ([(pts exs) (prepare-points prog)])
    (parameterize ([*points* pts] [*exacts* exs])
      (improve-loop (list (make-alt prog)) (list) fuel))))

(define (improve-alt alt fuel)
  (improve-loop (list alt) (list) fuel))

(define (improve-loop alts olds fuel)
  (if (= fuel 0)
      (reduce-alts (append alts olds))
      (improve-loop (filter-alts (apply append (map generate-alts alts)))
                    (append alts olds)
                    (- fuel 1))))

(define (reduce-alts alts)
  (argmin (compose errors-score alt-errors) alts))

(define (generate-alts altn)
  (apply append
         (for/list ([loc (analyze-local-error altn)])
           #;(println altn loc)
           (alt-rewrite-rm altn #:root loc))))

(define (filter-alts alts)
  (list (argmin (compose errors-score alt-errors) alts)))
