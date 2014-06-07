#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/combine-alts)

(provide *flags* improve improve-alt)

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         [filter   . ()]
         [reduce   . (regimes)]
         [setup    . (simplify)])))

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel)
  (debug-reset)
  (let-values ([(pts exs) (prepare-points prog)])
    (parameterize ([*points* pts] [*exacts* exs])
      (improve-alt (make-alt prog) fuel))))

(define (improve-alt alt fuel)
  (let ([clean-alt ((flag 'setup 'simplify-first) simplify-alt identity)])
    (improve-loop (list (clean-alt alt)) (list) fuel)))

(define (improve-loop alts olds fuel)
  (if (or (<= fuel 0) (null? alts))
      (reduce-alts (append alts olds) fuel)
      (improve-loop
       (filter-alts (append-map generate-alts alts))
       (append alts olds)
       (- fuel 1))))

(define (reduce-alts alts fuel)
  (let ([combine
         ((flag 'reduce 'regimes) regimes-alts (const #f))])
    (or (combine alts fuel) (best-alt alts))))

(define (generate-alts altn)
  (apply append
         (for/list ([loc (analyze-local-error altn)])
           (let ([rewrite
                  ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)]
                 [cleanup
                  ((flag 'generate 'simplify) simplify-alt identity)])
             (map cleanup (rewrite altn #:root loc))))))

(define (filter-alts alts)
  (if (null? alts)
      alts
      (list (best-alt alts))))

;; Some helpers

(define (simplify-alt altn)
  (apply-changes altn (simplify altn)))

(define (regimes-alts alts fuel)
  (let ([alts* (plausible-alts alts)])
    (if (> 2 (length alts*))
        #f
        (best-combination alts*
         #:pre-combo-func (curryr improve-alt (floor (/ fuel 2)))))))

(define (best-alt alts)
  (argmin (compose errors-score alt-errors) alts))
