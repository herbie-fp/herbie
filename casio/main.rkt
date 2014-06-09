#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/combine-alts)
(require casio/locations)
(require casio/programs)

(provide *flags* improve improve-alt)

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         [filter   . ()]
         [reduce   . (regimes zach)]
         [setup    . (simplify)])))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel)
  (debug-reset)
  (let-values ([(pts exs) (prepare-points prog)])
    (parameterize ([*points* pts] [*exacts* exs])
      (improve-alt (make-alt prog) fuel))))

(define (improve-alt alt fuel)
  (let* ([clean-alt ((flag 'setup 'simplify-first) simplify-alt identity)]
         [alt* (clean-alt alt)])
    (improve-loop (list alt*) (list alt*) fuel)))

;; Implementation

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))

(define (improve-loop alts olds fuel)
  (if (or (<= fuel 0) (null? alts))
      (reduce-alts olds 5)
      (improve-loop
       (filter-alts (append-map generate-alts alts) olds)
       (append olds alts)
       (- fuel 1))))

(define (reduce-alts alts fuel)
  (let ([combine
         ((flag 'reduce 'regimes) regimes-alts (const #f))]
        [fixup
         ((flag 'reduce 'zach) zach-alt (const '()))])
    (let* ([alts* (append alts (append-map fixup alts))]
           [alts* (remove-duplicates alts* #:key alt-program)])
      (or (combine alts* fuel) (best-alt alts*)))))

(define (generate-alts altn)
  (append-map (curry generate-alts-at altn) (analyze-local-error altn)))

(define (generate-alts-at altn loc)
  (let ([rewrite
         ((flag 'generate 'rm) alt-rewrite-rm alt-rewrite-expression)]
        [cleanup
         ((flag 'generate 'simplify) simplify-alt identity)])
    (map cleanup (rewrite altn #:root loc))))

(define (filter-alts alts olds)
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

(define (zach-alt altn)
  (apply append
         (for/list ([loc (analyze-local-error altn)])
           (let ([sibling (location-sibling loc)])
             (if (and sibling
                      (= (length (location-get (location-parent loc)
                                               (alt-program altn))) 3))
                 (generate-alts-at altn sibling)
                 '())))))
