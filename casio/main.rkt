#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/combine-alts)
(require casio/locations)
(require casio/programs)
(require casio/periodicity)

(provide *flags* improve improve-alt)

(define *flags*
  (make-parameter
   #hash([generate . (simplify rm)]
         [reduce   . (regimes zach)]
         [setup    . (simplify periodicity)])))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

(define (improve prog fuel)
  (debug-reset)
  (let-values ([(pts exs) (prepare-points prog)])
    (parameterize ([*points* pts] [*exacts* exs])
      (improve-alt (make-alt prog) fuel))))

(define (improve-alt alt fuel)
  (let ([alts* (setup-alt alt fuel)])
    (improve-loop alts* alts* fuel)))

;; Implementation

(define ((flag type f) a b)
  (if (member f (hash-ref (*flags*) type
                          (λ () (error "Invalid flag type" type))))
      a
      b))

(define (setup-alt altn fuel)
  (let ([maybe-period ((flag 'setup 'periodicity)
		       (curry optimize-periodicity
			      (λ (altn)
				 ;; We call improve-loop directly because we don't want simplify or periodicity running on our
				 ;; subexpressions.
				(improve-loop (list altn) (list altn) fuel))) identity)]
	[maybe-simplify ((flag 'setup 'simplify) simplify-alt identity)])
    (list (maybe-simplify (maybe-period altn)))))

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
        (combine-alts alts*
         #:pre-combo-func (λ (altn) (improve-loop (list altn) (list altn) (/ fuel 2)))))))

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
