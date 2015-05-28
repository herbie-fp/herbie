#lang racket

(require "../alternative.rkt")
(require "../programs.rkt")
(require "../matcher.rkt")
(require "../infer-regimes.rkt")

(provide find-best-axis texify-formula make-ranges graph-error
         expand-at-loc make-steps make-combo)

;; Find the axis that best portrays the error behavior
(define (find-best-axis alt) (car (program-variables (alt-program alt))))
;; Generate the tex for the given prog, with the given locations
;; highlighted and given MathJax ID's
(define (texify-formula prog [locs '()]) "x")
;; Given a context and an alt and some locations, identify which
;; ranges of error coorespond to which locations along the given axis,
;; and generate list of hash table objects for them.
(define (make-ranges context alt locs axis) '())
;; Draw the graph of average error using the given points for the
;; given alt, along the given axis. If combo is given draw it also on
;; the same graph in a different color.
(define (graph-error context alt axis [combo #f])
  (define (sow-data sow data-points theme)
    (sow (error-points data-points pnts #:axis axis #:color theme))
    (sow (error-avg data-points pnts #:axis axis #:color theme)))
  (let ([points (for/list ([(p e) (in-pcontext context)]) p)])
    (parameterize ([*pcontext* context])
      (reap [sow]
            (sow (error-avg (alt-errors alt) points #:axis axis #:color *red-theme*))
            (when combo
              (sow (error-avg (alt-errors alt) points #:axis axis #:color *blue-theme*)))))))
;; Generate at most three or four children for the given alt at the
;; given location.
(define (expand-at-loc alt loc)
  (general-filter
   (append (taylor-filter (taylor-alt alt loc))
           (rewrite-filter (alt-rewrite alt #:root loc)))))
;; Generate the list of steps hash objects representing the changes
;; between the parent and the child.
(define (make-steps child parent)
  (let steps-left ([cur child] [steps '()])
    (cond [(not cur) (error "The given parent is not a parent of the child!")]
          [(equal? cur parent)
           steps]
          [#t (steps-left (alt-prev cur)
                          (cons
                           (hash
                            "rule" (let ([rule (change-rule (alt-change cur))])
                                     (if (equal? (rule-name rule) 'simplify)
                                         "simplify"
                                         (texify-formula (change-rule (alt-change cur)))))
                            "prog" (texify-formula (alt-program cur)))
                           steps))])))
    
;; Combine the given alternatives into the best combination.
(define (make-combo alts)
  (match-let ([`(,splitpoints ,involved-alts) (infer-splitpoints alts)])
    (if (= (length involved-alts) 1)
        (car involved-alts)
        (combine-alts splitpoints involved-alts))))

;; =============== Lower level helper functions =============

;; Filter children
(define (general-filter alts) alts)
(define (taylor-filter alts) alts)
(define (rewrite-filter alts) alts)
