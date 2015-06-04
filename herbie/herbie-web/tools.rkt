#lang racket

;; ================== Dependencies ===================

;; For converting floating point numbers into their ordinal
;; representation, for clustering.
(require math/flonum)

(require "../alternative.rkt")
(require "../programs.rkt")
(require "../matcher.rkt")
(require "../infer-regimes.rkt")
(require "../plot.rkt")
(require "../points.rkt")
(require "../common.rkt")
(require "../main.rkt")
(require "../compile/tex.rkt")

;; ================== Interface =======================

(provide find-best-axis texify-formula make-ranges graph-error
         expand-at-loc make-steps make-combo)

;; ================== Parameters ======================

;; How many clusters to attempt to cluster points into to determine
;; which axis is best.
(define *num-clusters* (make-parameter 5))
;; The number of trials of k-means scoring to use.
(define *num-scores* (make-parameter 3))

;; Find the axis that best portrays the error behavior
(define (find-best-axis alt pcontext)
  (let ([bad-points (for/list ([(p ex) (in-pcontext pcontext)]
                               [e (parameterize ([*pcontext* pcontext])
                                    (alt-errors alt))]
                               #:when (> e (expt 2 10)))
                      p)]
        [vars (program-variables (alt-program alt))])
    (list-ref vars
              (argmax (λ (pidx)
                        ;; Rank the variables by how relevant they are to the error.
                        (cluster-rank (map (compose flonum->ordinal
                                                    (curryr list-ref pidx))
                                           bad-points)))
                      (build-list (length vars) identity)))))
;; Generate the tex for the given prog, with the given locations
;; highlighted and given MathJax ID's
(define (texify-formula prog [locs '()])
  (texify-expression (program-body prog)
                     #:highlight-ops
                     (for/list ([loc locs] [idx (in-naturals)])
                       (cons loc idx))))
;; Given a context and an alt and some locations, identify which
;; ranges of error coorespond to which locations along the given axis,
;; and generate list of hash table objects for them.
(define (make-ranges context alt locs axis) '())
;; Draw the graph of average error using the given points for the
;; given alt, along the given axis. If combo is given draw it also on
;; the same graph in a different color.
(define (graph-error context alt axis [combo #f])
  (let ([points (for/list ([(p e) (in-pcontext context)]) p)]
        [vars (program-variables (alt-program alt))])
    (parameterize ([*pcontext* context])
      (reap [sow]
            (sow (error-avg (alt-errors alt) points #:axis axis
                            #:vars vars #:color *red-theme*))
            (when combo
              (sow (error-avg (alt-errors alt) points #:axis axis
                              #:vars vars #:color *blue-theme*)))))))
;; Generate at most three or four children for the given alt at the
;; given location.
(define (expand-at-loc alt loc)
  (general-filter
   (append (taylor-filter (taylor-alt alt loc))
           (rewrite-filter (alt-rewrite-rm alt #:root loc)))))
;; Generate the list of steps hash objects representing the changes
;; between the parent and the child.
(define (make-steps child parent)
  (let steps-left ([cur child] [steps '()])
    (cond [(not cur) (error "The given parent is not a parent of the child!")]
          [(equal? cur parent)
           steps]
          [#t (steps-left
               (alt-prev cur)
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
  (parameterize ([*start-prog* (car alts)])
    (match-let ([`(,splitpoints ,involved-alts) (infer-splitpoints alts)])
      (if (= (length involved-alts) 1)
          (car involved-alts)
          (combine-alts splitpoints involved-alts)))))

;; =============== Lower level helper functions =============

;; Filter children
(define (general-filter alts) alts)
(define (taylor-filter alts) alts)
(define (rewrite-filter alts) alts)

;; Ranks a set of numbers by how well they group into clusters.
(define (cluster-rank xs)
  (for/sum ([idx (in-range (*num-scores*))])
    (k-means-score xs (*num-clusters*))))
;; Scores how well the given numbers can be clustered into
;; num-clusters clusters using k-means.
(define (k-means-score xs num-clusters)
  (let ([initial-means
         (for/list ([idx (in-range num-clusters)])
           (list-ref xs (random (length xs))))])
    (let loop ([means initial-means])
      (let* ([clustered-samples
              (for/list ([x xs])
                (cons x (argmin (λ (mean) (abs (- mean x))) means)))]
             [means* (for/list ([mean means])
                       (let ([cluster-xs (filter (compose (curry equal? mean) cdr) clustered-samples)])
                         (round (/ (apply + (map car cluster-xs)) (length cluster-xs)))))])
        (if (equal? means* means)
            (exact->inexact (/ (apply + (for/list ([sample clustered-samples])
                                          (sqr (- (car sample) (cdr sample)))))))
            (loop means*))))))
