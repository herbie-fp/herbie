#lang racket

(require math/bigfloat)
(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt"
         "../syntax/types.rkt" "../errors.rkt" "../points.rkt" "../float.rkt")

(provide infer-splitpoints (struct-out option) (struct-out si))

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-builtins))

(struct option (split-indices alts pts expr errors) #:transparent
	#:methods gen:custom-write
        [(define (write-proc opt port mode)
           (display "#<option " port)
           (write (option-split-indices opt) port)
           (display ">" port))])

;; Struct representing a splitindex
;; cidx = Candidate index: the index candidate program that should be used to the left of this splitindex
;; pidx = Point index: The index of the point to the left of which we should split.
(struct si (cidx pidx) #:prefab)

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints alts ctx)
  (timeline-event! 'regimes)
  (timeline-push! 'inputs (map (compose ~a program-body alt-program) alts))
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts ctx)
        (program-variables (alt-program (first alts)))))
  (define err-lsts (batch-errors (map alt-program alts) (*pcontext*) ctx))
  (define options (for/list ([bexpr branch-exprs]) (option-on-expr alts err-lsts bexpr ctx)))
  (define best (argmin (compose errors-score option-errors) options))
  (timeline-push! 'count (length alts) (length (option-split-indices best)))
  (timeline-push! 'outputs
                  (for/list ([sidx (option-split-indices best)])
                    (~a (program-body (alt-program (list-ref alts (si-cidx sidx)))))))
  (define err-lsts* (flip-lists err-lsts))
  (timeline-push! 'baseline (apply min (map errors-score err-lsts*)))
  (timeline-push! 'accuracy (errors-score (option-errors best)))
  (define repr (context-repr ctx))
  (timeline-push! 'repr (~a (representation-name repr)))
  (timeline-push! 'oracle (errors-score (map (curry apply max) err-lsts)))
  best)

(define (exprs-to-branch-on alts ctx)
  (define alt-critexprs (map (compose all-critical-subexpressions alt-program) alts))
  (define start-critexprs (all-critical-subexpressions (*start-prog*)))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (filter
   (λ (e) (equal? (type-of e ctx) 'real))
   (set-intersect start-critexprs (apply set-union alt-critexprs))))
  
;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions prog)
  (define (subexprs-in-expr expr)
    (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) '())))
  (define prog-body (program-body prog))
  ;; We append program-variables here in case of (λ (x y) 0) or
  ;; similar, where the variables do not appear in the body but are
  ;; still worth splitting on
  (for/list ([expr (remove-duplicates (append (program-variables prog)
                                              (subexprs-in-expr prog-body)))]
             #:when (and (not (null? (free-variables expr)))
                         (critical-subexpression? prog-body expr)))
    expr))

(define (option-on-expr alts err-lsts expr ctx)
  (define repr (repr-of expr ctx))
  (define timeline-stop! (timeline-start! 'times (~a expr)))

  (define vars (program-variables (alt-program (first alts))))
  (define pts (for/list ([(pt ex) (in-pcontext (*pcontext*))]) pt))
  (define fn (eval-prog `(λ ,vars ,expr) 'fl ctx))
  (define splitvals (for/list ([pt pts]) (apply fn pt)))
  (define big-table ; val and errors for each alt, per point
    (for/list ([(pt ex) (in-pcontext (*pcontext*))] [err-lst err-lsts])
      (list* pt (apply fn pt) err-lst)))
  (match-define (list pts* splitvals* err-lsts* ...)
                (flip-lists (sort big-table (curryr </total repr) #:key second)))

  (define bit-err-lsts* (map (curry map ulps->bits) err-lsts*))

  (define can-split? (append (list #f)
                             (for/list ([val (cdr splitvals*)] [prev splitvals*])
                               (</total prev val repr))))
  (define split-indices (err-lsts->split-indices bit-err-lsts* can-split?))
  (define out (option split-indices alts pts* expr (pick-errors split-indices pts* err-lsts* repr)))
  (timeline-stop!)
  (timeline-push! 'branch (~a expr) (errors-score (option-errors out)) (length split-indices) (~a (representation-name repr)))
  out)

(define/contract (pick-errors split-indices pts err-lsts repr)
  (->i ([sis (listof si?)] 
        [vss (r) (listof (listof (representation-repr? r)))]
        [errss (listof (listof real?))]
        [r representation?])
       [idxs (listof nonnegative-integer?)])
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (for/first ([si split-indices] #:when (< i (si-pidx si)))
      (list-ref errs (si-cidx si)))))

(module+ test
  (define ctx (make-debug-context '(x)))
  (parameterize ([*start-prog* '(λ (x) 1)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map (λ (body) (make-alt `(λ (x) ,body))) (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
    (define err-lsts `((,(expt 2 53) 1) (1 ,(expt 2 53))))

    ;; This is a basic sanity test
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts 'x ctx)
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '1 ctx)
           '(0))

    (check (λ (x y) (equal? (map si-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '(if (==.f64 x 0.5) 1 +nan.0) ctx)
           '(1 0))))

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; indices = The si's we are considering in this candidate.
(struct cse (cost indices) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (valid-splitindices? can-split? split-indices)
  (and
   (for/and ([pidx (map si-pidx (drop-right split-indices 1))])
     (and (> pidx 0)) (list-ref can-split? pidx))
   (= (si-pidx (last split-indices)) (length can-split?))))

(define/contract (err-lsts->split-indices err-lsts can-split-lst)
  (->i ([e (listof list)] [cs (listof boolean?)]) [result (cs) (curry valid-splitindices? cs)])
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (length err-lsts))
  (define num-points (length (car err-lsts)))
  (define min-weight num-points)

  (define psums (map (compose partial-sums list->vector) err-lsts))
  (define can-split? (curry vector-ref (list->vector can-split-lst)))

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/vector #:length num-points ([point-idx (in-naturals)] [point-entry (in-vector sp-prev)])
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-cost point-entry) min-weight)] [aest point-entry])
        (for ([prev-split-idx (in-range 0 point-idx)] [prev-entry (in-vector sp-prev)]
              #:when (can-split? (si-pidx (car (cse-indices prev-entry)))))
          ;; For each previous split point, we need the best candidate to fill the new regime
          (let ([best #f] [bcost #f])
            (for ([cidx (in-naturals)] [psum (in-list psums)])
              (let ([cost (- (vector-ref psum point-idx)
                             (vector-ref psum prev-split-idx))])
                (when (or (not best) (< cost bcost))
                  (set! bcost cost)
                  (set! best cidx))))
            (when (and (< (+ (cse-cost prev-entry) bcost) acost))
              (set! acost (+ (cse-cost prev-entry) bcost))
              (set! aest (cse acost (cons (si best (+ point-idx 1))
                                          (cse-indices prev-entry)))))))
        aest)))

  ;; We get the initial set of cse's by, at every point-index,
  ;; accumulating the candidates that are the best we can do
  ;; by using only one candidate to the left of that point.
  (define initial
    (for/vector #:length num-points ([point-idx (in-range num-points)])
      (argmin cse-cost
              ;; Consider all the candidates we could put in this region
              (map (λ (cand-idx cand-psums)
                      (let ([cost (vector-ref cand-psums point-idx)])
                        (cse cost (list (si cand-idx (+ point-idx 1))))))
                   (range num-candidates)
                   psums))))

  ;; We get the final splitpoints by applying add-splitpoints as many times as we want
  (define final
    (let loop ([prev initial])
      (let ([next (add-splitpoint prev)])
        (if (equal? prev next)
            next
            (loop next)))))

  ;; Extract the splitpoints from our data structure, and reverse it.
  (reverse (cse-indices (vector-ref final (- num-points 1)))))

