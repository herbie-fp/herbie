#lang racket

(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt"
         "../syntax/types.rkt" "../errors.rkt" "../points.rkt" "../float.rkt"
         "../compiler.rkt")
(require math/flonum)

(provide pareto-regimes (struct-out option) (struct-out si))

(module+ test
  (require rackunit "../load-plugin.rkt" "../syntax/syntax.rkt" "../syntax/sugar.rkt")
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

(define (pareto-regimes sorted ctx)
  (define err-lsts (flip-lists (batch-errors (map alt-expr sorted) (*pcontext*) ctx)))
  (let loop ([alts sorted] [errs (hash)] [err-lsts err-lsts])
    (cond
     [(null? alts) '()]
     ; Only return one option if not pareto mode
     [(and (not (*pareto-mode*)) (not (equal? alts sorted)))
      '()]
     [else
      (define-values (opt new-errs) 
        (infer-splitpoints alts err-lsts #:errs errs ctx))
      (define high (si-cidx (argmax (λ (x) (si-cidx x)) (option-split-indices opt))))
      (cons opt (loop (take alts high) new-errs (take err-lsts high)))])))

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints alts err-lsts* #:errs [cerrs (hash)] ctx)
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts ctx)
        (context-vars ctx)))
  (timeline-event! 'regimes)
  (timeline-push! 'inputs (map (compose ~a alt-expr) alts))
  (define sorted-bexprs (sort branch-exprs (lambda (x y) (< (hash-ref cerrs x -1) (hash-ref cerrs y -1)))))
  (define err-lsts (flip-lists err-lsts*))

  ;; invariant:
  ;; errs[bexpr] is some best option on branch expression bexpr computed on more alts than we have right now.
  (define-values (best best-err errs) 
      (for/fold ([best '()] [best-err +inf.0] [errs cerrs] 
            #:result (values best best-err errs)) 
            ([bexpr sorted-bexprs]
    ;; stop if we've computed this (and following) branch-expr on more alts and it's still worse
             #:break (> (hash-ref cerrs bexpr -1) best-err))
        (define opt (option-on-expr alts err-lsts bexpr ctx))
        (define err (+ (errors-score (option-errors opt)) (length (option-split-indices opt)))) ;; one-bit penalty per split
        (define new-errs (hash-set errs bexpr err))
        (if (< err best-err)
            (values opt err new-errs)
            (values best best-err new-errs))))

  (timeline-push! 'count (length alts) (length (option-split-indices best)))
  (timeline-push! 'outputs
                  (for/list ([sidx (option-split-indices best)])
                    (~a (alt-expr (list-ref alts (si-cidx sidx))))))
  (timeline-push! 'baseline (apply min (map errors-score err-lsts*)))
  (timeline-push! 'accuracy (errors-score (option-errors best)))
  (define repr (context-repr ctx))
  (timeline-push! 'repr (~a (representation-name repr)))
  (timeline-push! 'oracle (errors-score (map (curry apply max) err-lsts)))
  (values best errs))

(define (exprs-to-branch-on alts ctx)
  (define alt-critexprs
    (for/list ([alt (in-list alts)])
      (all-critical-subexpressions (alt-expr alt) ctx)))
  (define start-critexprs (all-critical-subexpressions (*start-prog*) ctx))
  ;; We can only binary search if the branch expression is critical
  ;; for all of the alts and also for the start prgoram.
  (filter
   (λ (e) (equal? (representation-type (repr-of e ctx)) 'real))
   (set-intersect start-critexprs (apply set-union alt-critexprs))))

;; Requires that expr is not a λ expression
(define (critical-subexpression? expr subexpr)
  (define crit-vars (free-variables subexpr))
  (define replaced-expr (replace-expression expr subexpr 1))
  (define non-crit-vars (free-variables replaced-expr))
  (set-disjoint? crit-vars non-crit-vars))

;; Requires that prog is a λ expression
(define (all-critical-subexpressions expr ctx)
  (define (subexprs-in-expr expr)
    (cons expr (if (list? expr) (append-map subexprs-in-expr (cdr expr)) '())))
  ;; We append all variables here in case of (λ (x y) 0) or similar,
  ;; where the variables do not appear in the body but are still worth
  ;; splitting on
  (for/list ([subexpr (remove-duplicates (append (context-vars ctx) (subexprs-in-expr expr)))]
             #:when (and (not (null? (free-variables subexpr)))
                         (critical-subexpression? expr subexpr)))
    subexpr))

(define (option-on-expr alts err-lsts expr ctx)
  (define repr (repr-of expr ctx))
  (define timeline-stop! (timeline-start! 'times (~a expr)))

  (define vars (context-vars ctx))
  (define pts (for/list ([(pt ex) (in-pcontext (*pcontext*))]) pt))
  (define fn (compile-prog expr ctx))
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
  (parameterize ([*start-prog* (literal 1 'binary64)]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
    (define err-lsts `((,(expt 2 53) 1) (1 ,(expt 2 53))))

    (define (test-regimes expr goal)
      (check (lambda (x y) (equal? (map si-cidx (option-split-indices x)) y))
             (option-on-expr alts err-lsts (spec->prog expr ctx) ctx)
             goal))

    ;; This is a basic sanity test
    (test-regimes 'x '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (test-regimes '1 '(0))

    (test-regimes '(if (== x 0.5) 1 NAN) '(1 0))))

;; Struct representing a candidate set of splitpoints that we are considering.
;; cost = The total error in the region to the left of our rightmost splitpoint
;; indices = The si's we are considering in this candidate.
(struct cse (cost indices) #:transparent)
;; TODO messy, delete me only used to setup data in (initial)
(struct cand (acost idx point-idx prev-idx) #:transparent)
;; Given error-lsts, returns a list of sp objects representing where the optimal splitpoints are.
(define (valid-splitindices? can-split? split-indices)
  (and
   (for/and ([pidx (map si-pidx (drop-right split-indices 1))])
     (and (> pidx 0)) (list-ref can-split? pidx))
   (= (si-pidx (last split-indices)) (length can-split?))))

(define/contract (err-lsts->split-indices err-lsts can-split)
  (->i ([e (listof list)] [cs (listof boolean?)]) 
       [result (cs) (curry valid-splitindices? cs)])
  ;; TODO add internal function
  (define can-split-vec (list->vector can-split))
  (define err-lsts-vec (list->vector err-lsts))
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  (define num-candidates (vector-length err-lsts-vec))
  (define num-points (length (vector-ref err-lsts-vec 0)))
  (define min-weight (fl num-points))

  ;; Vector version of psums, idk how to inline make-vec-psum yet
  (define (make-vec-psum lst) 
   (vector->flvector (partial-sums (list->vector lst))))
  (define flvec-psums (vector-map make-vec-psum err-lsts-vec))
  
  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint v-acost v-cidx v-aidx v-pidx)
    
    ;; output vectors
    (define vec-acost (make-flvector num-points))
    (define vec-cidx (make-vector num-points))
    (define vec-aidx (make-vector num-points))
    (define vec-pidx (make-vector num-points))

    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for ([cidx (in-naturals)] [psum (in-vector flvec-psums)])
      (for ([point-idx (in-range 0 num-points)])
        (define aest-cost (flvector-ref v-acost point-idx))
        (define aest-best (vector-ref v-cidx point-idx))
        (define aest-bidx (vector-ref v-aidx point-idx))
        (define aest-prev-idx (vector-ref v-pidx point-idx))
        ;; We take the CSE corresponding to the best choice of previous split point.
        ;; The default, not making a new split-point, gets a bonus of min-weight
        (let ([acost (- aest-cost min-weight)])
          (for ([prev-split-idx (in-range 0 point-idx)])
            ;; For each previous split point, we need the best candidate to fill the new regime
           (when
            (vector-ref can-split-vec (vector-ref v-aidx prev-split-idx))
            (let ([best #f] [bcost #f])
                (let ([cost (- (flvector-ref psum point-idx)
                               (flvector-ref psum prev-split-idx))])
                  (when (or (not best) (< cost bcost))
                    (set! bcost cost)
                    (set! best cidx)))
              (define temp (+ (flvector-ref v-acost prev-split-idx) bcost))
              (when
                (< temp acost)
                (set! acost temp)
                (set! aest-cost acost)
                (set! aest-best best)
                (set! aest-bidx (+ point-idx 1))
                (set! aest-prev-idx prev-split-idx)))))
          (flvector-set! vec-acost point-idx aest-cost)
          (vector-set! vec-cidx point-idx aest-best)
          (vector-set! vec-aidx point-idx aest-bidx)
          (vector-set! vec-pidx point-idx aest-prev-idx))))
  (values vec-acost vec-cidx vec-aidx vec-pidx))

  ;; We get the initial set of cse's by, at every point-index,
  ;; accumulating the candidates that are the best we can do
  ;; by using only one candidate to the left of that point.
  (define (initial)
    (define vec-acost (make-flvector num-points))
    (define vec-cidx (make-vector num-points))
    (define vec-aidx (make-vector num-points))
    (define vec-pidx (make-vector num-points))
    (for ([point-idx (in-range num-points)])
      ;; TODO this is kinda sloppy using #cand for only vector construction
      (define cse-min (vector-argmin cand-acost
        ;; Consider all the candidates we could put in this region
        ;; by sorting candidates
        (for/vector #:length num-candidates
          ([cand-idx (range num-candidates)] [cand-psums flvec-psums])
            (let ([cost (flvector-ref cand-psums point-idx)])
              (cand cost cand-idx (+ point-idx 1) num-points)))))
      (flvector-set! vec-acost point-idx (fl (cand-acost cse-min)))
      (vector-set! vec-cidx point-idx (cand-idx cse-min))
      (vector-set! vec-aidx point-idx (cand-point-idx cse-min))
      (vector-set! vec-pidx point-idx (cand-prev-idx cse-min)))
    (values vec-acost vec-cidx vec-aidx vec-pidx))

  ;; prefix of p is for previous
  ;; prefix of n is for next
  ;; prefix of f is for final result vectors
  ;; a for acost vectors
  ;; b for candidate index
  ;; c for alt index
  ;; d for previous index
  ;; This is where the high level bulk of the algorithm is applied
  ;; We get the final splitpoints by applying add-splitpoints as many times as we want
  (define-values (pa pb pc pd) (initial))
  (define-values (fa fb fc fd)
    (let loop ([pa pa] [pb pb] [pc pc] [pd pd])
      (define-values (na nb nc nd) (add-splitpoint pa pb pc pd))
      (if (equal? nb pb) ;; only need to compare candidate index
          (values na nb nc nd)
          (loop na nb nc nd))))
    
    ;; From here down is messy code translating from 4 vectors back to
    ;; the original list of split points
    (define fixed-final (make-vector num-points))
    (for ([idx (in-range 0 num-points)])
      (define a (flvector-ref fa idx))
      (define b (vector-ref fb idx))
      (define c (vector-ref fc idx))
      (define d (vector-ref fd idx))
      (vector-set! fixed-final idx (cand a b c d)))

  ;; start at (- num-points 1)
  ;; if num-points we are done
  ;; traversing and then reversing is bad
  (define (build-list current-cand)
    (cond 
      [(not(= (cand-prev-idx current-cand) num-points))
        (cons (si (cand-idx current-cand) (cand-point-idx current-cand))
               (build-list (vector-ref fixed-final (cand-prev-idx current-cand))))]
      [else 
        (cons (si (cand-idx current-cand) (cand-point-idx current-cand)) (list))]))
  
  (reverse (build-list (vector-ref fixed-final (- num-points 1)))))
