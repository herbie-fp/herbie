#lang racket

(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt"
         "../syntax/types.rkt" "../errors.rkt" "../points.rkt" "../float.rkt"
         "../compiler.rkt")
(require racket/trace)
(provide pareto-regimes infer-splitpoints (struct-out option) (struct-out split-index))

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
; (struct si (cidx pidx) #:prefab)
(struct split-index (cidx pidx) #:prefab)

(define (pareto-regimes sorted ctx)
  (define err-lsts (batch-errors (map alt-expr sorted) (*pcontext*) ctx))
  (let loop ([alts sorted] [errs (hash)])
    (cond
     [(null? alts) '()]
     [else
      (define-values (opt new-errs) 
        (infer-splitpoints alts #:errs errs ctx))
      (define high (split-index-cidx (argmax (λ (x) (split-index-cidx x)) (option-split-indices opt))))
      (cons opt (loop (take alts high) new-errs))])))

;; `infer-splitpoints` and `combine-alts` are split so the mainloop
;; can insert a timeline break between them.

(define (infer-splitpoints alts #:errs [cerrs (hash)] ctx)
  (define branch-exprs
    (if (flag-set? 'reduce 'branch-expressions)
        (exprs-to-branch-on alts ctx)
        (context-vars ctx)))
  (timeline-event! 'regimes)
  (timeline-push! 'inputs (map (compose ~a alt-expr) alts))
  (define err-lsts (batch-errors (map alt-expr alts) (*pcontext*) ctx))
  (define sorted-bexprs (sort branch-exprs (lambda (x y) (< (hash-ref cerrs x -1) (hash-ref cerrs y -1)))))

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
                    (~a (alt-expr (list-ref alts (split-index-cidx sidx))))))
  (define err-lsts* (flip-lists err-lsts))
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
   (λ (e) (equal? (type-of e ctx) 'real))
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
  ;; TODO split-indices will be sturct of 3 arrays
  (define out (option split-indices alts pts* expr (pick-errors split-indices pts* err-lsts* repr)))
  (timeline-stop!)
  (timeline-push! 'branch (~a expr) (errors-score (option-errors out)) (length split-indices) (~a (representation-name repr)))
  out)

(define/contract (pick-errors split-indices pts err-lsts repr)
  (->i ([sis (listof split-index?)] 
        [vss (r) (listof (listof (representation-repr? r)))]
        [errss (listof (listof real?))]
        [r representation?])
       [idxs (listof nonnegative-integer?)])
  (for/list ([i (in-naturals)] [pt pts] [errs (flip-lists err-lsts)])
    (for/first ([split-index split-indices] #:when (< i (split-index-pidx split-index)))
      (list-ref errs (split-index-cidx split-index)))))

;; TODO Zane will need to update this
(module+ test
  (define ctx (make-debug-context '(x)))
  (parameterize ([*start-prog* 1]
                 [*pcontext* (mk-pcontext '((0.5) (4.0)) '(1.0 1.0))])
    (define alts (map make-alt (list '(fmin.f64 x 1) '(fmax.f64 x 1))))
    (define err-lsts `((,(expt 2 53) 1) (1 ,(expt 2 53))))

    ;; This is a basic sanity test
    (check (λ (x y) (equal? (map split-index-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts 'x ctx)
           '(1 0))

    ;; This test ensures we handle equal points correctly. All points
    ;; are equal along the `1` axis, so we should only get one
    ;; splitpoint (the second, since it is better at the further point).
    (check (λ (x y) (equal? (map split-index-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '1 ctx)
           '(0))

    (check (λ (x y) (equal? (map split-index-cidx (option-split-indices x)) y))
           (option-on-expr alts err-lsts '(if (==.f64 x 0.5) 1 +nan.0) ctx)
           '(1 0))))

;; Struct representing an alternatives set of splitpoints that we are 
;; considering.
;; region-cost = The total error in the region to the left of our rightmost 
;; splitpoint
;; split-indexs = The split-indexs we are considering in this candidate.
(struct cse (region-cost split-indexs) #:transparent)

;; zane's weird output struct
(struct soa-cse (cses bests prev-k) #:transparent)

;; Given error-lsts, returns a list of sp objects representing where the 
;; optimal splitpoints are.
(define (valid-splitindices? can-split? split-indexs)
  (and
   (for/and ([pidx (map split-index-pidx (drop-right split-indexs 1))])
     (and (> pidx 0)) (list-ref can-split? pidx))
   (= (split-index-pidx (last split-indexs)) (length can-split?))))

;; TODO don't need alts we can recompute them
;; TODO not really sure what the actual return type is gonna be other then
;; TODO return 3 vectors
;; ???  only need AST with 3 nodes, don't remember why this matters and something about 5


;; alts-err-lsts is a list of lists [[]]
;; which holds the error of each alt on each sampled point
;; can-split-lst is a list [Bool]
(define (err-lsts->split-indices alts-err-lsts can-split-lst)
  ;; We have num-candidates candidates, each of whom has error lists of length num-points.
  ;; We keep track of the partial sums of the error lists so that we can easily find the cost of regions.
  ;; ??? What regions?

  ;; alts-err-lsts = candidates
  ;; candidates = alts
  (define number-of-alts (length alts-err-lsts))
  ;; an alt is a list of errors
  (define num-points (length (car alts-err-lsts)))
  (define min-weight num-points)
  ; (printf "~a\n" alts-err-lsts)

  ;; cost-of-regions is a list of vectors?
  (define cost-of-regions 
    (map (compose partial-sums list->vector) alts-err-lsts))
  ; (printf "~a\n" (vector? (first cost-of-regions)))

  (define can-split? (curry vector-ref (list->vector can-split-lst)))

  ;; maybe new return type instead 
  ;; TODO better names
  ;; TODO how long should these be?
  (define vector-size (+ num-points 1))
  (define best-costs (make-vector vector-size)) ;; floating point number?
  ;; best-alt-index?
  (define best-cost-indexs (make-vector vector-size)) ;; integer 
  (define prev-ks (make-vector vector-size)) ;; integer
  (define output-vector-index 0) 

  ;; Our intermediary data is a list of cse's,
  ;; where each cse represents the optimal splitindices after however many passes
  ;; if we only consider indices to the left of that cse's index.
  ;; Given one of these lists, this function tries to add another splitindices to each cse.
  (define (add-splitpoint sp-prev)
    ;; If there's not enough room to add another splitpoint, just pass the sp-prev along.
    (for/vector #:length num-points 
      ([current-point-idx-k (in-naturals)] [point-entry (in-vector sp-prev)])
      ; (printf "~a\n" (vector? sp-prev)) ;; DAG data structure
      ; (printf "~a\n" (struct? point-entry))
      ; (printf "~a\n" current-point-idx-k)
      ;; We take the CSE corresponding to the best choice of previous split point.
      ;; The default, not making a new split-point, gets a bonus of min-weight
      (let ([acost (- (cse-region-cost point-entry) min-weight)] 
            [aest point-entry])
        ; (printf "~a\n" aest) ;; what should I name this?
        (for ([prev-split-idx-l (in-range 0 current-point-idx-k)] 
              [prev-entry (in-vector sp-prev)]
              #:when (can-split? 
                      (split-index-pidx (car (cse-split-indexs prev-entry)))))
          ;; For each previous split point, we need the best candidate to fill the new regime
          (define first-diff
            (- (vector-ref (first cost-of-regions) current-point-idx-k)
               (vector-ref (first cost-of-regions) prev-split-idx-l)))
          (let ([best-index 0] [best-cost first-diff])
            ;; finds best cost/alt?
            (for ([current-index (in-naturals)] 
                  [cost-of-region (in-list cost-of-regions)])
              (define k-cost (vector-ref cost-of-region current-point-idx-k))
              (define l-cost (vector-ref cost-of-region prev-split-idx-l))
              (define cost (- k-cost l-cost))
                (when (< cost best-cost)
                  (set! best-cost cost)
                  (set! best-index current-index)))
            ;; 
            (when (and (< (+ (cse-region-cost prev-entry) best-cost) acost))
              (set! acost (+ (cse-region-cost prev-entry) best-cost))
              (define next-k (+ current-point-idx-k 1))
              (define current-split-point (split-index best-index next-k))
              ;; don't need this as we can recompute using split points
              (define current-alt (cse-split-indexs prev-entry))
               ;; TODO vector of cost?, acost?
              ;; so I think we need to save
              ;; best-cost, best-index, current-point-idx-k
              (vector-set! best-costs output-vector-index best-cost)
              (vector-set! best-cost-indexs output-vector-index best-index)
              (vector-set! prev-ks output-vector-index current-point-idx-k)
              (if (< output-vector-index num-points)
                (set! output-vector-index (add1 output-vector-index))
                empty)

              ;; cons current best to previous ones?
              (set! aest (cse acost (cons current-split-point
                                          current-alt))))))
        aest)))

  ;; We get the initial set of cse's by, at every point-index,
  ;; accumulating the candidates that are the best we can do
  ;; by using only one candidate to the left of that point.
  (define initial
    ;; our vector of structs. So this will need to be struct of 3 vectors.
    (for/vector #:length num-points ([point-idx (in-range num-points)])
      (argmin cse-region-cost
              ;; Consider all the candidates we could put in this region
              (map (λ (cand-idx cand-psums)
                      (let ([cost (vector-ref cand-psums point-idx)])
                        (cse cost (list (split-index cand-idx (+ point-idx 1))))))
                   (range number-of-alts)
                   cost-of-regions))))
  ;; We get the final splitpoints by applying add-splitpoints as many times as we want
  (define final
    (let loop ([prev initial])
      (let ([next (add-splitpoint prev)])
        (if (equal? prev next)
            next
            ;; when loop is "called" `next` gets set to `prev`
            (loop next)))))

  (define result-soa (soa-cse best-costs best-cost-indexs prev-ks))
  ; (printf "~a\n" result-soa)

  ;; Extract the splitpoints from our data structure (DAG?), and reverse it.
  (define output-list (cse-split-indexs (vector-ref final (- num-points 1))))
  ; (printf "~a\n" (first output-list))
  (reverse output-list)
  ;TODO return result-soa filled with values
  )

