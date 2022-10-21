#lang racket

(require math/bigfloat)
(require "../common.rkt" "../alternative.rkt" "../programs.rkt" "../timeline.rkt"
         "../syntax/types.rkt" "../errors.rkt" "../points.rkt" "regimes.rkt" "../float.rkt"
         "../pretty-print.rkt" "../ground-truth.rkt")

(provide combine-alts)

(define (combine-alts best-option ctx)
  (match-define (option splitindices alts pts expr _) best-option)
  (match splitindices
   [(list (si cidx _)) (list-ref alts cidx)]
   [_
    (timeline-event! 'bsearch)
    (define splitpoints (sindices->spoints pts expr alts splitindices ctx))

    (define expr*
      (for/fold
          ([expr (program-body (alt-program (list-ref alts (sp-cidx (last splitpoints)))))])
          ([splitpoint (cdr (reverse splitpoints))])
        (define repr (repr-of (sp-bexpr splitpoint) ctx))
        (define <=-operator (get-parametric-operator '<= repr repr))
        `(if (,<=-operator ,(sp-bexpr splitpoint) ,(repr->real (sp-point splitpoint) repr))
             ,(program-body (alt-program (list-ref alts (sp-cidx splitpoint))))
             ,expr)))

    ;; We don't want unused alts in our history!
    (define-values (alts* splitpoints*) (remove-unused-alts alts splitpoints))
    (alt `(λ ,(program-variables (alt-program (first alts))) ,expr*)
         (list 'regimes splitpoints*) alts*)]))

(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* '()] [splitpoints* '()]) ([splitpoint splitpoints])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (index-of alts** alt)]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))

;; Invariant: (pred p1) and (not (pred p2))
(define (binary-search-floats pred p1 p2 repr)
  (let ([midpoint (midpoint p1 p2 repr)])
    (cond
     ; Avoid sampling on [+max, nan] at all costs
     ; (only works for floats, problematic since p1 and p2 are repr values)
     ; (this is handled by catching all sampling errors, so we can comment this out)
     ; [(nan? midpoint) p1]
     [(<= (ulp-difference p1 p2 repr) (expt 2 48))
      ((representation-bf->repr repr)
       (bigfloat-interval-shortest
        ((representation-repr->bf repr) p1)
        ((representation-repr->bf repr) p2)))]
     [else
      ; cmp usually equals 0 if sampling failed
      ; if so, give up and return the current midpoint
      (define cmp (pred midpoint))
      (cond
       [(negative? cmp) (binary-search-floats pred midpoint p2 repr)]
       [(positive? cmp) (binary-search-floats pred p1 midpoint repr)]
       [else ((representation-bf->repr repr)
              (bigfloat-interval-shortest
               ((representation-repr->bf repr) p1)
               ((representation-repr->bf repr) p2)))])])))

(define (extract-subexpression program var expr)
  (define body* (replace-expression (program-body program) expr var))
  (define vars* (set-subtract (program-variables program) (free-variables expr)))
  (if (subset? (free-variables body*) (cons var vars*))
      `(λ (,var ,@vars*) ,body*)
      #f))

(define (prepend-argument fn val pcontext ctx #:length length)
  (define (new-sampler)
    (define-values (pt ex) (random-ref (in-pcontext pcontext)))
    (cons val pt))
  (apply mk-pcontext
         (parameterize ([*num-points* length])
           (cdr (batch-prepare-points fn ctx new-sampler)))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define (sindices->spoints points expr alts sindices ctx)
  (define repr (repr-of expr ctx))

  (define eval-expr
    (eval-prog `(λ ,(program-variables (alt-program (car alts))) ,expr) 'fl ctx))

  (define var (gensym 'branch))
  (define ctx* (context-extend ctx var repr))
  (define progs (map (compose (curryr extract-subexpression var expr) alt-program) alts))
  (define start-prog (extract-subexpression (*start-prog*) var expr))

  ; Not totally clear if this should actually use the precondition
  (define start-fn (make-search-fn `(λ ,(program-variables start-prog) (TRUE)) ctx*))

  (define (find-split prog1 prog2 v1 v2)
    (define iters 0)

    (define best-guess #f)
    (define current-guess v1)
    (define sampling-fail? #f)

    (define (pred v)
      (set! iters (+ 1 iters))
      (set! best-guess current-guess)
      (set! current-guess v)
      (with-handlers ([exn:fail:user:herbie?
                       (λ (e) (set! sampling-fail? #t) 0)]) ; couldn't sample points
        (define pctx
          (prepend-argument start-fn v (*pcontext*) repr #:length (*binary-search-test-points*)))
          (define acc1 (errors-score (errors prog1 pctx ctx*)))
          (define acc2 (errors-score (errors prog2 pctx ctx*)))
          (- acc1 acc2)))
    (define pt (binary-search-floats pred v1 v2 repr))
    (when sampling-fail?
      (set! pt best-guess))
    pt)

  ; a little more rigorous than it sounds:
  ; finds the shortest number `x` near `p1` such that
  ; `x1` is in `[p1, p2]` and is no larger than
  ;  - if `p1` is negative, `p1 / 2`
  ;  - if `p1` is positive, `p1 * 2`
  (define (left-point p1 p2)
    (let ([left ((representation-repr->bf repr) p1)]
          [right ((representation-repr->bf repr) p2)])
      ((representation-bf->repr repr)
        (if (bfnegative? left)
            (bigfloat-interval-shortest left (bfmin (bf/ left 2.bf) right))
            (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))))))

  (define use-binary
    (and (flag-set? 'reduce 'binary-search)
         ;; Binary search is only valid if we correctly extracted the branch expression
         (andmap identity (cons start-prog progs))))
  
  (append
   (for/list ([si1 sindices] [si2 (cdr sindices)])
     (define prog1 (list-ref progs (si-cidx si1)))
     (define prog2 (list-ref progs (si-cidx si2)))

     (define p1 (apply eval-expr (list-ref points (sub1 (si-pidx si1)))))
     (define p2 (apply eval-expr (list-ref points (si-pidx si1))))

     (define timeline-stop! (timeline-start! 'bstep (value->json p1 repr) (value->json p2 repr)))
     (define split-at
       (if use-binary
           (with-handlers ([exn:fail:user:herbie:sampling? (const p1)])
             (find-split prog1 prog2 p1 p2))
           (left-point p1 p2)))
     (timeline-stop!)

     (timeline-push! 'method (if use-binary "binary-search" "left-value"))
     (sp (si-cidx si1) expr split-at))
   (list (sp (si-cidx (last sindices)) expr +nan.0))))
