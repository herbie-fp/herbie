#lang racket

(require math/bigfloat racket/random)
(require "../common.rkt" "../alternative.rkt" "../timeline.rkt" "../errors.rkt"
         "../syntax/types.rkt" "../syntax/syntax.rkt"
         "../programs.rkt" "../points.rkt" "regimes.rkt" "../float.rkt"
         "../pretty-print.rkt" "../ground-truth.rkt")

(provide combine-alts (struct-out sp) splitpoints->point-preds)

(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-builtins))

;; A splitpoint (sp a b pt) means we should use alt a if b < pt
;; The last splitpoint uses +nan.0 for pt and represents the "else"
(struct sp (cidx bexpr point) #:prefab)

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
  (cond
   [(<= (ulps->bits (ulp-difference p1 p2 repr)) (*max-bsearch-bits*))
    (timeline-push! 'stop "narrow-enough" 1)
    (values p1 p2)]
   [else
    (define p3 (midpoint p1 p2 repr))
    (define cmp
      ;; Sampling error: don't know who's better
      (with-handlers ([exn:fail:user:herbie? (const 'fail)])
        (pred p3)))

    (cond
     [(eq? cmp 'fail)
      (timeline-push! 'stop "predicate-failed" 1)
      (values p1 p2)]
     [(negative? cmp) (binary-search-floats pred p3 p2 repr)]
     [(positive? cmp) (binary-search-floats pred p1 p3 repr)]
     ;; cmp = 0 usually means sampling failed, so we give up
     [else
      (timeline-push! 'stop "predicate-same" 1)
      (values p1 p2)])]))

(define (extract-subexpression program var expr)
  (define body* (replace-expression (program-body program) expr var))
  (define vars* (set-subtract (program-variables program) (free-variables expr)))
  (if (subset? (free-variables body*) (cons var vars*))
      `(λ (,var ,@vars*) ,body*)
      #f))

(define (prepend-argument fn val pcontext ctx)
  (define pts (for/list ([(pt ex) (in-pcontext pcontext)]) pt))
  (define (new-sampler) (cons val (random-ref pts)))
  (apply mk-pcontext (cdr (batch-prepare-points fn ctx new-sampler))))

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
  (define precondition `(λ ,(program-variables start-prog) (TRUE)))
  (define start-fn (make-search-func precondition (list start-prog) ctx*))

  (define (find-split prog1 prog2 v1 v2)
    (define (pred v)
      (define pctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (prepend-argument start-fn v (*pcontext*) ctx*)))
      (define acc1 (errors-score (errors prog1 pctx ctx*)))
      (define acc2 (errors-score (errors prog2 pctx ctx*)))
      (- acc1 acc2))
    (define-values (p1 p2) (binary-search-floats pred v1 v2 repr))
    (left-point p1 p2))

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
           (find-split prog1 prog2 p1 p2)
           (left-point p1 p2)))
     (timeline-stop!)

     (timeline-push! 'method (if use-binary "binary-search" "left-value"))
     (sp (si-cidx si1) expr split-at))
   (list (sp (si-cidx (last sindices)) expr +nan.0))))

(define (valid-splitpoints? splitpoints)
  (and (= (set-count (list->set (map sp-bexpr splitpoints))) 1)
       (nan? (sp-point (last splitpoints)))))

(define/contract (splitpoints->point-preds splitpoints alts ctx)
  (-> valid-splitpoints? (listof alt?) context? (listof procedure?))

  (define bexpr (sp-bexpr (car splitpoints)))
  (define ctx* (struct-copy context ctx [repr (repr-of bexpr ctx)]))
  (define prog (eval-prog `(λ ,(context-vars ctx*) ,bexpr) 'fl ctx*))

  (for/list ([i (in-naturals)] [alt alts]) ;; alts necessary to terminate loop
    (λ (pt)
      (define val (apply prog pt))
      (for/first ([right splitpoints]
                  #:when (or (equal? (sp-point right) +nan.0)
                             (<=/total val (sp-point right) (context-repr ctx*))))
        ;; Note that the last splitpoint has an sp-point of +nan.0, so we always find one
        (equal? (sp-cidx right) i)))))

(module+ test
  (define context (make-debug-context '(x y)))
  (parameterize ([*start-prog* '(λ (x y) (/.f64 x y))])
    (define sps
      (list (sp 0 '(/.f64 y x) -inf.0)
            (sp 2 '(/.f64 y x) 0.0)
            (sp 0 '(/.f64 y x) +inf.0)
            (sp 1 '(/.f64 y x) +nan.0)))
    (match-define (list p0? p1? p2?)
                  (splitpoints->point-preds
                    sps
                    (map make-alt (build-list 3 (const '(λ (x y) (/ x y)))))
                    context))

    (check-pred p0? '(0.0 -1.0))
    (check-pred p2? '(-1.0 1.0))
    (check-pred p0? '(+1.0 1.0))
    (check-pred p1? '(0.0 0.0))))
