#lang racket

(require math/bigfloat
         racket/random)
(require "../config.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../utils/errors.rkt"
         "../utils/float.rkt"
         "../utils/pretty-print.rkt"
         "../syntax/types.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "compiler.rkt"
         "regimes.rkt"
         "rival.rkt"
         "sampling.rkt"
         "points.rkt"
         "programs.rkt")

(provide combine-alts
         (struct-out sp)
         regimes-pcontext-masks)

(module+ test
  (require rackunit))

;; A splitpoint (sp a b pt) means we should use alt a if b < pt
;; The last splitpoint uses +nan.0 for pt and represents the "else"
(struct sp (cidx bexpr point) #:prefab)

(define (combine-alts best-option start-prog ctx pcontext)
  (match-define (option splitindices alts pts expr _) best-option)
  (match splitindices
    [(list (si cidx _)) (list-ref alts cidx)]
    [_
     (timeline-event! 'bsearch)
     (define splitpoints (sindices->spoints pts expr alts splitindices start-prog ctx pcontext))

     (define expr*
       (for/fold ([expr (alt-expr (list-ref alts (sp-cidx (last splitpoints))))])
                 ([splitpoint (cdr (reverse splitpoints))])
         (define repr (repr-of (sp-bexpr splitpoint) ctx))
         (define if-impl (get-fpcore-impl 'if '() (list (get-representation 'bool) repr repr)))
         (define <=-impl (get-fpcore-impl '<= '() (list repr repr)))
         `(,if-impl (,<=-impl ,(sp-bexpr splitpoint)
                              ,(literal (repr->real (sp-point splitpoint) repr)
                                        (representation-name repr)))
                    ,(alt-expr (list-ref alts (sp-cidx splitpoint)))
                    ,expr)))

     ;; We don't want unused alts in our history!
     (define-values (alts* splitpoints*) (remove-unused-alts alts splitpoints))
     (define preprocessing (alt-preprocessing (first alts*)))
     (alt expr* (list 'regimes splitpoints*) alts* preprocessing)]))

(define (remove-unused-alts alts splitpoints)
  (for/fold ([alts* '()]
             [splitpoints* '()])
            ([splitpoint splitpoints])
    (define alt (list-ref alts (sp-cidx splitpoint)))
    ;; It's important to snoc the alt in order for the indices not to change
    (define alts** (remove-duplicates (append alts* (list alt))))
    (define splitpoint* (struct-copy sp splitpoint [cidx (index-of alts** alt)]))
    (define splitpoints** (append splitpoints* (list splitpoint*)))
    (values alts** splitpoints**)))

;; Invariant: (pred p1) and (not (pred p2))
(define (binary-search-floats pred p1 p2 repr)
  (cond
    [(<= (ulps->bits (ulp-difference p1 p2 repr)) (*binary-search-accuracy*))
     (timeline-push! 'stop "narrow-enough" 1)
     (values p1 p2)]
    [else
     (define p3 (midpoint p1 p2 repr))
     (define cmp
       ;; Sampling error: don't know who's better
       (with-handlers ([exn:fail:user:herbie:sampling? (const 'fail)])
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

(define (extract-subexpression expr var pattern ctx)
  (define body* (replace-expression expr pattern var))
  (define vars* (set-subtract (context-vars ctx) (free-variables pattern)))
  (and (subset? (free-variables body*) (cons var vars*)) body*))

(define (prepend-argument evaluator val pcontext)
  (define pts
    (for/list ([(pt ex) (in-pcontext pcontext)])
      pt))
  ; new-sampler returns: (cons (cons val pts) hint)
  ; Since the sampler does not call rival-analyze, the hint is set to #f
  (define (new-sampler)
    (values (vector-append (vector val) (random-ref pts)) #f))
  (define-values (results _) (batch-prepare-points evaluator new-sampler))
  (apply mk-pcontext results))

(define/reset *prepend-arguement-cache* (make-hash))
(define (cache-get-prepend v expr macro)
  (define key (cons expr v))
  (hash-ref! (*prepend-arguement-cache*) key (lambda () (macro v))))

(define (valid-splitpoints? splitpoints)
  (and (= (set-count (list->set (map sp-bexpr splitpoints))) 1) (nan? (sp-point (last splitpoints)))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define/contract (sindices->spoints points expr alts sindices start-prog ctx pcontext)
  (-> (listof vector?) any/c (listof alt?) (listof si?) any/c context? pcontext? valid-splitpoints?)
  (define repr (repr-of expr ctx))

  (define eval-expr (compile-prog expr ctx))

  (define var (gensym 'branch))
  (define ctx* (context-extend ctx var repr))
  (define progs (map (compose (curryr extract-subexpression var expr ctx) alt-expr) alts))
  (define start-prog-sub (extract-subexpression start-prog var expr ctx))

  ; Not totally clear if this should actually use the precondition
  (define start-real-compiler
    (and start-prog (make-real-compiler (list (prog->spec start-prog)) (list ctx*))))

  (define (prepend-macro v)
    (prepend-argument start-real-compiler v pcontext))

  (define (find-split expr1 expr2 v1 v2)
    (define (pred v)
      (define pctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (cache-get-prepend v expr prepend-macro)))
      (define acc1 (errors-score (errors expr1 pctx ctx*)))
      (define acc2 (errors-score (errors expr2 pctx ctx*)))
      (- acc1 acc2))
    (define-values (p1 p2) (binary-search-floats pred v1 v2 repr))
    (left-point p1 p2))

  (define (left-point p1 p2)
    (define left ((representation-repr->bf repr) p1))
    (define right ((representation-repr->bf repr) p2))
    (define out ; TODO: Try using bigfloat-pick-point here?
      (if (bfnegative? left)
          (bigfloat-interval-shortest left (bfmin (bf/ left 2.bf) right))
          (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))))
    ;; It's important to return something strictly less than right
    (if (bf= out right)
        p1
        ((representation-bf->repr repr) out)))

  (define use-binary
    (and (flag-set? 'reduce 'binary-search)
         ;; Binary search is only valid if we correctly extracted the branch expression
         (andmap identity (cons start-prog-sub progs))))

  (append (for/list ([si1 sindices]
                     [si2 (cdr sindices)])
            (define prog1 (list-ref progs (si-cidx si1)))
            (define prog2 (list-ref progs (si-cidx si2)))

            (define p1 (eval-expr (list-ref points (sub1 (si-pidx si1)))))
            (define p2 (eval-expr (list-ref points (si-pidx si1))))

            (define timeline-stop!
              (timeline-start! 'bstep (value->json p1 repr) (value->json p2 repr)))
            (define split-at
              (if use-binary
                  (find-split prog1 prog2 p1 p2)
                  (left-point p1 p2)))
            (timeline-stop!)

            (timeline-push! 'method (if use-binary "binary-search" "left-value"))
            (sp (si-cidx si1) expr split-at))
          (list (sp (si-cidx (last sindices)) expr +nan.0))))

(define (regimes-pcontext-masks pcontext splitpoints alts ctx)
  (define num-alts (length alts))
  (define bexpr (sp-bexpr (car splitpoints)))
  (define ctx* (struct-copy context ctx [repr (repr-of bexpr ctx)]))
  (define prog (compile-prog bexpr ctx*))

  (flip-lists (for/list ([(pt ex) (in-pcontext pcontext)])
                (define val (prog pt))
                (define alt-id
                  (for/first ([right (in-list splitpoints)]
                              #:when (or (equal? (sp-point right) +nan.0)
                                         (<=/total val (sp-point right) (context-repr ctx*))))
                    (sp-cidx right)))
                (build-list num-alts (curry = alt-id)))))
