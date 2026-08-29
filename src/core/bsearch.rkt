#lang racket

(require math/bigfloat
         racket/random)
(require "../config.rkt"
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../utils/errors.rkt"
         "../syntax/float.rkt"
         "../utils/pretty-print.rkt"
         "../syntax/types.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "../syntax/block.rkt"
         "compiler.rkt"
         "regimes.rkt"
         "../syntax/rival.rkt"
         "sampling.rkt"
         "points.rkt"
         "programs.rkt")

(provide combine-alts
         combine-alts/binary
         regimes-pcontext-masks)

(module+ test
  (require rackunit))

(define (finish-combine-alts block alts v splitindices splitpoints)
  (define splitpoints* (append splitpoints (list (sp (si-cidx (last splitindices)) v +nan.0))))
  (define v*
    (for/fold ([v (alt-expr (list-ref alts (sp-cidx (last splitpoints*))))])
              ([splitpoint (cdr (reverse splitpoints*))])
      (define repr (block-repr-of (sp-bexpr splitpoint)))
      (define if-impl (get-fpcore-impl 'if '() (list (get-representation 'bool) repr repr)))
      (define <=-impl (get-fpcore-impl '<= '() (list repr repr)))
      (define lit-v
        (block-add! block
                    (literal (repr->real (sp-point splitpoint) repr) (representation-name repr))))
      (define cmp-v (block-add! block (list <=-impl (sp-bexpr splitpoint) lit-v)))
      (block-add! block (list if-impl cmp-v (alt-expr (list-ref alts (sp-cidx splitpoint))) v))))

  ;; We don't want unused alts in our history!
  (define-values (alts* splitpoints**) (remove-unused-alts alts splitpoints*))
  (alt v* (list 'regimes splitpoints**) alts*))

(define (combine-alts block best-option)
  (match-define (option splitindices alts pts v) best-option)
  (define splitpoints (sindices->spoints/left block pts v splitindices))
  (finish-combine-alts block alts v splitindices splitpoints))

(define (combine-alts/binary block best-option start-prog pcontext)
  (match-define (option splitindices alts pts v) best-option)
  (define splitpoints (sindices->spoints/binary block pts v alts splitindices start-prog pcontext))
  (finish-combine-alts block alts v splitindices splitpoints))

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
(define (binary-search-floats pred p1 p2 repr ulps)
  (cond
    [(<= (ulps->bits (ulps p1 p2)) (*binary-search-accuracy*))
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
       [(negative? cmp) (binary-search-floats pred p3 p2 repr ulps)]
       [(positive? cmp) (binary-search-floats pred p1 p3 repr ulps)]
       ;; cmp = 0 usually means sampling failed, so we give up
       [else
        (timeline-push! 'stop "predicate-same" 1)
        (values p1 p2)])]))

(define (extract-subexpression block v pattern-v block* var-v)
  (define pattern-idx (val-idx pattern-v))
  (define var (val-def var-v))
  (define free-vars (block-free-vars block))
  (define vars* (set-subtract (list->set (block-vars block)) (free-vars pattern-v)))
  (define copy
    (block-recurse
     block
     (λ (v recurse)
       (cond
         [(= (val-idx v) pattern-idx) var-v]
         [else (block-push! block* (expr-recurse (val-def v) (compose val-idx recurse)))]))))
  (define body-v (copy v))
  (define free-vars* (block-free-vars block*))
  (and (subset? (free-vars* body-v) (set-add vars* var)) body-v))

(define (deterministic-branch-var block)
  (define used-vars (list->set (block-vars block)))
  (let loop ([n 0])
    (define var (string->symbol (format "branch-~a" n)))
    (if (set-member? used-vars var)
        (loop (add1 n))
        var)))

(define (prepend-argument evaluator val pcontext)
  (define pts
    (for/list ([(pt ex) (in-pcontext pcontext)])
      pt))
  ; new-sampler returns: (cons (cons val pts) hint)
  ; Since the sampler does not call rival-analyze, the hint is set to #f
  (define (new-sampler)
    (values (vector-append (vector val) (random-ref pts)) #f))
  (define-values (results _) (block-prepare-points evaluator new-sampler))
  (apply mk-pcontext results))

(define/reset *prepend-arguement-cache* (make-hash))
(define (cache-get-prepend v key-v macro)
  (define key (cons key-v v))
  (hash-ref! (*prepend-arguement-cache*) key (lambda () (macro v))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper interior splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define/contract (sindices->spoints/left block points v sindices)
  (-> block? (listof vector?) val? (listof si?) (listof sp?))
  (define repr (block-repr-of v))
  (define eval-expr (compose (curryr vector-ref 0) (compile-block block (list v))))

  (define ->bf (representation-repr->bf repr))
  (define bf-> (representation-bf->repr repr))

  (define (left-point p1 p2)
    (define left (->bf p1))
    (define right (->bf p2))
    (define out
      (if (bfnegative? left)
          (bigfloat-interval-shortest left (bfmin (bf/ left 2.bf) right))
          (bigfloat-interval-shortest left (bfmin (bf* left 2.bf) right))))
    ;; It's important to return something strictly less than right
    (if (bf= out right)
        p1
        (bf-> out)))

  ;; A sign change is the most common boundary, so a gap across zero splits at zero.
  (define (midpoint-threshold p1 p2)
    (define left (->bf p1))
    (define right (->bf p2))
    (cond
      [(or (bfnan? right) (bfinfinite? left)) (left-point p1 p2)]
      [(and (bfnegative? left) (bfpositive? right)) (real->repr 0 repr)]
      [else
       (define mid (->bf (midpoint p1 p2 repr)))
       (define split (bf-> (bigfloat-interval-shortest left mid)))
       ;; It's important to return something in [p1, p2)
       (if (and (<=/total p1 split repr) (</total split p2 repr))
           split
           (left-point p1 p2))]))

  (for/list ([si1 sindices]
             [si2 (cdr sindices)])
    (define p1 (eval-expr (list-ref points (sub1 (si-pidx si1)))))
    (define p2 (eval-expr (list-ref points (si-pidx si1))))

    (define timeline-stop! (timeline-start! 'bstep (value->json p1 repr) (value->json p2 repr)))
    (define split-at (midpoint-threshold p1 p2))
    (timeline-stop!)

    (timeline-push! 'method "midpoint")
    (sp (si-cidx si1) v split-at)))

(define/contract (sindices->spoints/binary block points target-v alts sindices start-prog pcontext)
  (-> block? (listof vector?) val? (listof alt?) (listof si?) any/c pcontext? (listof sp?))
  (define repr (block-repr-of target-v))
  (define ulps (repr-ulps repr))
  (define eval-expr (compose (curryr vector-ref 0) (compile-block block (list target-v))))
  (define v-node (val-def target-v))
  (define var
    (if (symbol? v-node)
        v-node
        (deterministic-branch-var block)))
  (define-values (block* var-v) (block-empty-extend block var repr))
  (define progs
    (for/list ([alt (in-list alts)])
      (extract-subexpression block (alt-expr alt) target-v block* var-v)))
  (define start-prog-sub (extract-subexpression block start-prog target-v block* var-v))
  (unless (and start-prog-sub (andmap identity progs))
    (raise-user-error
     'sindices->spoints/binary
     "mainloop called binary splitpoint search without extractable critical subexpressions"))
  (define spec-block (block-empty (context (block-vars block*) #f (block-var-reprs block*))))
  (define spec-vs (block-to-spec! block* spec-block (list start-prog-sub)))
  (define start-real-compiler (make-real-compiler spec-block spec-vs (list repr)))

  (define (prepend-macro v)
    (prepend-argument start-real-compiler v pcontext))

  (define (find-split si1 si2 p1 p2)
    (define v1 (list-ref progs (si-cidx si1)))
    (define v2 (list-ref progs (si-cidx si2)))
    (define eval-errors (compile-block block* (list v1 v2)))
    (define score-ulps (repr-ulps (block-repr-of v1)))
    (define (pred v)
      (define pctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (cache-get-prepend v target-v prepend-macro)))
      (for/sum ([(pt ex) (in-pcontext pctx)])
               (match-define (vector out1 out2) (eval-errors pt))
               (- (ulps->bits (score-ulps out1 ex)) (ulps->bits (score-ulps out2 ex)))))
    (define-values (bp1 _) (binary-search-floats pred p1 p2 repr ulps))
    bp1)

  (for/list ([si1 sindices]
             [si2 (cdr sindices)])
    (define p1 (eval-expr (list-ref points (sub1 (si-pidx si1)))))
    (define p2 (eval-expr (list-ref points (si-pidx si1))))

    (define timeline-stop! (timeline-start! 'bstep (value->json p1 repr) (value->json p2 repr)))
    (define split-at (find-split si1 si2 p1 p2))
    (timeline-stop!)

    (timeline-push! 'method "binary-search")
    (sp (si-cidx si1) target-v split-at)))

(define (regimes-pcontext-masks pcontext splitpoints alts ctx)
  (define num-alts (length alts))
  (define num-points (pcontext-length pcontext))
  (define bexpr (sp-bexpr (car splitpoints)))
  (define repr (repr-of bexpr ctx))
  (define ctx* (struct-copy context ctx [repr repr]))
  (define prog (compile-prog bexpr ctx*))
  (define masks (build-vector num-alts (λ (_) (make-vector num-points #f))))
  (for ([(pt _) (in-pcontext pcontext)]
        [idx (in-naturals)])
    (define val (prog pt))
    (for/first ([right (in-list splitpoints)]
                #:when (or (equal? (sp-point right) +nan.0) (<=/total val (sp-point right) repr)))
      (vector-set! (vector-ref masks (sp-cidx right)) idx #t)))
  masks)
