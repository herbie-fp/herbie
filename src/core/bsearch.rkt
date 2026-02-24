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
         "../syntax/batch.rkt"
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

(define (finish-combine-alts batch alts brf splitindices splitpoints ctx)
  (define splitpoints* (append splitpoints (list (sp (si-cidx (last splitindices)) brf +nan.0))))
  (define reprs (batch-reprs batch ctx))
  (define brf*
    (for/fold ([brf (alt-expr (list-ref alts (sp-cidx (last splitpoints*))))])
              ([splitpoint (cdr (reverse splitpoints*))])
      (define repr (reprs (sp-bexpr splitpoint)))
      (define if-impl (get-fpcore-impl 'if '() (list (get-representation 'bool) repr repr)))
      (define <=-impl (get-fpcore-impl '<= '() (list repr repr)))
      (define lit-brf
        (batch-add! batch
                    (literal (repr->real (sp-point splitpoint) repr) (representation-name repr))))
      (define cmp-brf (batch-add! batch `(,<=-impl ,(sp-bexpr splitpoint) ,lit-brf)))
      (batch-add! batch `(,if-impl ,cmp-brf ,(alt-expr (list-ref alts (sp-cidx splitpoint))) ,brf))))

  ;; We don't want unused alts in our history!
  (define-values (alts* splitpoints**) (remove-unused-alts alts splitpoints*))
  (alt brf* (list 'regimes splitpoints**) alts*))

(define (combine-alts batch best-option ctx)
  (match-define (option splitindices alts pts brf _) best-option)
  (define splitpoints (sindices->spoints/left batch pts brf splitindices ctx))
  (finish-combine-alts batch alts brf splitindices splitpoints ctx))

(define (combine-alts/binary batch best-option start-prog ctx pcontext)
  (match-define (option splitindices alts pts brf _) best-option)
  (define splitpoints
    (sindices->spoints/binary batch pts brf alts splitindices start-prog ctx pcontext))
  (finish-combine-alts batch alts brf splitindices splitpoints ctx))

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

(define (extract-subexpression batch brf var pattern-brf ctx)
  (define var-brf (batch-add! batch var))
  (if (= (batchref-idx pattern-brf) (batchref-idx var-brf))
      brf
      (let ()
        (define free-vars (batch-free-vars batch))
        (define body-brf (batch-replace-subexpr batch brf pattern-brf var-brf))
        (define vars* (set-subtract (list->set (context-vars ctx)) (free-vars pattern-brf)))
        (and (subset? (free-vars body-brf) (set-add vars* var)) body-brf))))

(define (deterministic-branch-var ctx)
  (define used-vars (list->set (context-vars ctx)))
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
  (define-values (results _) (batch-prepare-points evaluator new-sampler))
  (apply mk-pcontext results))

(define/reset *prepend-arguement-cache* (make-hash))
(define (cache-get-prepend v brf macro)
  (define key (cons brf v))
  (hash-ref! (*prepend-arguement-cache*) key (lambda () (macro v))))

;; Accepts a list of sindices in one indexed form and returns the
;; proper interior splitpoints in float form. A crucial constraint is that the
;; float form always come from the range [f(idx1), f(idx2)). If the
;; float form of a split is f(idx2), or entirely outside that range,
;; problems may arise.
(define/contract (sindices->spoints/left batch points brf sindices ctx)
  (-> batch? (listof vector?) batchref? (listof si?) context? (listof sp?))
  (define repr ((batch-reprs batch ctx) brf))
  (define eval-expr (compose (curryr vector-ref 0) (compile-batch batch (list brf) ctx)))

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

  (for/list ([si1 sindices]
             [si2 (cdr sindices)])
    (define p1 (eval-expr (list-ref points (sub1 (si-pidx si1)))))
    (define p2 (eval-expr (list-ref points (si-pidx si1))))

    (define timeline-stop! (timeline-start! 'bstep (value->json p1 repr) (value->json p2 repr)))
    (define split-at (left-point p1 p2))
    (timeline-stop!)

    (timeline-push! 'method "left-value")
    (sp (si-cidx si1) brf split-at)))

(define/contract (sindices->spoints/binary batch points brf alts sindices start-prog ctx pcontext)
  (-> batch?
      (listof vector?)
      batchref?
      (listof alt?)
      (listof si?)
      any/c
      context?
      pcontext?
      (listof sp?))
  (define repr ((batch-reprs batch ctx) brf))
  (define ulps (repr-ulps repr))
  (define eval-expr (compose (curryr vector-ref 0) (compile-batch batch (list brf) ctx)))
  (define brf-node (deref brf))
  (define var
    (if (symbol? brf-node)
        brf-node
        (deterministic-branch-var ctx)))
  (define ctx* (context-extend ctx var repr))
  (define progs
    (for/list ([alt (in-list alts)])
      (extract-subexpression batch (alt-expr alt) var brf ctx)))
  (define start-prog-sub (extract-subexpression batch start-prog var brf ctx))
  (unless (and start-prog-sub (andmap identity progs))
    (raise-user-error
     'sindices->spoints/binary
     "mainloop called binary splitpoint search without extractable critical subexpressions"))
  ; Not totally clear if this should actually use the precondition
  (define spec-brfs (batch-to-spec! batch (list start-prog)))
  (define start-real-compiler (make-real-compiler batch spec-brfs (list ctx*)))

  (define (prepend-macro v)
    (prepend-argument start-real-compiler v pcontext))

  (define (find-split si1 si2 p1 p2)
    (define brf1 (list-ref progs (si-cidx si1)))
    (define brf2 (list-ref progs (si-cidx si2)))
    (define (pred v)
      (define pctx
        (parameterize ([*num-points* (*binary-search-test-points*)])
          (cache-get-prepend v brf prepend-macro)))
      (match-define (list errs1 errs2) (batch-errors batch (list brf1 brf2) pctx ctx*))
      (- (errors-score errs1) (errors-score errs2)))
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
    (sp (si-cidx si1) brf split-at)))

(define (regimes-pcontext-masks pcontext splitpoints alts ctx)
  (define num-alts (length alts))
  (define num-points (pcontext-length pcontext))
  (define bexpr (sp-bexpr (car splitpoints)))
  (define ctx* (struct-copy context ctx [repr (repr-of bexpr ctx)]))
  (define prog (compile-prog bexpr ctx*))
  (define masks (build-vector num-alts (λ (_) (make-vector num-points #f))))
  (for ([(pt _) (in-pcontext pcontext)]
        [idx (in-naturals)])
    (define val (prog pt))
    (for/first ([right (in-list splitpoints)]
                #:when (or (equal? (sp-point right) +nan.0)
                           (<=/total val (sp-point right) (context-repr ctx*))))
      (vector-set! (vector-ref masks (sp-cidx right)) idx #t)))
  masks)
