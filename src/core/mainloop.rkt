#lang racket

(require "../config.rkt"
         "../core/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "alt-table.rkt"
         "bsearch.rkt"
         "../syntax/batch.rkt"
         "derivations.rkt"
         "patch.rkt"
         "points.rkt"
         "preprocess.rkt"
         "programs.rkt"
         "regimes.rkt"
         "batch-reduce.rkt")

(provide run-improve!
         sort-alts)

;; The Herbie main loop goes through a simple iterative process:
;;
;; - Choose all unfinished candidates
;; - Generating new candidates based on them
;; - Evaluate all the new and old candidates and prune to the best
;;
;; Each stage is stored in this global variable for REPL debugging.

(define/reset ^table^ #f)

;; Starting program for the current run
(define *start-brf* (make-parameter #f))
(define *pcontext* (make-parameter #f))
(define *preprocessing* (make-parameter '()))

(define *global-batch* (make-parameter #f))

;; These high-level functions give the high-level workflow of Herbie:
;; - Initial steps: explain, preprocessing, initialize the alt table
;; - the loop: choose some alts, localize, run the patch table, and finalize
;; - Final steps: regimes, derivations, and remove preprocessing

(define (run-improve! initial specification context pcontext)
  (timeline-event! 'preprocess)
  (define preprocessing
    (if (flag-set? 'setup 'preprocess)
        (find-preprocessing specification context)
        '()))
  (timeline-push! 'symmetry (map ~a preprocessing))
  (define pcontext* (preprocess-pcontext context pcontext preprocessing))
  (*pcontext* pcontext*)

  (parameterize ([*global-batch* (batch-empty)])
    (define global-spec-batch (batch-empty))
    (define spec-reducer (batch-reduce global-spec-batch))

    (*preprocessing* preprocessing)
    (define initial-brf (batch-add! (*global-batch*) initial))
    (*start-brf* initial-brf)
    (define start-alt (alt initial-brf 'start '()))
    (^table^ (make-alt-table (*global-batch*) pcontext start-alt context))

    (for ([_ (in-range (*num-iterations*))]
          #:break (atab-completed? (^table^)))
      (run-iteration! global-spec-batch spec-reducer))
    (define alternatives (extract!))
    (timeline-event! 'preprocess)
    (for/list ([altn alternatives])
      (define expr (alt-expr altn))
      (define expr* (compile-useful-preprocessing expr context pcontext (*preprocessing*)))
      (alt expr* 'add-preprocessing (list altn)))))

(define (extract!)
  (timeline-push-alts! '())
  (define all-alts (atab-all-alts (^table^)))
  (define joined-alts (make-regime! (*global-batch*) all-alts (*start-brf*)))
  (define annotated-alts (add-derivations! joined-alts))
  (define unbatched-alts (unbatchify-alts (*global-batch*) annotated-alts))

  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  (map car (sort-alts unbatched-alts)))

;; The rest of the file is various helper / glue functions used by
;; Herbie. These often wrap other Herbie components, but add logging
;; and timeline data.

(define (batch-score-alts altns)
  (map errors-score (batch-errors (*global-batch*) (map alt-expr altns) (*pcontext*) (*context*))))

(define (timeline-push-alts! next-alts)
  (define pending-alts (atab-not-done-alts (^table^)))
  (define active-alts (atab-active-alts (^table^)))
  (define scores (batch-score-alts active-alts))
  (define batch-jsexpr (batch->jsexpr (*global-batch*) (map alt-expr active-alts)))
  (define roots (hash-ref batch-jsexpr 'roots))
  (define repr (context-repr (*context*)))
  (timeline-push! 'alts-batch batch-jsexpr)
  (for ([alt (in-list active-alts)]
        [score (in-list scores)]
        [root (in-list roots)])
    (timeline-push! 'alts
                    root
                    (cond
                      [(set-member? next-alts alt) "next"]
                      [(set-member? pending-alts alt) "fresh"]
                      [else "done"])
                    score
                    (~a (representation-name repr)))))

(define (set-intersect-size keys set)
  (for/sum ([key (in-list keys)] #:when (set-member? set key)) 1))

(define (expr-recurse-impl expr f)
  (match expr
    [(approx _ impl) (f impl)]
    [_ (expr-recurse expr f)]))

;; Converts a patch to full alt with valid history
(define (reconstruct! starting-alts new-alts)
  (timeline-event! 'reconstruct)

  (define (compute-referrers parents root)
    (define seen (mutable-seteq))
    (define (recurse! cur)
      (unless (set-member? seen cur)
        (set-add! seen cur)
        (for-each recurse! (vector-ref parents cur))))
    (recurse! (batchref-idx root))
    seen)

  (define (reconstruct-alt altn orig can-refer)
    (define (loop altn)
      (match altn
        [(alt start-expr 'patch '()) (values orig start-expr)]
        [(alt cur-expr event (list prev))
         (define-values (prev-altn start-expr) (loop prev))
         (define event*
           (match event
             [(list 'evaluate) (list 'evaluate start-expr)]
             [(list 'taylor name var) (list 'taylor start-expr name var)]
             [(list 'rr input proof) (list 'rr (alt-expr prev) cur-expr input proof)]))
         (define expr* (batch-replace-subexpr batch (alt-expr orig) start-expr cur-expr can-refer))
         (values (alt expr* event* (list prev-altn)) start-expr)]))
    (define-values (result-alt _) (loop altn))
    result-alt)

  (define batch (*global-batch*))
  (define parents (make-vector (batch-length batch) '()))
  (define (walk-body brf recurse)
    (define idx (batchref-idx brf))
    (expr-recurse-impl (deref brf)
                       (lambda (child)
                         (define child-idx (batchref-idx child))
                         (vector-set! parents child-idx (cons idx (vector-ref parents child-idx)))
                         (recurse child)))
    (void))
  (for-each (batch-recurse batch walk-body) (map alt-expr starting-alts))
  (define grouped-alts (group-by get-starting-expr new-alts))

  (remove-duplicates
   (for*/list ([start-alts (in-list grouped-alts)]
               [can-refer (in-value (compute-referrers parents (get-starting-expr (car start-alts))))]
               [altn (in-list start-alts)]
               [full-altn (in-list starting-alts)]
               #:when (set-member? can-refer (batchref-idx (alt-expr full-altn))))
     (reconstruct-alt altn full-altn can-refer))
   #:key (compose batchref-idx alt-expr)))

;; Finish iteration
(define (finalize-iter! picked-alts patched)
  (timeline-event! 'eval)
  (define orig-all-alts (atab-active-alts (^table^)))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract orig-all-alts (atab-not-done-alts (^table^))))

  (define-values (errss costs) (atab-eval-altns (^table^) (*global-batch*) patched (*context*)))
  (timeline-event! 'prune)
  (^table^ (atab-add-altns (^table^) patched errss costs (*context*)))
  (define final-fresh-set (list->seteq (atab-not-done-alts (^table^))))
  (define final-active-set (list->seteq (atab-active-alts (^table^))))
  (define final-done-set (set-subtract final-active-set final-fresh-set))
  (timeline-push! 'count
                  (+ (length patched) (length orig-fresh-alts) (length orig-done-alts))
                  (+ (set-count final-fresh-set) (set-count final-done-set)))

  (define data
    (hash 'new
          (list (length patched) (set-intersect-size patched final-fresh-set))
          'fresh
          (list (length orig-fresh-alts) (set-intersect-size orig-fresh-alts final-fresh-set))
          'done
          (list (- (length orig-done-alts) (length picked-alts))
                (- (set-intersect-size orig-done-alts final-done-set)
                   (set-intersect-size picked-alts final-done-set)))
          'picked
          (list (length picked-alts) (set-intersect-size picked-alts final-done-set))))
  (timeline-push! 'kept data)

  (define repr (context-repr (*context*)))
  (timeline-push! 'min-error
                  (errors-score (atab-min-errors (^table^)))
                  (format "~a" (representation-name repr)))
  (void))

(define (run-iteration! global-spec-batch spec-reducer)
  (define pending-alts (atab-not-done-alts (^table^)))
  (timeline-push-alts! pending-alts)
  (^table^ (atab-set-picked (^table^) pending-alts))

  (define brfs (map alt-expr pending-alts))
  (define brfs* (batch-reachable (*global-batch*) brfs #:condition node-is-impl?))

  (define results (generate-candidates (*global-batch*) brfs* global-spec-batch spec-reducer))
  (define patched (reconstruct! pending-alts results))
  (finalize-iter! pending-alts patched)
  (void))

(define (make-regime! batch alts start-prog)
  (define ctx (*context*))
  (define repr (context-repr ctx))
  (define alt-costs (alt-batch-costs batch))

  (cond
    [(and (flag-set? 'reduce 'regimes)
          (> (length alts) 1)
          (equal? (representation-type repr) 'real)
          (not (null? (context-vars ctx)))
          (get-fpcore-impl 'if '() (list <bool> repr repr))
          (get-fpcore-impl '<= '() (list repr repr)))
     (define opts
       (pareto-regimes batch
                       (sort alts < #:key (compose (curryr alt-costs repr) alt-expr))
                       start-prog
                       ctx
                       (*pcontext*)))
     (for/list ([opt (in-list opts)])
       (match-define (option splitindices opt-alts _ brf _) opt)
       (timeline-event! 'bsearch)
       (define exprs (batch-exprs batch))
       (define branch-expr (exprs brf))
       (define use-binary?
         (and (flag-set? 'reduce 'binary-search)
              (> (length splitindices) 1)
              (critical-subexpression? (exprs start-prog) branch-expr)
              (for/and ([alt (in-list opt-alts)])
                (critical-subexpression? (exprs (alt-expr alt)) branch-expr))))
       (cond
         [(= (length splitindices) 1) (list-ref opt-alts (si-cidx (first splitindices)))]
         [use-binary? (combine-alts/binary batch opt start-prog ctx (*pcontext*))]
         [else (combine-alts batch opt ctx)]))]
    [else
     (define scores (batch-score-alts alts))
     (list (cdr (argmin car (map (λ (a s) (cons s a)) alts scores))))]))

(define (add-derivations! alts)
  (cond
    [(flag-set? 'generate 'proofs)
     (timeline-event! 'derivations)
     (add-derivations alts)]
    [else alts]))

(define (sort-alts alts [errss (exprs-errors (map alt-expr alts) (*pcontext*) (*context*))])
  ;; sort everything by error + cost
  (define repr (context-repr (*context*)))
  (define alts-to-be-sorted (map cons alts errss))
  (sort alts-to-be-sorted
        (lambda (x y)
          (or (< (errors-score (cdr x)) (errors-score (cdr y))) ; sort by error
              (and (equal? (errors-score (cdr x))
                           (errors-score (cdr y))) ; if error is equal sort by cost
                   (< (alt-cost (car x) repr) (alt-cost (car y) repr)))))))
