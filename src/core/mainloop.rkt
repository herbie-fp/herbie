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
         "../syntax/block.rkt"
         "derivations.rkt"
         "patch.rkt"
         "points.rkt"
         "compiler.rkt"
         "preprocess.rkt"
         "programs.rkt"
         "regimes.rkt"
         "reduce.rkt")

(provide run-improve!)

;; The Herbie main loop goes through a simple iterative process:
;;
;; - Choose all unfinished candidates
;; - Generating new candidates based on them
;; - Evaluate all the new and old candidates and prune to the best
;;
;; Each stage is stored in this global variable for REPL debugging.

(define/reset ^table^ #f)

;; Starting program for the current run
(define *start-v* (make-parameter #f))
(define *pcontext* (make-parameter #f))
(define *preprocessing* (make-parameter '()))

(define *global-block* (make-parameter #f))

;; These high-level functions give the high-level workflow of Herbie:
;; - Initial steps: explain, preprocessing, initialize the alt table
;; - the loop: choose some alts, localize, run the patch table, and finalize
;; - Final steps: regimes, derivations, and remove preprocessing

(define (run-improve! initial specification context pcontext #:sampler [sampler #f])
  (parameterize ([*global-block* (block-empty context)])
    (define global-spec-block (block-empty context))
    (define specification-v (block-add! global-spec-block specification))
    (define initial-v
      (match initial
        [(approx _ impl) (block-add! (*global-block*) (approx specification-v impl))]
        [_ (block-add! (*global-block*) initial)]))
    (timeline-event! 'preprocess)
    (define preprocessing
      (if (flag-set? 'setup 'preprocess)
          (find-preprocessing global-spec-block specification-v context)
          '()))
    (define-values (cover-sample pcontext*)
      (preprocess-pcontext context pcontext preprocessing #:sampler sampler))
    ;; If no covers were selected, the sample isn't modified; thus, use the original pcontext.
    (define train-pcontext (or cover-sample pcontext))
    (timeline-push! 'preprocessing (map ~a preprocessing))
    (*pcontext* pcontext*)

    (define spec-reducer (block-reduce global-spec-block))

    (*preprocessing* preprocessing)
    (*start-v* initial-v)
    (define start-alt (alt initial-v 'start '()))
    (^table^ (make-alt-table (*global-block*) train-pcontext start-alt))

    (for ([_ (in-range (*num-iterations*))]
          #:break (atab-completed? (^table^)))
      (run-iteration! global-spec-block spec-reducer))
    (define alternatives (extract! global-spec-block))
    (timeline-event! 'preprocess)
    ;; Combine the original and the taylor cover sample.
    (define validation-pcontext
      (if cover-sample
          (pcontext-append pcontext cover-sample)
          pcontext))
    (for/list ([altn alternatives])
      (define expr (alt-expr altn))
      (define expr* (compile-useful-preprocessing expr context validation-pcontext (*preprocessing*)))
      (alt expr* 'add-preprocessing (list altn)))))

(define (extract! spec-block)
  (timeline-push-alts! '() spec-block)
  (define all-alts (atab-all-alts (^table^)))
  (define joined-alts (make-regime! (*global-block*) all-alts (*start-v*) spec-block))
  (define annotated-alts (add-derivations! joined-alts))
  (define scores (block-errors (*global-block*) (map alt-expr annotated-alts) (*pcontext*)))
  (define sorted-alts (map car (sort-alts (*global-block*) annotated-alts scores)))
  (define unblocked-alts (unblockify-alts (*global-block*) sorted-alts spec-block))
  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  unblocked-alts)

;; The rest of the file is various helper / glue functions used by
;; Herbie. These often wrap other Herbie components, but add logging
;; and timeline data.

(define (dump-intermediates! block altns spec-block)
  (define dump-dir "dump-intermediates")
  (unless (directory-exists? dump-dir)
    (make-directory dump-dir))
  (define name
    (for/first ([i (in-naturals)]
                #:unless (file-exists? (build-path dump-dir (format "~a.rktd" i))))
      (build-path dump-dir (format "~a.rktd" i))))
  (define spec-f (block-exprs spec-block))
  (define exprs (block-exprs block #:spec-f spec-f))
  (call-with-output-file name
                         #:exists 'replace
                         (lambda (out)
                           (for ([altn (in-list altns)])
                             (writeln (exprs (alt-expr altn)) out)))))

(define (block-score-alts altns)
  (map errors-score (block-errors (*global-block*) (map alt-expr altns) (*pcontext*))))

(define (timeline-push-alts! next-alts spec-block)
  (define pending-alts (atab-not-done-alts (^table^)))
  (define active-alts (atab-active-alts (^table^)))
  (define scores (block-score-alts active-alts))
  (define block-jsexpr (block->jsexpr (*global-block*) spec-block (map alt-expr active-alts)))
  (define roots (hash-ref block-jsexpr 'roots))
  (define repr (context-repr (*context*)))
  (timeline-push! 'block block-jsexpr)
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

(define (taylor-record altn)
  (match altn
    [(alt _ `(taylor ,start-expr ,transform ,var ,order) prevs) altn]
    [(alt _
          `(rr ,start-expr ,end-expr ,input ,proof)
          (list (alt _ `(taylor ,prev-start-expr ,transform ,var ,order) prevs)))
     (car (alt-prevs altn))]
    [_ #f]))

;; Converts a patch to full alt with valid history
(define (reconstruct! starting-alts new-alts)
  (timeline-event! 'reconstruct)

  (define (group-equivalent-alts alts)
    (define fn (compile-block (*global-block*) (map alt-expr alts)))
    (define signatures (make-vector (length alts) '()))
    (define block-cost (alt-block-costs (*global-block*)))

    (for ([pt (in-vector (pcontext-points (*pcontext*)))])
      (define outs (fn pt))
      (for ([out (in-vector outs)]
            [idx (in-naturals)])
        (vector-set! signatures idx (cons out (vector-ref signatures idx)))))

    (define (best-alt alt1 alt2)
      (define cost1 (block-cost (alt-expr alt1)))
      (define cost2 (block-cost (alt-expr alt2)))
      (if (or (< cost1 cost2) (and (= cost1 cost2) (expr<? (alt-expr alt1) (alt-expr alt2))))
          alt1
          alt2))

    (define groups (make-hash))
    (for ([altn (in-list alts)]
          [signature (in-vector signatures)])
      (define key (cons (get-starting-expr altn) signature))
      (hash-update! groups key (curry best-alt altn) altn))

    (sort (hash-values groups) expr<? #:key alt-expr))

  (define (compute-referrers parents root)
    (define seen (mutable-seteq))
    (define (recurse! cur)
      (unless (set-member? seen cur)
        (set-add! seen cur)
        (for-each recurse! (vector-ref parents cur))))
    (recurse! (val-idx root))
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
             [(list 'taylor name var order) (list 'taylor start-expr name var order)]
             [(list 'rr input proof) (list 'rr (alt-expr prev) cur-expr input proof)]))
         (define expr* (block-replace-subexpr block (alt-expr orig) start-expr cur-expr can-refer))
         (values (alt expr* event* (list prev-altn)) start-expr)]))
    (define-values (result-alt _) (loop altn))
    result-alt)

  (define block (*global-block*))
  (define parents (make-vector (block-length block) '()))
  (define (walk-body v recurse)
    (define idx (val-idx v))
    (expr-recurse (val-def v)
                  (lambda (child)
                    (define child-idx (val-idx child))
                    (vector-set! parents child-idx (cons idx (vector-ref parents child-idx)))
                    (recurse child)))
    (void))
  (for-each (block-recurse block walk-body) (map alt-expr starting-alts))
  (define new-alts* (group-equivalent-alts new-alts))
  (timeline-push! 'count (length new-alts) (length new-alts*))
  (define grouped-alts (group-by get-starting-expr new-alts*))

  (remove-duplicates
   (for*/list ([start-alts (in-list grouped-alts)]
               [can-refer (in-value (compute-referrers parents (get-starting-expr (car start-alts))))]
               [altn (in-list start-alts)]
               [full-altn (in-list starting-alts)]
               #:when (set-member? can-refer (val-idx (alt-expr full-altn))))
     (reconstruct-alt altn full-altn can-refer))
   #:key (compose val-idx alt-expr)))

;; Finish iteration
(define (finalize-iter! picked-alts patched spec-block)
  (when (flag-set? 'dump 'intermediates)
    (dump-intermediates! (*global-block*) patched spec-block))
  (timeline-event! 'eval)
  (define orig-all-alts (atab-active-alts (^table^)))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract orig-all-alts (atab-not-done-alts (^table^))))

  (define-values (errss costs) (atab-eval-altns (^table^) (*global-block*) patched))
  (timeline-event! 'prune)
  (^table^ (atab-add-altns (^table^) patched errss costs))
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
  (define free-vars (block-free-vars (*global-block*)))
  (for ([altn (in-list patched)])
    (match (taylor-record altn)
      [(alt _ `(taylor ,start-expr ,transform ,var ,order) prevs)
       (define kept? (set-member? final-active-set altn))
       (define nvars (min (set-count (free-vars start-expr)) 2))
       (timeline-push! 'taylor-count (~a transform) order nvars 1 (if kept? 1 0))]
      [#f (void)]))

  (define repr (block-repr-of (*start-v*)))
  (timeline-push! 'min-error
                  (errors-score (atab-min-errors (^table^)))
                  (format "~a" (representation-name repr)))
  (void))

(define (run-iteration! global-spec-block spec-reducer)
  (define pending-alts (atab-not-done-alts (^table^)))
  (timeline-push-alts! pending-alts global-spec-block)
  (^table^ (atab-set-picked (^table^) pending-alts))

  (define vs (map alt-expr pending-alts))
  (define vs* (block-reachable (*global-block*) vs #:condition node-is-impl?))

  (define results (generate-candidates (*global-block*) vs* global-spec-block spec-reducer))
  (define patched (reconstruct! pending-alts results))
  (finalize-iter! pending-alts patched global-spec-block)
  (void))

(define (make-regime! block alts start-prog spec-block)
  (define repr (block-repr-of start-prog))
  (define alt-costs (alt-block-costs block))

  (cond
    [(and (flag-set? 'reduce 'regimes)
          (> (length alts) 1)
          (equal? (representation-type repr) 'real)
          (not (null? (block-vars block)))
          (get-fpcore-impl 'if '() (list <bool> repr repr))
          (get-fpcore-impl '<= '() (list repr repr)))
     (define opts
       (pareto-regimes block
                       (sort alts < #:key (compose alt-costs alt-expr))
                       start-prog
                       (*pcontext*)
                       spec-block))
     (for/list ([opt (in-list opts)])
       (match-define (option splitindices opt-alts _ v) opt)
       (timeline-event! 'bsearch)
       (define use-binary?
         (and (flag-set? 'reduce 'binary-search)
              (> (length splitindices) 1)
              (critical-subexpression? block start-prog v)
              (for/and ([alt (in-list opt-alts)])
                (critical-subexpression? block (alt-expr alt) v))))
       (cond
         [(= (length splitindices) 1) (list-ref opt-alts (si-cidx (first splitindices)))]
         [use-binary? (combine-alts/binary block opt start-prog (*pcontext*))]
         [else (combine-alts block opt)]))]
    [else
     (define scores (block-score-alts alts))
     (list (cdr (argmin car (map (λ (a s) (cons s a)) alts scores))))]))

(define (add-derivations! alts)
  (cond
    [(flag-set? 'generate 'proofs)
     (timeline-event! 'derivations)
     (add-derivations alts)]
    [else alts]))

(define (sort-alts block alts errss)
  ;; sort everything by error + cost
  (define alt-costs (alt-block-costs block))
  (define alts-to-be-sorted (map cons alts errss))
  (sort alts-to-be-sorted
        (lambda (x y)
          (or (< (errors-score (cdr x)) (errors-score (cdr y))) ; sort by error
              (and (equal? (errors-score (cdr x))
                           (errors-score (cdr y))) ; if error is equal sort by cost
                   (< (alt-costs (alt-expr (car x))) (alt-costs (alt-expr (car y)))))))))
