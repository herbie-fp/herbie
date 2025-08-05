#lang racket

(require "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../syntax/platform.rkt"
         "../syntax/types.rkt"
         "alt-table.rkt"
         "bsearch.rkt"
         "batch.rkt"
         "derivations.rkt"
         "patch.rkt"
         "points.rkt"
         "preprocess.rkt"
         "programs.rkt"
         "regimes.rkt")

(provide run-improve!
         sort-alts)

;; The Herbie main loop goes through a simple iterative process:
;;
;; - Choose a subset of candidates
;; - Generating new candidates based on them
;; - Evaluate all the new and old candidates and prune to the best
;;
;; Each stage is stored in this global variable for REPL debugging.

(define/reset ^next-alts^ #f)
(define/reset ^patched^ #f)
(define/reset ^table^ #f)

;; Starting program for the current run
(define *start-prog* (make-parameter #f))
(define *pcontext* (make-parameter #f))
(define *preprocessing* (make-parameter '()))

;; These high-level functions give the high-level workflow of Herbie:
;; - Initial steps: explain, preprocessing, initialize the alt table
;; - the loop: choose some alts, localize, run the patch table, and finalize
;; - Final steps: regimes, derivations, and remove preprocessing

(define (run-improve! initial specification context pcontext)
  (timeline-event! 'preprocess)
  (define preprocessing (find-preprocessing specification context))
  (timeline-push! 'symmetry (map ~a preprocessing))
  (define pcontext* (preprocess-pcontext context pcontext preprocessing))
  (*pcontext* pcontext*)
  (*start-prog* initial)
  (*preprocessing* preprocessing)
  (define start-alt (alt initial 'start '()))
  (^table^ (make-alt-table pcontext start-alt context))

  (for ([iteration (in-range (*num-iterations*))]
        #:break (atab-completed? (^table^)))
    (run-iteration!))
  (define alternatives (extract!))
  (timeline-event! 'preprocess)
  (for/list ([altn alternatives])
    (define expr (alt-expr altn))
    (define expr* (compile-useful-preprocessing expr context pcontext (*preprocessing*)))
    (alt expr* 'add-preprocessing (list altn))))

(define (extract!)
  (timeline-push-alts! '())

  (define all-alts (atab-all-alts (^table^)))
  (define joined-alts (make-regime! all-alts (*start-prog*))) ;; HERE
  (define annotated-alts (add-derivations! joined-alts))

  (timeline-push! 'stop (if (atab-completed? (^table^)) "done" "fuel") 1)
  (map car (sort-alts annotated-alts)))

;; The next few functions are for interactive use in a REPL, usually for debugging
;; In Emacs, you can install racket-mode and then use C-c C-k to start that REPL

(define (list-alts)
  (printf "Key: [.] = done, [>] = chosen\n")
  (let ([ndone-alts (atab-not-done-alts (^table^))])
    (for ([alt (atab-active-alts (^table^))]
          [n (in-naturals)])
      (printf "~a ~a ~a\n"
              (cond
                [(set-member? (^next-alts^) alt) ">"]
                [(set-member? ndone-alts alt) " "]
                [else "."])
              (~r #:min-width 4 n)
              (alt-expr alt))))
  (printf "Error: ~a bits\n" (errors-score (atab-min-errors (^table^)))))

(define (choose-alt! n)
  (unless (< n (length (atab-active-alts (^table^))))
    (raise-user-error 'choose-alt!
                      "Couldn't select the ~ath alt of ~a (not enough alts)"
                      n
                      (length (atab-active-alts (^table^)))))
  (define picked (list-ref (atab-active-alts (^table^)) n))
  (^next-alts^ (list picked))
  (^table^ (atab-set-picked (^table^) (^next-alts^)))
  (void))

(define (inject-candidate! expr)
  (define new-alts (list (make-alt expr)))
  (define-values (errss costs) (atab-eval-altns (^table^) new-alts (*context*)))
  (^table^ (atab-add-altns (^table^) new-alts errss costs (*context*)))
  (void))

;; The rest of the file is various helper / glue functions used by
;; Herbie. These often wrap other Herbie components, but add logging
;; and timeline data.

(define (score-alt alt)
  (errors-score (errors (alt-expr alt) (*pcontext*) (*context*))))

; Pareto mode alt picking
(define (choose-mult-alts altns)
  (define repr (context-repr (*context*)))
  (cond
    [(< (length altns) (*pareto-pick-limit*)) altns] ; take max
    [else
     (define best (argmin score-alt altns))
     (define altns* (sort (set-remove altns best) < #:key (curryr alt-cost repr)))
     (define simplest (car altns*))
     (define altns** (cdr altns*))
     (define div-size (round (/ (length altns**) (- (*pareto-pick-limit*) 1))))
     (append (list best simplest)
             (for/list ([i (in-range 1 (- (*pareto-pick-limit*) 1))])
               (list-ref altns** (- (* i div-size) 1))))]))

(define (timeline-push-alts! picked-alts)
  (define fresh-alts (atab-not-done-alts (^table^)))
  (define repr (context-repr (*context*)))
  (for ([alt (atab-active-alts (^table^))])
    (timeline-push! 'alts
                    (~a (alt-expr alt))
                    (cond
                      [(set-member? picked-alts alt) "next"]
                      [(set-member? fresh-alts alt) "fresh"]
                      [else "done"])
                    (score-alt alt)
                    (~a (representation-name repr)))))

(define (choose-alts!)
  (define fresh-alts (atab-not-done-alts (^table^)))
  (define alts (choose-mult-alts fresh-alts))

  (timeline-push-alts! alts)
  (^next-alts^ alts)
  (^table^ (atab-set-picked (^table^) alts))
  (void))

;; Converts a patch to full alt with valid history
(define (reconstruct! global-batch alts)
  ;; extracts the base expressions of a patch as a batchref
  (define (get-starting-expr altn)
    (match (alt-prevs altn)
      [(list) (alt-expr altn)]
      [(list prev) (get-starting-expr prev)]))

  ;; takes a patch and converts it to a full alt
  (define (reconstruct-alt altn loc0 orig)
    (let loop ([altn altn])
      (match-define (alt _ event prevs) altn)
      (match event
        ['patch orig]
        [_
         (define event*
           (match event
             [(list 'evaluate) (list 'evaluate loc0)]
             [(list 'taylor name var) (list 'taylor loc0 name var)]
             [(list 'rr input proof) (list 'rr loc0 input proof)]))
         (define expr* (batch-location-set loc0 (alt-expr orig) (alt-expr altn)))
         (alt expr* event* (list (loop (first prevs))))])))

  (^patched^ (remove-duplicates
              (reap [sow]
                    (for ([altn (in-list alts)])
                      (define start-expr (get-starting-expr altn))
                      (for ([full-altn (in-list (^next-alts^))])
                        (define expr (alt-expr full-altn))
                        (sow (for/fold ([full-altn full-altn])
                                       ([loc (in-list (batch-get-locations expr start-expr))])
                               (reconstruct-alt altn loc full-altn))))))
              #:key (compose batchref-idx alt-expr)))

  (^patched^ (unbatchify-alts global-batch (^patched^)))
  ; No need to unmunge ^next-alts^
  (void))

;; Finish iteration
(define (finalize-iter!)
  (unless (^patched^)
    (raise-user-error 'finalize-iter! "No candidates ready for pruning!"))

  (timeline-event! 'eval)
  (define orig-all-alts (atab-active-alts (^table^)))
  (define orig-fresh-alts (atab-not-done-alts (^table^)))
  (define orig-done-alts (set-subtract orig-all-alts (atab-not-done-alts (^table^))))

  (define-values (errss costs) (atab-eval-altns (^table^) (^patched^) (*context*)))
  (timeline-event! 'prune)
  (^table^ (atab-add-altns (^table^) (^patched^) errss costs (*context*)))
  (define final-fresh-alts (atab-not-done-alts (^table^)))
  (define final-done-alts (set-subtract (atab-active-alts (^table^)) final-fresh-alts))
  (timeline-push! 'count
                  (+ (length (^patched^)) (length orig-fresh-alts) (length orig-done-alts))
                  (+ (length final-fresh-alts) (length final-done-alts)))

  (define data
    (hash 'new
          (list (length (^patched^)) (length (set-intersect (^patched^) final-fresh-alts)))
          'fresh
          (list (length orig-fresh-alts) (length (set-intersect orig-fresh-alts final-fresh-alts)))
          'done
          (list (- (length orig-done-alts) (length (or (^next-alts^) empty)))
                (- (length (set-intersect orig-done-alts final-done-alts))
                   (length (set-intersect final-done-alts (or (^next-alts^) empty)))))
          'picked
          (list (length (or (^next-alts^) empty))
                (length (set-intersect final-done-alts (or (^next-alts^) empty))))))
  (timeline-push! 'kept data)

  (define repr (context-repr (*context*)))
  (timeline-push! 'min-error
                  (errors-score (atab-min-errors (^table^)))
                  (format "~a" (representation-name repr)))
  (^next-alts^ #f)
  (^patched^ #f)
  (void))

(define (run-iteration!)
  (unless (^next-alts^)
    (choose-alts!))

  (define global-batch (progs->batch (map alt-expr (^next-alts^))))
  (define (make-batchref x idx)
    (struct-copy alt x [expr (batchref global-batch idx)]))

  (^next-alts^ (map make-batchref (^next-alts^) (vector->list (batch-roots global-batch))))
  (define roots (batch-alive-nodes global-batch #:condition node-is-impl?))
  (set-batch-roots! global-batch roots)

  (reconstruct! global-batch (generate-candidates global-batch))
  (finalize-iter!)
  (void))

(define (make-regime! alts start-prog)
  (define ctx (*context*))
  (define repr (context-repr ctx))

  (cond
    [(and (flag-set? 'reduce 'regimes)
          (> (length alts) 1)
          (equal? (representation-type repr) 'real)
          (not (null? (context-vars ctx)))
          (get-fpcore-impl 'if '() (list <bool> repr repr))
          (get-fpcore-impl '<= '() (list repr repr)))
     (define opts
       (pareto-regimes (sort alts < #:key (curryr alt-cost repr)) start-prog ctx (*pcontext*)))
     (for/list ([opt (in-list opts)])
       (combine-alts opt start-prog ctx (*pcontext*)))]
    [else (list (argmin score-alt alts))]))

(define (add-derivations! alts)
  (cond
    [(flag-set? 'generate 'proofs)
     (timeline-event! 'derivations)
     (add-derivations alts)]
    [else alts]))

(define (sort-alts alts [errss (batch-errors (map alt-expr alts) (*pcontext*) (*context*))])
  ;; sort everything by error + cost
  (define repr (context-repr (*context*)))
  (define alts-to-be-sorted (map cons alts errss))
  (sort alts-to-be-sorted
        (lambda (x y)
          (or (< (errors-score (cdr x)) (errors-score (cdr y))) ; sort by error
              (and (equal? (errors-score (cdr x))
                           (errors-score (cdr y))) ; if error is equal sort by cost
                   (< (alt-cost (car x) repr) (alt-cost (car y) repr)))))))
