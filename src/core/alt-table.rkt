#lang racket

(require racket/hash)
(require "../utils/common.rkt"
         "../utils/alternative.rkt"
         "points.rkt"
         "programs.rkt"
         "../syntax/types.rkt"
         "../utils/pareto.rkt")

(provide (contract-out
          (make-alt-table (pcontext? alt? any/c . -> . alt-table?))
          (atab-active-alts (alt-table? . -> . (listof alt?)))
          (atab-all-alts (alt-table? . -> . (listof alt?)))
          (atab-not-done-alts (alt-table? . -> . (listof alt?)))
          (atab-eval-altns (alt-table? (listof alt?) context? . -> . (values any/c any/c)))
          (atab-add-altns (alt-table? (listof alt?) any/c any/c . -> . alt-table?))
          (atab-set-picked (alt-table? (listof alt?) . -> . alt-table?))
          (atab-completed? (alt-table? . -> . boolean?))
          (atab-min-errors (alt-table? . -> . (listof real?)))))

;; Public API

(struct alt-table (point->alts alt->points alt->done? alt->cost context all) #:prefab)

(define (backup-alt-cost altn)
  (let loop ([expr (alt-expr altn)])
    (match expr
      [(list 'if cond ift iff) (+ 1 (loop cond) (max (loop ift) (loop iff)))]
      [(list op args ...) (apply + 1 (map loop args))]
      [_ 1])))

; In normal mode, cost is not considered so we return a constant
; The alt table becomes "degenerate"
(define (alt-cost* altn repr)
  (if (*pareto-mode*) (alt-cost altn repr) 1))

(define (make-alt-table pcontext initial-alt ctx)
  (define cost (alt-cost* initial-alt (context-repr ctx)))
  (alt-table (make-immutable-hash (for/list ([(pt ex) (in-pcontext pcontext)]
                                             [err (errors (alt-expr initial-alt) pcontext ctx)])
                                    (cons pt (list (pareto-point cost err (list initial-alt))))))
             (hash initial-alt
                   (for/list ([(pt ex) (in-pcontext pcontext)])
                     pt))
             (hash initial-alt #f)
             (hash initial-alt cost)
             pcontext
             (list initial-alt)))

(define (atab-set-picked atab alts)
  (define new-done-table
    (for/fold ([table (alt-table-alt->done? atab)]) ([alt (in-list alts)])
      (hash-set table alt #t)))
  (struct-copy alt-table atab [alt->done? new-done-table]))

(define (atab-completed? atab)
  (andmap identity (hash-values (alt-table-alt->done? atab))))

;;
;; Extracting lists from sets or hash tables
;; need to be treated with care:
;;   - Internal hash tables and sets may cause
;;     non-deterministic behavior in ordering.
;;   - Need to sort to ensure some predictable order
;;
;; But why?? Still unclear.
;; If the conversion from seteq or hasheq to list is guarded
;; by sorting shouldn't everything else be deterministic???
;;
(define (order-altns altns)
  (sort altns expr<? #:key alt-expr))

(define (atab-active-alts atab)
  (order-altns (hash-keys (alt-table-alt->done? atab))))

(define (atab-all-alts atab)
  (order-altns (alt-table-all atab)))

(define (atab-not-done-alts atab)
  (order-altns (for/list ([(alt done?) (in-hash (alt-table-alt->done? atab))] #:unless done?)
                 alt)))

;; Implementation

(struct set-cover (removable coverage))

(define (atab->set-cover atab)
  (match-define (alt-table pnts->alts alts->pnts alt->done? alt->cost _ _) atab)
  (define tied (list->mutable-seteq (hash-keys alts->pnts)))
  (define coverage '())
  (for* ([pcurve (in-hash-values pnts->alts)] [ppt (in-list pcurve)])
    (match (pareto-point-data ppt)
      [(list) (error "This point has no alts which are best at it!" ppt)]
      [(list altn) (set-remove! tied altn)]
      [altns (set! coverage (cons (list->vector altns) coverage))]))
  (set-cover tied (list->vector coverage)))

(define (set-cover-remove! sc altn)
  (match-define (set-cover removable coverage) sc)
  (set-remove! removable altn)
  (for ([j (in-naturals)] [s (in-vector coverage)] #:when s)
    (define count 0)
    (define last #f)
    (for ([i (in-naturals)] [a (in-vector s)] #:when a)
      (cond
        [(eq? a altn) (vector-set! s i #f)]
        [a
         (set! count (add1 count))
         (set! last a)]))
    (when (= count 1)
      (vector-set! coverage j #f)
      (set-remove! removable last))))

(define (worst atab removable)
  ;; Metrics for "worst" alt
  (define (alt-num-points a)
    (length (hash-ref (alt-table-alt->points atab) a)))
  (define (alt-done? a)
    (if (hash-ref (alt-table-alt->done? atab) a) 1 0))
  (define (alt-cost a)
    (if (*pareto-mode*) (hash-ref (alt-table-alt->cost atab) a) (backup-alt-cost a)))
  ;; Rank by multiple metrics
  (define not-done (argmins alt-done? (set->list removable)))
  (define least-best-points (argmins alt-num-points not-done))
  (define worst-cost (argmaxs alt-cost least-best-points))
  ;; The set may have non-deterministic behavior,
  ;; so we can only rely on some total order
  (first (order-altns worst-cost)))

(define (atab-prune atab)
  (define sc (atab->set-cover atab))
  (let loop ([removed '()])
    (if (set-empty? (set-cover-removable sc))
        (apply atab-remove* atab removed)
        (let ([worst-alt (worst atab (set-cover-removable sc))])
          (set-cover-remove! sc worst-alt)
          (loop (cons worst-alt removed))))))

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

(define (atab-remove* atab . altns)
  (match-define (alt-table point->alts alt->points alt->done? alt->cost pctx _) atab)
  (define pnts->alts*
    (for/hash ([(pt curve) (in-hash point->alts)])
      (values pt (pareto-map (curry remq* altns) curve))))
  (struct-copy alt-table
               atab
               [point->alts pnts->alts*]
               [alt->points (hash-remove* alt->points altns)]
               [alt->done? (hash-remove* alt->done? altns)]
               [alt->cost (hash-remove* alt->cost altns)]))

(define (atab-eval-altns atab altns ctx)
  (define errss (flip-lists (batch-errors (map alt-expr altns) (alt-table-context atab) ctx)))
  (define costs (map (curryr alt-cost* (context-repr ctx)) altns))
  (values errss costs))

(define (atab-add-altns atab altns errss costs)
  (define-values (atab* progs*)
    (for/fold ([atab atab] [progs (list->set (map alt-expr (alt-table-all atab)))])
              ([altn (in-list altns)] [errs (in-list errss)] [cost (in-list costs)])
      ;; this is subtle, we actually want to check for duplicates
      ;; in terms of expressions, not alts: the default `equal?`
      ;; returns #f for the same expression with different derivations.
      (let ([prog (alt-expr altn)])
        (if (set-member? progs prog)
            (values atab progs)
            (values (atab-add-altn atab altn errs cost) (set-add progs prog))))))
  (define atab**
    (struct-copy alt-table atab* [alt->points (invert-index (alt-table-point->alts atab*))]))
  (define atab*** (atab-prune atab**))
  (struct-copy alt-table
               atab***
               [alt->points (invert-index (alt-table-point->alts atab***))]
               [all (set-union (alt-table-all atab) (hash-keys (alt-table-alt->points atab***)))]))

(define (invert-index idx)
  (define alt->points* (make-hasheq))
  (for* ([(pt curve) (in-hash idx)] [ppt (in-list curve)] [alt (in-list (pareto-point-data ppt))])
    (hash-set! alt->points* alt (cons pt (hash-ref alt->points* alt '()))))
  (make-immutable-hash (hash->list alt->points*)))

(define (atab-add-altn atab altn errs cost)
  (match-define (alt-table point->alts alt->points alt->done? alt->cost pcontext all-alts) atab)

  (define point->alts*
    (for/hash ([(pt ex) (in-pcontext pcontext)] [err (in-list errs)])
      (define ppt (pareto-point cost err (list altn)))
      (values pt (pareto-union (list ppt) (hash-ref point->alts pt)))))

  (alt-table point->alts*
             (hash-set alt->points altn #f)
             (hash-set alt->done? altn #f)
             (hash-set alt->cost altn cost)
             pcontext
             #f))

(define (atab-min-errors atab)
  (define pnt->alts (alt-table-point->alts atab))
  (for/list ([(pt ex) (in-pcontext (alt-table-context atab))])
    (define curve (hash-ref pnt->alts pt))
    ;; Curve is sorted so lowest error is first
    (pareto-point-error (first curve))))
