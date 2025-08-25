#lang racket

(require racket/hash)
(require "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/pareto.rkt"
         "../syntax/types.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "batch.rkt"
         "points.rkt"
         "programs.rkt")

(provide (contract-out
          (make-alt-table (batch? pcontext? alt? any/c . -> . alt-table?))
          (atab-active-alts (alt-table? . -> . (listof alt?)))
          (atab-all-alts (alt-table? . -> . (listof alt?)))
          (atab-not-done-alts (alt-table? . -> . (listof alt?)))
          (atab-eval-altns (alt-table? batch? (listof alt?) context? . -> . (values any/c any/c)))
          (atab-add-altns (alt-table? (listof alt?) any/c any/c context? . -> . alt-table?))
          (atab-set-picked (alt-table? (listof alt?) . -> . alt-table?))
          (atab-completed? (alt-table? . -> . boolean?))
          (atab-min-errors (alt-table? . -> . (listof real?)))
          (alt-batch-costs (batch? representation? . -> . (batchref? . -> . real?)))))

;; Public API

(struct alt-table (point-idx->alts alt->point-idxs alt->done? alt->cost pcontext all) #:prefab)

(define (alt-batch-costs batch repr)
  (define node-cost-proc (platform-node-cost-proc (*active-platform*)))
  (batch-map batch
             (λ (get-args-costs node)
               (match node
                 [(? literal?) ((node-cost-proc node repr))]
                 [(? symbol?) ((node-cost-proc node repr))]
                 [(? number?) 0] ; specs
                 [(approx _ impl) (get-args-costs impl)]
                 [(list (? (negate impl-exists?) impl) args ...) 0] ; specs
                 [(list impl args ...)
                  (define cost-proc (node-cost-proc node repr))
                  (define itypes (impl-info impl 'itype))
                  (apply cost-proc (map get-args-costs args))]))))

(define (make-alt-table batch pcontext initial-alt ctx)
  (define cost ((alt-batch-costs batch (context-repr ctx)) (alt-expr initial-alt)))
  (define errs (batchref-errors (alt-expr initial-alt) pcontext ctx))
  (alt-table (for/vector #:length (pcontext-length pcontext)
                         ([err (in-list errs)])
               (list (pareto-point cost err (list initial-alt))))
             (hasheq initial-alt
                     (for/list ([idx (in-range (pcontext-length pcontext))])
                       idx))
             (hasheq initial-alt #f)
             (hasheq initial-alt cost)
             pcontext
             (list initial-alt)))

(define (atab-set-picked atab alts)
  (struct-copy alt-table
               atab
               [point-idx->alts (vector-copy (alt-table-point-idx->alts atab))]
               [alt->done?
                (for/fold ([alt->done? (alt-table-alt->done? atab)]) ([alt (in-list alts)])
                  (hash-set alt->done? alt #t))]))

(define (atab-completed? atab)
  (andmap (curry hash-ref (alt-table-alt->done? atab)) (hash-keys (alt-table-alt->point-idxs atab))))

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
  (order-altns (hash-keys (alt-table-alt->point-idxs atab))))

(define (atab-all-alts atab)
  (order-altns (alt-table-all atab)))

(define (atab-not-done-alts atab)
  (define altns (hash-keys (alt-table-alt->point-idxs atab)))
  (define not-done? (negate (curry hash-ref (alt-table-alt->done? atab))))
  (order-altns (filter not-done? altns)))

;; Implementation

(struct set-cover (removable coverage))

(define (atab->set-cover atab)
  (match-define (alt-table pnts->alts alts->pnts alt->done? alt->cost _ _) atab)
  (define tied (list->mutable-seteq (hash-keys alts->pnts)))
  (define coverage '())
  (for* ([pcurve (in-vector pnts->alts)]
         [ppt (in-list pcurve)])
    (match (pareto-point-data ppt)
      [(list) (error "This point has no alts which are best at it!" ppt)]
      [(list altn) (set-remove! tied altn)]
      [altns (set! coverage (cons (list->vector altns) coverage))]))
  (set-cover tied (list->vector coverage)))

(define (set-cover-remove! sc altn)
  (match-define (set-cover removable coverage) sc)
  (set-remove! removable altn)
  (for ([j (in-naturals)]
        [s (in-vector coverage)]
        #:when s)
    (define count 0)
    (define last #f)
    (for ([i (in-naturals)]
          [a (in-vector s)]
          #:when a)
      (cond
        [(eq? a altn) (vector-set! s i #f)]
        [a
         (set! count (add1 count))
         (set! last a)]))
    (when (= count 1)
      (vector-set! coverage j #f)
      (set-remove! removable last))))

(define ((removability<? atab) alt1 alt2)
  (define alt1-done? (hash-ref (alt-table-alt->done? atab) alt1))
  (define alt2-done? (hash-ref (alt-table-alt->done? atab) alt2))
  (cond
    [(and (not alt1-done?) alt2-done?) #t]
    [(and alt1-done? (not alt2-done?)) #f]
    [else
     (define alt1-num (length (hash-ref (alt-table-alt->point-idxs atab) alt1)))
     (define alt2-num (length (hash-ref (alt-table-alt->point-idxs atab) alt2)))
     (cond
       [(< alt1-num alt2-num) #t]
       [(> alt1-num alt2-num) #f]
       [else
        (define alt1-cost (hash-ref (alt-table-alt->cost atab) alt1))
        (define alt2-cost (hash-ref (alt-table-alt->cost atab) alt2))
        (cond
          [(< alt1-cost alt2-cost) #f]
          [(< alt2-cost alt1-cost) #t]
          [else (expr<? (alt-expr alt1) (alt-expr alt2))])])]))

(define (atab-prune atab)
  (define sc (atab->set-cover atab))
  (define removability (sort (set->list (set-cover-removable sc)) (removability<? atab)))
  (let loop ([removed '()]
             [removability removability])
    (match removability
      ['() (apply atab-remove* atab removed)]
      [(cons worst-alt other-alts)
       (cond
         [(set-member? (set-cover-removable sc) worst-alt)
          (set-cover-remove! sc worst-alt)
          (loop (cons worst-alt removed) other-alts)]
         [else (loop removed other-alts)])])))

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

(define (atab-remove* atab . altns)
  (match-define (alt-table point-idx->alts alt->point-idxs alt->done? alt->cost pctx _) atab)
  (define pnt-idx->alts*
    (for/vector #:length (vector-length point-idx->alts)
                ([pcurve (in-vector point-idx->alts)])
      (pareto-map (curry remq* altns) pcurve)))
  (struct-copy alt-table
               atab
               [point-idx->alts pnt-idx->alts*]
               [alt->point-idxs (hash-remove* alt->point-idxs altns)]
               [alt->done? (hash-remove* alt->done? altns)]
               [alt->cost (hash-remove* alt->cost altns)]))

(define (atab-eval-altns atab batch altns ctx)
  (define brfs (map alt-expr altns))
  (define errss (batch-errors batch brfs (alt-table-pcontext atab) ctx))
  (define costs (map (alt-batch-costs batch (context-repr ctx)) brfs))
  (values errss costs))

(define (atab-add-altns atab altns errss costs ctx)
  (define atab*
    (for/fold ([atab atab])
              ([altn (in-list altns)]
               [errs (in-list errss)]
               [cost (in-list costs)])
      (atab-add-altn atab altn errs cost ctx)))
  (define atab**
    (struct-copy alt-table atab* [alt->point-idxs (invert-index (alt-table-point-idx->alts atab*))]))
  (define atab*** (atab-prune atab**))
  (struct-copy alt-table
               atab***
               [alt->point-idxs (invert-index (alt-table-point-idx->alts atab***))]
               [all
                (set-union (alt-table-all atab) (hash-keys (alt-table-alt->point-idxs atab***)))]))

(define (invert-index point-idx->alts)
  (define alt->points* (make-hasheq))
  (for ([pcurve (in-vector point-idx->alts)]
        [idx (in-naturals)])
    (for* ([ppt (in-list pcurve)]
           [alt (in-list (pareto-point-data ppt))])
      (hash-update! alt->points* alt (λ (v) (cons idx v)) '())))
  (make-immutable-hasheq (hash->list alt->points*)))

(define (atab-add-altn atab altn errs cost ctx)
  (match-define (alt-table point-idx->alts alt->point-idxs alt->done? alt->cost pcontext _) atab)
  (define max-error (+ 1 (expt 2 (representation-total-bits (context-repr ctx)))))

  ;; Check  whether altn is already inserted into atab
  (match (hash-has-key? alt->point-idxs altn)
    [#f
     (define point-idx->alts*
       (for/vector #:length (vector-length point-idx->alts)
                   ([pcurve (in-vector point-idx->alts)]
                    [err (in-list errs)])
         (cond
           [(< err max-error) ; Only include points if they are valid
            (define ppt (pareto-point cost err (list altn)))
            (pareto-union (list ppt) pcurve #:combine append)]
           [else pcurve])))

     (alt-table point-idx->alts*
                (hash-set alt->point-idxs altn #f)
                (hash-set alt->done? altn #f)
                (hash-set alt->cost altn cost)
                pcontext
                #f)]
    [_ atab]))

(define (atab-min-errors atab)
  (define pnt-idx->alts (alt-table-point-idx->alts atab))
  (for/list ([idx (in-range (pcontext-length (alt-table-pcontext atab)))])
    (define curve (vector-ref pnt-idx->alts idx))
    ;; Curve is sorted so lowest error is first
    (pareto-point-error (first curve))))
