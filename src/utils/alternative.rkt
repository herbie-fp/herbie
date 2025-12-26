#lang racket

(require "../syntax/platform.rkt"
         "../core/batch.rkt")
(provide (struct-out alt)
         (struct-out sp)
         make-alt
         alt-cost
         alt-map
         unbatchify-alts)

;; A splitpoint (sp a b pt) means we should use alt a if b < pt
;; The last splitpoint uses +nan.0 for pt and represents the "else"
(struct sp (cidx bexpr point) #:prefab)

;; Alts are an expression plus a derivation for it.

(struct alt (expr event prevs) #:prefab)

(define (make-alt expr)
  (alt expr 'start '()))

(define (alt-cost altn repr)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn) repr))

(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (curry alt-map f) (alt-prevs altn))])))

;; Converts batchrefs of altns into expressions, assuming that batchrefs refer to batch
(define (unbatchify-alts batch altns)
  (define exprs (batch-exprs batch))
  (define (unmunge-event event)
    (match event
      [(list 'evaluate (? batchref? start-expr)) (list 'evaluate (exprs start-expr))]
      [(list 'taylor (? batchref? start-expr) name var) (list 'taylor (exprs start-expr) name var)]
      [(list 'rr (? batchref? start-expr) (? batchref? end-expr) input proof)
       (list 'rr (exprs start-expr) (exprs end-expr) input proof)]
      [(list 'regimes splitpoints)
       (list 'regimes
             (for/list ([spt (in-list splitpoints)])
               (struct-copy sp spt [bexpr (exprs (sp-bexpr spt))])))]
      [_ event]))
  (define (unmunge altn)
    (define expr (alt-expr altn))
    (define expr*
      (if (batchref? expr)
          (exprs expr)
          expr))
    (define event* (unmunge-event (alt-event altn)))
    (struct-copy alt altn [expr expr*] [event event*]))
  (map (curry alt-map unmunge) altns))
