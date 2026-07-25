#lang typed/racket

(require/typed "../syntax/syntax.rkt"
               [#:struct literal ([value : Any] [precision : Any])]
               [#:struct approx ([spec : Any] [impl : Any])])
(require/typed "../syntax/batch.rkt"
               [#:opaque Batch batch?]
               [#:struct batchref ([batch : Batch] [idx : Index])])
(require typed/racket/unsafe)
(unsafe-require/typed "../syntax/platform.rkt"
                      [*active-platform* (Parameterof Any)]
                      [platform-cost-proc (-> Any (-> Any Real))])

(define-type Expr (U Symbol Boolean Real literal approx batchref (Listof Expr)))

(require/typed "../syntax/batch.rkt" [batch-exprs (-> Batch (-> batchref Expr))])

(unsafe-provide (struct-out alt) (struct-out sp) make-alt alt-cost alt-map unbatchify-alts)

;; A splitpoint (sp a b pt) means we should use alt a if b < pt
;; The last splitpoint uses +nan.0 for pt and represents the "else"
(struct sp ([cidx : Integer] [bexpr : Expr] [point : Any]) #:prefab)

;; Alts are an expression plus a derivation for it.

(struct alt ([expr : Expr] [event : Any] [prevs : (Listof alt)]) #:prefab)

(: make-alt (-> Expr alt))
(define (make-alt expr)
  (alt expr 'start '()))

(: alt-cost (-> alt Real))
(define (alt-cost altn)
  (define expr-cost (platform-cost-proc (*active-platform*)))
  (expr-cost (alt-expr altn)))

(: alt-map (-> (-> alt alt) alt alt))
(define (alt-map f altn)
  (f (struct-copy alt altn [prevs (map (lambda ([prev : alt]) (alt-map f prev)) (alt-prevs altn))])))

;; Converts batchrefs of altns into expressions, assuming that batchrefs refer to batch
(: unbatchify-alts (-> Batch (Listof alt) (Listof alt)))
(define (unbatchify-alts batch altns)
  (define exprs (batch-exprs batch))
  (: unmunge-splitpoint (-> Any sp))
  (define (unmunge-splitpoint spt)
    (define spt* (assert spt sp?))
    (sp (assert (sp-cidx spt*) exact-integer?)
        (exprs (assert (sp-bexpr spt*) batchref?))
        (sp-point spt*)))
  (define (unmunge-event event)
    (match event
      [(list 'evaluate (? batchref? start-expr)) (list 'evaluate (exprs start-expr))]
      [(list 'taylor (? batchref? start-expr) name var order)
       (list 'taylor (exprs start-expr) name var order)]
      [(list 'rr (? batchref? start-expr) (? batchref? end-expr) input proof)
       (define proof*
         (and proof (map (lambda ([brf : Any]) (exprs (assert brf batchref?))) (assert proof list?))))
       (list 'rr (exprs start-expr) (exprs end-expr) input proof*)]
      [(list 'regimes splitpoints)
       (list 'regimes (map unmunge-splitpoint (assert splitpoints list?)))]
      [_ event]))
  (: unmunge (-> alt alt))
  (define (unmunge altn)
    (define expr (alt-expr altn))
    (define expr*
      (if (batchref? expr)
          (exprs (assert expr batchref?))
          expr))
    (define event* (unmunge-event (alt-event altn)))
    (struct-copy alt altn [expr expr*] [event event*]))
  (map (curry alt-map unmunge) altns))
