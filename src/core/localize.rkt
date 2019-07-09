#lang racket

(require math/flonum math/bigfloat)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt" "../alternative.rkt")
(require "../interface.rkt" "../type-check.rkt")

(provide localize-error)

(define (repeat c)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    c))

(define *analyze-cache* (make-hash))
(define *analyze-context* (make-parameter #f))

(define (localize-on-expression expr vars cache)
  (define ctx
    (for/hash ([(var vals) (in-dict vars)])
      (values var (match (representation-name (infer-representation (first vals)))
                    [(or 'binary32 'binary64) 'real]
                    [x x]))))
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? constant?)
                   (cons (repeat (->bf expr)) (repeat 1))]
                  [(? variable?)
                   (cons (map ->bf (dict-ref vars expr)) (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (localize-on-expression ift vars cache))]
                         [exact-iff (car (localize-on-expression iff vars cache))]
                         [exact-cond (for/list ([(p _) (in-pcontext (*pcontext*))])
				       ((eval-prog `(λ ,(map car vars) ,c) 'bf) p))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f ,args ...)
                   (define <-bf (representation-bf->repr (get-representation* (type-of expr ctx))))
                   (define arg<-bfs
                     (for/list ([arg args])
                       (representation-bf->repr (get-representation* (type-of arg ctx)))))

                   (define argexacts
                     (flip-lists (map (compose car (curryr localize-on-expression vars cache)) args)))
                   (define argapprox
                     (for/list ([pt argexacts])
                       (for/list ([val pt] [arg<-bf arg<-bfs]) (arg<-bf val))))

                   (define exact (map (curry apply (operator-info f 'bf)) argexacts))
                   (define approx (map (curry apply (operator-info f 'fl)) argapprox))
                   (define repr (infer-double-representation (<-bf ex) ap))
                   (cons exact (map (λ (ex ap) (+ 1 (abs (ulp-difference (<-bf ex) ap repr)))) exact approx))]))))

(register-reset
 (λ ()
  (*analyze-context* (*pcontext*))
  (hash-clear! *analyze-cache*)))

(define (localize-error prog)
  (define varmap (map cons (program-variables prog)
		      (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))])
				    p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
        *analyze-cache*
        (make-hash)))
  (define expr->loc (location-hash prog))

  (localize-on-expression (program-body prog) varmap cache)

  (define locs
    (reap [sow]
          (for ([(expr locs) (in-hash expr->loc)]
                #:when (hash-has-key? cache expr))
            (define err (cdr (hash-ref cache expr)))
            (when (ormap (curry < 1) err)
              (for-each (compose sow (curry cons err)) locs)))))

  (take-up-to
   (sort locs > #:key (compose errors-score car))
   (*localize-expressions-limit*)))
