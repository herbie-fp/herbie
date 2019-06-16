#lang racket

(require math/flonum math/bigfloat)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt" "../alternative.rkt" "../interface.rkt")

(provide localize-error)

(define (repeat c)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    c))

(define *analyze-cache* (make-hash))
(define *analyze-context* (make-parameter #f))

(define (localize-on-expression expr vars cache prec)
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? constant?)
                   (cons (repeat (->bf expr)) (repeat 1))]
                  [(? variable?)
                   (cons (map ->bf (dict-ref vars expr)) (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (localize-on-expression ift vars cache prec))]
                         [exact-iff (car (localize-on-expression iff vars cache prec))]
                         [exact-cond (for/list ([(p _) (in-pcontext (*pcontext*))])
				       ((eval-prog `(λ ,(map car vars) ,c) 'bf) p))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f ,args ...)
                   (define <-bf (representation-bf->repr (get-representation prec)))
                   (let* ([argvals
                           (flip-lists (map (compose car (curryr localize-on-expression vars cache prec)) args))]
                          [f-exact  (operator-info f 'bf)]
                          [f-approx (operator-info f 'fl)]
                          [exact  (map (curry apply f-exact) argvals)]
                          [approx (map (compose (curry apply f-approx) (curry map <-bf)) argvals)]
                          [error
                           (map (λ (ex ap) (+ 1 (abs (ulp-difference (<-bf ex) ap)))) exact approx)])
                     (cons exact error))]))))

(register-reset
 (λ ()
  (*analyze-context* (*pcontext*))
  (hash-clear! *analyze-cache*)))

(define (localize-error prog prec)
  (define varmap (map cons (program-variables prog)
		      (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))])
				    p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
        *analyze-cache*
        (make-hash)))
  (define expr->loc (location-hash prog))

  (localize-on-expression (program-body prog) varmap cache prec)

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
