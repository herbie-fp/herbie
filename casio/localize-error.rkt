#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)

(provide localize-error *analyze-points*)

(define (real-op->bigfloat-op op) (list-ref (hash-ref (*operations*) op) mode:bf))
(define (real-op->float-op op) (list-ref (hash-ref (*operations*) op) mode:fl))

(define (repeat c)
  (map (λ (x) c) (*points*)))

(define *analyze-cache* (make-hash))
(define *analyze-points* (make-parameter #f))

(define (localize-on-expression expr vars cache)
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? constant?)
                   (cons (repeat (->bf expr)) (repeat 1))]
                  [(? variable?)
                   (cons (map ->bf (cdr (assoc expr vars))) (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (localize-on-expression ift vars cache))]
                         [exact-iff (car (localize-on-expression iff vars cache))]
                         [exact-cond (map (eval-prog `(λ ,(map car vars) ,c) mode:bf) (*points*))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f ,args ...)
                   (let* ([argvals
                           (flip-lists (map (compose car (curryr localize-on-expression vars cache)) args))]
                          [f-exact  (real-op->bigfloat-op f)]
                          [f-approx (real-op->float-op f)]
                          [exact  (map (curry apply f-exact) argvals)]
                          [approx (map (compose (curry apply f-approx) (curry map ->flonum)) argvals)]
                          [error
                           (map (λ (ex ap) (+ 1 (abs (ulp-difference (->flonum ex)
								     (->flonum ap))))) exact approx)])
                     (cons exact error))]))))

(define (localize-error prog)
  (define varmap (map cons (program-variables prog) (flip-lists (*points*))))
  (define cache
    (if (eq? (*analyze-points*) (*points*))
        *analyze-cache*
        (make-hash)))
  (define expr->loc (location-hash prog))

  (localize-on-expression (program-body prog) varmap cache)

  (map cdr
       (take-up-to
        (sort
         (reap [sow]
               (for ([expr (hash-keys expr->loc)])
                 (let ([err (cdr (hash-ref! cache expr (λ () (localize-on-expression expr varmap cache))))]
                       [locs (hash-ref expr->loc expr)])
                   (when (ormap (curry < 1) err)
                     (map sow (map (curry cons err) locs))))))
         > #:key (compose errors-score car))
        3)))
