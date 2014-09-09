#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/simplify)

(provide analyze-local-error)

(define (real-op->bigfloat-op op) (list-ref (hash-ref operations op) mode:bf))
(define (real-op->float-op op) (list-ref (hash-ref operations op) mode:fl))

(define (repeat c)
  (map (λ (x) c) (*points*)))

(define (analyze-expression expr vars cache)
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? constant?)
                   (cons (repeat (->bf expr)) (repeat 1))]
                  [(? variable?)
                   (cons (map ->bf (cdr (assoc expr vars))) (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (analyze-expression ift vars cache))]
                         [exact-iff (car (analyze-expression iff vars cache))]
                         [exact-cond (map (eval-prog `(λ ,(map car vars) ,c) mode:bf) (*points*))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f ,args ...)
                   (let* ([argvals
                           (flip-lists (map (compose car (curryr analyze-expression vars cache)) args))]
                          [f-exact  (real-op->bigfloat-op f)]
                          [f-approx (real-op->float-op f)]
                          [exact  (map (curry apply f-exact) argvals)]
                          [approx (map (compose (curry apply f-approx) (curry map ->flonum)) argvals)]
                          [error
                           (map (λ (ex ap) (+ 1 (abs (flonums-between (->flonum ex) ap)))) exact approx)])
                     (cons exact error))]))))

(define (analyze-program prog)
  (define varmap (map cons (program-variables prog) (flip-lists (*points*))))
  (define cache (make-hash))
  (define expr->loc (location-hash prog))

  (analyze-expression (program-body prog) varmap cache)

  (map cdr
       (take-up-to
        (sort
         (reap [sow]
               (for ([expr (hash-keys cache)])
                 (let ([err (cdr (hash-ref cache expr))]
                       [locs (hash-ref expr->loc expr)])
                   (when (ormap (curry < 1) err)
                     (map sow (map (curry cons err) locs))))))
         > #:key (compose errors-score car))
        3)))

(define (analyze-local-error altn)
  (analyze-program (alt-program altn)))
