#lang racket

(require math/flonum)
(require math/bigfloat)
(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")
(require "alternative.rkt")

(provide localize-error localize-error-e2e *analyze-context*)

(define (repeat c)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    c))

(define *analyze-cache* (make-hash))
(define *analyze-context* (make-parameter #f))

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
                         [exact-cond (for/list ([(p _) (in-pcontext (*pcontext*))])
				       ((eval-prog `(λ ,(map car vars) ,c) mode:bf) p))])
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
  (define varmap (map cons (program-variables prog)
		      (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))])
				    p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
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
        (*localize-expressions-limit*))))

(define (list-with lst idx val)
  (if (= 0 idx)
      (cons val (cdr lst))
      (cons (car lst) (list-with (cdr lst) (sub1 idx) val))))

(define (localize-error-e2e prog)
  (define max-ulps (expt 2 (*bit-width*)))
  (define vars (map cons (program-variables prog)
                    (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))]) p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
        *analyze-cache*
        (make-hash)))
  (define (local-errors expr)
    (hash-ref! cache expr
               (λ ()
                 (match expr
                   [(? constant?) (list (repeat (->bf expr)))]
                   [(? variable?) (list (map ->bf (cdr (assoc expr vars))))]
                   [`(,f ,args ...)
                    (let ([arg-results (map local-errors args)])
                      (list*
                       ;; The first item in the return list here is
                       ;; the exact evaluation at this level, for each
                       ;; point.
                       (apply map (λ args (apply (real-op->bigfloat-op f) args))
                              (map car arg-results))
                       ;; The second item is what happens when we
                       ;; approximate it at this level, for each point
                       (cons '()
                             (apply map (λ args
                                          (->bf (apply (real-op->float-op f)
                                                       (map ->flonum args))))
                                    (map car arg-results)))
                       ;; The rest is the result of taking the
                       ;; approximations everywhere else, and
                       ;; propagating them up, with only one
                       ;; approximation per propagation.
                       (for/append ([arg args] [arg-result arg-results] [arg-idx (in-naturals 1)])
                         (for/list ([approximation (cdr arg-result)])
                           ;; Each approximation result is a pair of a
                           ;; location and a list of values.
                           (cons (cons arg-idx (car approximation))
                                 (apply map (λ args (apply (real-op->bigfloat-op f)
                                                           (list-with (cdr args)
                                                                      (sub1 arg-idx)
                                                                      (car args))))
                                        (cdr approximation)
                                        (map car arg-results)))))))]))))
                    
  (let ([result (local-errors (program-body prog))])
    (map (compose (curry cons 2) car)
         (take-up-to
          (sort (for/list ([approximate (cdr result)])
                  (cons (car approximate)
                        (errors-score
                         (for/list ([exact-at-pt (car result)]
                                    [approx-at-pt (cdr approximate)])
                           (let ([ex (->flonum exact-at-pt)]
                                 [out (->flonum approx-at-pt)])
                             (add1
                              (if (real? out)
                                  (abs (ulp-difference out ex))
                                  max-ulps)))))))
                >
                #:key cdr)
          (*localize-expressions-limit*)))))
                   
                            
                    
