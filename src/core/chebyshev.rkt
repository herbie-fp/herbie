#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "../syntax/platform.rkt"
         "../core/alternative.rkt"
         "../core/compiler.rkt"
         "../core/programs.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../syntax/float.rkt"
         "../config.rkt")

(provide chebyshev-alts)

(define (polynomial-expr? expr var)
  (match expr
    [(== var) #t]
    [(? number?) #t]
    [(list (or '+ '- '*) a b) (and (polynomial-expr? a var) (polynomial-expr? b var))]
    [(list 'neg a) (polynomial-expr? a var)]
    [(list 'pow base (? exact-integer?)) (polynomial-expr? base var)]
    [_ #f]))

(define (compute-chebyshev-coeffs values N)
  ;; t_j = cos(pi * (j + 1/2) / (N + 1))
  ;; theta_j = pi * (j + 1/2) / (N + 1)
  ;; f(t) ~= c_0 / 2 + sum(c_k * T_k(t))
  ;; c_k = (2 / (N + 1)) * sum(f(t_j) * cos(k * theta_j))
  (define denom (add1 N))
  (define scale (/ 2.0 denom))
  (for/vector #:length denom
              ([k (in-range denom)])
    (* scale
       (for/sum ([j (in-range denom)])
                (define theta (* pi (/ (+ j 0.5) denom)))
                (* (vector-ref values j) (cos (* k theta)))))))

(define (chebyshev->monomial coeffs d)
  ;; p(t) = c[0] / 2 + sum(c[k]*T_k(t)) = sum(a[i]*t^i)
  ;; T[0](t) = 1
  ;; T[1](t) = t
  ;; T[k](t) = 2 * t * T[k - 1](t) - T[k - 2](t)
  (define sz (add1 d))
  (define result (make-vector sz 0.0))
  (define prev (make-vector sz 0.0)) ; T[k - 2]
  (define curr (make-vector sz 0.0)) ; T[k - 1]
  (vector-set! prev 0 1.0)
  (vector-set! result 0 (* 0.5 (vector-ref coeffs 0)))
  (when (>= d 1)
    ;; T[1](t) = t
    (vector-set! curr 1 1.0)
    (vector-set! result 1 (+ (vector-ref result 1) (vector-ref coeffs 1)))
    (for ([k (in-range 2 sz)])
      (define next (make-vector sz 0.0))
      ;; T[k](t) = 2 * t * T[k - 1](t) - T[k - 2](t)
      (for ([i (in-range d)])
        (vector-set! next (add1 i) (+ (vector-ref next (add1 i)) (* 2.0 (vector-ref curr i)))))
      (for ([i (in-range sz)])
        (vector-set! next i (- (vector-ref next i) (vector-ref prev i))))
      (for ([i (in-range sz)])
        (vector-set! result
                     i
                     (+ (vector-ref result i) (* (vector-ref coeffs k) (vector-ref next i)))))
      (vector-copy! prev 0 curr)
      (vector-copy! curr 0 next)))
  result)

(define (build-horner-poly mono d var m r)
  (define t `(/ (- ,var ,(real->double-flonum m)) ,(real->double-flonum r)))
  (let loop ([k 0])
    (define ak (real->double-flonum (vector-ref mono k)))
    (if (= k d)
        ak
        `(+ ,ak (* ,t ,(loop (add1 k)))))))

(define (chebyshev-alts altns global-batch spec-batch reducer regime-intervals)
  (cond
    [(hash-empty? regime-intervals) '()]
    [else
     (define ctx (*context*))
     (define vars
       (for/list ([var (in-list (context-vars ctx))]
                  #:when (equal? (representation-type (context-lookup ctx var)) 'real))
         var))
     (define brfs (map alt-expr altns))
     (define reprs (map (batch-reprs global-batch ctx) brfs))
     (define spec-brfs (batch-to-spec! global-batch brfs))
     (define free-vars-fn (batch-free-vars global-batch))
     (define copier (batch-copy-only! spec-batch global-batch))
     (define exprs-fn (batch-exprs spec-batch))
     (define N (* 2 (*taylor-order-limit*)))

     (reap [sow]
           (for ([var (in-list vars)])
             (define intervals (hash-ref regime-intervals var '()))
             (define var-repr (context-lookup ctx var))

             ;; Must be univariate, non-polynomial, non-array
             (define qualifying
               (for/list ([impl-brf (in-list brfs)]
                          [spec-brf (in-list spec-brfs)]
                          [repr (in-list reprs)]
                          [altn (in-list altns)]
                          #:when (not (array-representation? repr))
                          #:do [(define fv (free-vars-fn spec-brf))]
                          #:when (and (= (set-count fv) 1) (set-member? fv var))
                          #:do [(define spec-expr (exprs-fn (reducer (copier spec-brf))))]
                          #:when (pair? spec-expr)
                          #:when (not (polynomial-expr? spec-expr var)))
                 (list impl-brf spec-brf repr altn spec-expr)))

             (when (pair? qualifying)
               (define qualifying-impl-brfs (map first qualifying))
               (define qualifying-reprs (list->vector (map third qualifying)))
               (define n-exprs (length qualifying))
               (define eval-ctx (context (list var) var-repr (list var-repr)))
               (define evaluator!
                 (parameterize ([*timeline-disabled* #t])
                   (compile-batch! global-batch qualifying-impl-brfs eval-ctx)))

               (define pt (vector #f))
               (define outs (make-vector n-exprs))

               (for ([interval (in-list intervals)])
                 (match-define (cons lo hi) interval)
                 ;; Constrain the max interval width
                 (when (and (< lo hi) (< (- hi lo) 1e6))
                   (define flo (exact->inexact lo))
                   (define fhi (exact->inexact hi))
                   ;; m = (lo + hi) / 2
                   ;; r = (hi - lo) / 2
                   (define m (/ (+ flo fhi) 2.0))
                   (define r (/ (- fhi flo) 2.0))
                   (define denom (add1 N))
                   (define nodes
                     (for/vector #:length denom
                                 ([j (in-range denom)])
                       ;; theta = pi * (j + 1/2) / (N + 1)
                       ;; x = m + r * cos(theta)
                       (define theta (* pi (/ (+ j 0.5) denom)))
                       (+ m (* r (cos theta)))))

                   ;; Evaluate expressions at all nodes
                   (define vals-matrix
                     (for/vector #:length n-exprs
                                 ([_ (in-range n-exprs)])
                       (make-vector denom +nan.0)))

                   (for ([j (in-range denom)])
                     (vector-set! pt 0 (real->repr (vector-ref nodes j) var-repr))
                     (evaluator! pt outs)
                     (for ([i (in-range n-exprs)])
                       (define val (repr->real (vector-ref outs i) (vector-ref qualifying-reprs i)))
                       (vector-set! (vector-ref vals-matrix i)
                                    j
                                    (if (real? val)
                                        (exact->inexact val)
                                        +nan.0))))

                   ;; Generate polynomial alts per expression
                   (for ([q (in-list qualifying)]
                         [vals (in-vector vals-matrix)])
                     (match-define (list _ spec-brf repr altn spec-expr) q)
                     (define valid?
                       (for/and ([v (in-vector vals)])
                         (and (real? v) (rational? v))))
                     (when valid?
                       (define coeffs0 (compute-chebyshev-coeffs vals N))
                       (define max-coeff
                         (for/fold ([mx 0.0]) ([c (in-vector coeffs0)])
                           (max mx (abs c))))
                       ;; Turn coefficients into 0 when smaller than 1e-12 * (max coefficient)
                       (define coeffs
                         (for/vector #:length (vector-length coeffs0)
                                     ([c (in-vector coeffs0)])
                           (if (> (abs c) (* 1e-12 max-coeff)) c 0.0)))

                       ;; Truncate polynomial to desired degree (ignoring 0 coefficients)
                       (define order 0)
                       (for ([k (in-range (add1 N))]
                             #:break (>= order (*taylor-order-limit*)))
                         (when (not (zero? (vector-ref coeffs k)))
                           (define mono (chebyshev->monomial coeffs k))
                           (define poly-expr (build-horner-poly mono k var m r))
                           (define poly-brf (batch-add! global-batch poly-expr))
                           (define gen (approx spec-brf (hole (representation-name repr) poly-brf)))
                           (define brf (batch-add! global-batch gen))
                           (sow (alt brf `(taylor chebyshev ,var ,order) (list altn)))
                           (set! order (add1 order)))))))))))]))
