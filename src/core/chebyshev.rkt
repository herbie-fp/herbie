#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "../syntax/platform.rkt"
         "../core/alternative.rkt"
         "../core/programs.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../syntax/float.rkt"
         "../syntax/rival.rkt"
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

(define (build-clenshaw-poly! batch coeffs d var m r)
  (define t-brf (batch-add! batch `(/ (- ,var ,(real->double-flonum m)) ,(real->double-flonum r))))
  (define c0-half (real->double-flonum (* 0.5 (vector-ref coeffs 0))))
  (if (zero? d)
      (batch-add! batch c0-half)
      (let ([two-t-brf (batch-add! batch `(* 2.0 ,t-brf))])
        (let loop ([k d]
                   [b-k+1 (batch-add! batch 0.0)] ; b_{k+1}
                   [b-k+2 (batch-add! batch 0.0)]) ; b_{k+2}
          (if (zero? k)
              (let* ([tb1-brf (batch-add! batch `(* ,t-brf ,b-k+1))]
                     [tail-brf (batch-add! batch `(- ,tb1-brf ,b-k+2))])
                (batch-add! batch `(+ ,c0-half ,tail-brf)))
              (let* ([ck (real->double-flonum (vector-ref coeffs k))]
                     [sum-brf (batch-add! batch `(+ ,ck (* ,two-t-brf ,b-k+1)))]
                     [b-k (batch-add! batch `(- ,sum-brf ,b-k+2))])
                (loop (sub1 k) b-k b-k+1)))))))

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
     ;; TODO: hacky to account for 0 coefficients
     (define N (* 2 (*chebyshev-order-limit*)))

     (reap [sow]
           (for ([var (in-list vars)])
             (define intervals (hash-ref regime-intervals var '()))
             (define var-repr (context-lookup ctx var))

             ;; Must be univariate, non-polynomial, non-array
             (define filtered
               (for/list ([impl-brf (in-list brfs)]
                          [spec-brf (in-list spec-brfs)]
                          [repr (in-list reprs)]
                          [altn (in-list altns)]
                          #:when (not (array-representation? repr))
                          #:do [(define fv (free-vars-fn spec-brf))]
                          #:when (and (= (set-count fv) 1) (set-member? fv var))
                          #:do [(define reduced-spec-brf (reducer (copier spec-brf)))]
                          #:do [(define spec-expr (exprs-fn reduced-spec-brf))]
                          #:when (pair? spec-expr)
                          #:when (not (polynomial-expr? spec-expr var)))
                 (list impl-brf spec-brf reduced-spec-brf repr altn spec-expr)))

             (define seen-specs (make-hash))
             (define qualifying
               (for/list ([q (in-list filtered)]
                          #:do [(define spec-brf (second q))]
                          #:unless (hash-has-key? seen-specs spec-brf))
                 (hash-set! seen-specs spec-brf q)
                 q))

             (when (pair? qualifying)
               (define qualifying-spec-brfs (map third qualifying))
               (define qualifying-reprs (list->vector (map fourth qualifying)))
               (define n-exprs (length qualifying))
               (define evaluators
                 (for/vector #:length n-exprs
                             ([spec-brf (in-list qualifying-spec-brfs)]
                              [repr (in-vector qualifying-reprs)])
                   (parameterize ([*timeline-disabled* #t])
                     ;; Avoid invalid expressions invalidating the whole batch
                     (make-real-compiler spec-batch
                                         (list spec-brf)
                                         (list (context (list var) repr (list var-repr)))))))

               (define pt (vector #f))

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
                     (for ([i (in-range n-exprs)])
                       (define-values (status vals)
                         (parameterize ([*timeline-disabled* #t])
                           (real-apply (vector-ref evaluators i) pt)))
                       (define out
                         (if (and (equal? status 'valid) (pair? vals))
                             (repr->real (first vals) (vector-ref qualifying-reprs i))
                             +nan.0))
                       (vector-set! (vector-ref vals-matrix i)
                                    j
                                    (if (real? out)
                                        (exact->inexact out)
                                        +nan.0))))

                   ;; Generate polynomial alts per expression
                   (for ([q (in-list qualifying)]
                         [vals (in-vector vals-matrix)])
                     (match-define (list _ spec-brf _ repr altn spec-expr) q)
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
                             #:break (>= order (*chebyshev-order-limit*)))
                         (when (not (zero? (vector-ref coeffs k)))
                           (define poly-brf (build-clenshaw-poly! global-batch coeffs k var m r))
                           (define gen (approx spec-brf (hole (representation-name repr) poly-brf)))
                           (define brf (batch-add! global-batch gen))
                           (sow (alt brf `(taylor chebyshev ,var ,order) (list altn)))
                           (set! order (add1 order)))))))))))]))
