#lang racket

(require math/flonum
         "../config.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "../syntax/platform.rkt"
         "../core/alternative.rkt"
         "../core/programs.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "../syntax/float.rkt"
         "../syntax/rival.rkt"
         "points.rkt")

(provide chebyshev-alts)

(struct error-cluster (start end selected) #:transparent)
(struct chebyshev-interval (source lo hi) #:transparent)

(define (finite-real? x)
  (and (real? x) (not (nan? x)) (not (infinite? x))))

(define (make-chebyshev-interval source lo hi)
  (and (finite-real? lo)
       (finite-real? hi)
       (< lo hi)
       (< (- hi lo) (*chebyshev-max-width*))
       (chebyshev-interval source lo hi)))

(define (chebyshev-interval-key interval)
  (list (chebyshev-interval-source interval)
        (chebyshev-interval-lo interval)
        (chebyshev-interval-hi interval)))

(define (polynomial-expr? expr var)
  (match expr
    [(== var) #t]
    [(? number?) #t]
    [(list (or '+ '- '*) a b) (and (polynomial-expr? a var) (polynomial-expr? b var))]
    [(list 'neg a) (polynomial-expr? a var)]
    [(list 'pow base (? exact-integer?)) (polynomial-expr? base var)]
    [_ #f]))

(define (point-value point var-idx var-repr)
  (repr->real (vector-ref point var-idx) var-repr))

(define (sorted-point-values pcontext var-idx var-repr)
  (sort (for/list ([point (in-vector (pcontext-points pcontext))]
                   [idx (in-naturals)]
                   #:do [(define value (point-value point var-idx var-repr))]
                   #:when (finite-real? value))
          (cons (exact->inexact value) idx))
        <
        #:key car))

(define (minimum-errors errss)
  (define n (flvector-length (first errss)))
  (for/flvector #:length n
                ([idx (in-range n)])
                (for/fold ([best +inf.0]) ([errs (in-list errss)])
                  (min best (flvector-ref errs idx)))))

(define (error-threshold point-values errs)
  (define sorted-errors
    (sort (for/list ([point-value (in-list point-values)])
            (flvector-ref errs (cdr point-value)))
          >))
  (define n (length sorted-errors))
  (define idx
    (inexact->exact (floor (min (sub1 n)
                                (max 0 (sub1 (ceiling (* n (*chebyshev-error-fraction*)))))))))
  (list-ref sorted-errors idx))

(define (candidate-error-clusters point-values errs)
  (define threshold (error-threshold point-values errs))
  (define max-gap (*chebyshev-gap-points*))
  (define-values (clusters current start last-selected selected gap)
    (for/fold ([clusters '()]
               [current #f]
               [start #f]
               [last-selected #f]
               [selected 0]
               [gap 0])
              ([point-value (in-list point-values)]
               [pos (in-naturals)])
      (define err (flvector-ref errs (cdr point-value)))
      (define selected? (>= err threshold))
      (match* (current selected?)
        [(#f #t) (values clusters #t pos pos 1 0)]
        [(#f #f) (values clusters #f #f #f 0 0)]
        [(#t #t) (values clusters #t start pos (add1 selected) 0)]
        [(#t #f)
         (cond
           [(< gap max-gap) (values clusters #t start last-selected selected (add1 gap))]
           [else
            (values (cons (error-cluster start last-selected selected) clusters) #f #f #f 0 0)])])))
  (define clusters*
    (if current
        (cons (error-cluster start last-selected selected) clusters)
        clusters))
  (reverse clusters*))

(define (cluster->intervals point-values cluster)
  (match-define (error-cluster start end selected) cluster)
  (define n (length point-values))
  (define padding (*chebyshev-padding-points*))
  (define min-points (*chebyshev-min-points*))
  (define target-points (*chebyshev-target-points*))
  (define start* (max 0 (- start padding)))
  (define end* (min (sub1 n) (+ end padding)))
  (define total-points (add1 (- end* start*)))
  (cond
    [(or (< selected min-points) (< total-points min-points)) '()]
    [else
     (define chunks (max 1 (ceiling (/ total-points target-points))))
     (remove-duplicates
      (filter identity
              (for/list ([chunk (in-range chunks)]
                         #:do [(define lo-pos (+ start* (floor (/ (* chunk total-points) chunks))))
                               (define hi-pos
                                 (+ start* (sub1 (floor (/ (* (add1 chunk) total-points) chunks)))))
                               (define lo (car (list-ref point-values lo-pos)))
                               (define hi (car (list-ref point-values hi-pos)))])
                (and (< lo-pos hi-pos) (make-chebyshev-interval 'error lo hi))))
      #:key chebyshev-interval-key)]))

(define (chebyshev-intervals point-values errs)
  (remove-duplicates (append-map (curry cluster->intervals point-values)
                                 (candidate-error-clusters point-values errs))
                     #:key chebyshev-interval-key))

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

(define (chebyshev-alts altns global-batch spec-batch reducer pcontext source-brfs)
  (cond
    [(or (not pcontext) (null? source-brfs)) '()]
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
     (define degree-limit (*chebyshev-order-limit*))
     ;; TODO: hacky to account for 0 coefficients
     (define N (* 2 degree-limit))
     (define source-errors
       (parameterize ([*timeline-disabled* #t])
         (minimum-errors (batch-errors global-batch source-brfs pcontext ctx))))

     (reap [sow]
           (for ([var (in-list vars)])
             (define var-repr (context-lookup ctx var))
             (define var-idx (index-of (context-vars ctx) var))
             (define point-values (sorted-point-values pcontext var-idx var-repr))
             (define intervals (chebyshev-intervals point-values source-errors))

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
                 (define lo (chebyshev-interval-lo interval))
                 (define hi (chebyshev-interval-hi interval))
                 (when (< lo hi)
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
                     (match-define (list _ spec-brf _ repr altn _) q)
                     (define valid?
                       (for/and ([v (in-vector vals)])
                         (and (real? v) (rational? v))))
                     (when valid?
                       (define coeffs0 (compute-chebyshev-coeffs vals N))
                       (define max-coeff
                         (for/fold ([mx 0.0]) ([c (in-vector coeffs0)])
                           (max mx (abs c))))
                       ;; Drop relatively small coefficients.
                       (define coeffs
                         (for/vector #:length (vector-length coeffs0)
                                     ([c (in-vector coeffs0)])
                           (if (and (finite-real? max-coeff)
                                    (> (abs c) (* (*chebyshev-threshold*) max-coeff)))
                               c
                               0.0)))
                       ;; Truncate polynomial to desired degree (ignoring 0 coefficients).
                       (define order 0)
                       (for ([k (in-range (add1 N))]
                             #:break (>= order degree-limit))
                         (when (not (zero? (vector-ref coeffs k)))
                           (define poly-brf (build-clenshaw-poly! global-batch coeffs k var m r))
                           (define gen (approx spec-brf (hole (representation-name repr) poly-brf)))
                           (define brf (batch-add! global-batch gen))
                           (define transform
                             (string->symbol (format "chebyshev-~a"
                                                     (chebyshev-interval-source interval))))
                           (sow (alt brf `(taylor ,transform ,var ,order) (list altn)))
                           (set! order (add1 order)))))))))))]))
