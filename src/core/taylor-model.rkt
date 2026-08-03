#lang racket

;; Taylor models to certify a one-term Taylor cover near zero:
;;
;;   f(x) = c_o x^o + ... + c_{o+n-1} x^(o+n-1) + d x^(o+n),  some d in delta
;;
;; for every nonzero x in X, each c an interval.

(require math/bigfloat
         math/number-theory)
(require "../syntax/batch.rkt"
         "../syntax/rival.rkt")

(provide taylor-model
         tmodel-offset
         taylor-model-fits?)

(define order (make-parameter 8))
(define max-order 32)
(define precision 256)

(struct tmodel (offset coeffs delta) #:transparent)

(define zero-ival (ival (bf 0) (bf 0)))
(define one-ival (ival (bf 1) (bf 1)))
(define nan-ival (ival +nan.bf +nan.bf))

(define (exact->ival x)
  (ival (parameterize ([bf-rounding-mode 'down])
          (bf x))
        (parameterize ([bf-rounding-mode 'up])
          (bf x))))

(define (ival-zero? iv)
  (and (bfzero? (ival-lo iv)) (bfzero? (ival-hi iv))))

(define (ival-mag iv)
  (bfmax (bfabs (ival-lo iv)) (bfabs (ival-hi iv))))

(define (ival-expt iv n)
  (for/fold ([acc one-ival]) ([_ (in-range n)])
    (ival-mul acc iv)))

(define powers (make-parameter #f))

(define (x^ d)
  (hash-ref! (powers) d (λ () (ival-mul (x^ 1) (x^ (sub1 d))))))

(define invalid-model (tmodel 0 (list nan-ival) nan-ival))

(define (tm-end tm)
  (+ (tmodel-offset tm) (length (tmodel-coeffs tm))))

(define (tm-order tm)
  (let loop ([n (tmodel-offset tm)]
             [coeffs (tmodel-coeffs tm)])
    (if (or (null? (cdr coeffs)) (not (ival-zero? (car coeffs))))
        n
        (loop (add1 n) (cdr coeffs)))))

(define (tm-ref tm n)
  (define i (- n (tmodel-offset tm)))
  (if (< -1 i (length (tmodel-coeffs tm)))
      (list-ref (tmodel-coeffs tm) i)
      zero-ival))

;; Everything at exponent `end` and up, over x^end; drops lower coefficients, so
;; callers must only skip exponents known to be zero.
(define (tm-tail tm end)
  (for/fold ([acc (ival-mul (tmodel-delta tm) (x^ (- (tm-end tm) end)))])
            ([c (in-list (tmodel-coeffs tm))]
             [n (in-range (tmodel-offset tm) (tm-end tm))]
             #:when (>= n end))
    (ival-add acc (ival-mul c (x^ (- n end))))))

(define (tm-poly-range tm)
  (for/fold ([acc zero-ival])
            ([c (in-list (tmodel-coeffs tm))]
             [i (in-naturals)])
    (ival-add acc (ival-mul c (x^ i)))))

(define (tm-range tm)
  (tm-tail tm 0))

(define (tm-shift tm d)
  (struct-copy tmodel tm [offset (+ (tmodel-offset tm) d)]))

(define (tm-normalize tm)
  (define dropped (- (tm-order tm) (tmodel-offset tm)))
  (struct-copy tmodel tm [offset (tm-order tm)] [coeffs (list-tail (tmodel-coeffs tm) dropped)]))

(define (tm-truncate tm end)
  (struct-copy tmodel
               tm
               [coeffs
                (for/list ([n (in-range (tmodel-offset tm) end)])
                  (tm-ref tm n))]
               [delta (tm-tail tm end)]))

(define (tm-monomial n c)
  (tmodel n (cons c (make-list (max 0 (- (order) n 1)) zero-ival)) zero-ival))

(define (tm-neg tm)
  (tmodel (tmodel-offset tm) (map ival-neg (tmodel-coeffs tm)) (ival-neg (tmodel-delta tm))))

(define (tm-add a b)
  (define offset (min (tmodel-offset a) (tmodel-offset b)))
  (define end (min (tm-end a) (tm-end b)))
  (tmodel offset
          (for/list ([n (in-range offset end)])
            (ival-add (tm-ref a n) (tm-ref b n)))
          (ival-add (tm-tail a end) (tm-tail b end))))

;; Writing a = P_a + D_a x^Ea and b = P_b + D_b x^Eb, the product is
;; P_a P_b + D_a x^Ea b + D_b x^Eb P_a.
(define (tm-mul a b)
  (define as (tmodel-coeffs a))
  (define bs (list->vector (tmodel-coeffs b)))
  (define len (min (length as) (vector-length bs)))
  (define offset (+ (tmodel-offset a) (tmodel-offset b)))
  (define products
    (for/list ([n (in-range (+ (length as) (vector-length bs) -1))])
      (for/fold ([acc zero-ival])
                ([c (in-list as)]
                 [i (in-naturals)]
                 #:when (< -1 (- n i) (vector-length bs)))
        (ival-add acc (ival-mul c (vector-ref bs (- n i)))))))
  (define crossed
    (ival-add
     (ival-mul (ival-mul (tmodel-delta a) (tm-tail b (tmodel-offset b))) (x^ (- (length as) len)))
     (ival-mul (ival-mul (tmodel-delta b) (tm-poly-range a)) (x^ (- (vector-length bs) len)))))
  (tmodel offset
          (take products len)
          (for/fold ([acc crossed])
                    ([c (in-list (drop products len))]
                     [d (in-naturals)])
            (ival-add acc (ival-mul c (x^ d))))))

;; phi(e(x)), where (term i y) encloses phi^(i)(y)/i!. With y0 the constant term
;; of e and w = e - y0, Lagrange gives
;;
;;   phi(e(x)) = sum_(i < q) term(i, y0) w(x)^i + term(q, xi) w(x)^q
;;
;; for some xi in the range of e.
(define (tm-compose e term)
  (cond
    [(negative? (tmodel-offset e)) invalid-model]
    [else
     (define y0 (tm-ref e 0))
     (define w
       (if (zero? (tmodel-offset e))
           (tmodel 0 (cons zero-ival (rest (tmodel-coeffs e))) (tmodel-delta e))
           e))
     (define sigma (max 1 (tm-order w)))
     (define q (ceiling (/ (tm-end e) sigma)))
     (define sum
       (for/fold ([acc (tm-monomial 0 (term (sub1 q) y0))]) ([i (in-range (- q 2) -1 -1)])
         (tm-add (tm-monomial 0 (term i y0)) (tm-mul w acc))))
     (define end (min (tm-end sum) (* q sigma)))
     (define remainder
       (ival-mul (ival-mul (term q (tm-range e)) (ival-expt (tm-tail w sigma) q))
                 (x^ (- (* q sigma) end))))
     (define truncated (tm-truncate sum end))
     (struct-copy tmodel truncated [delta (ival-add (tmodel-delta truncated) remainder)])]))

(define (exp-term i y)
  (ival-div (ival-exp y) (exact->ival (factorial i))))

(define (sin-term i y)
  (ival-div (match (modulo i 4)
              [0 (ival-sin y)]
              [1 (ival-cos y)]
              [2 (ival-neg (ival-sin y))]
              [3 (ival-neg (ival-cos y))])
            (exact->ival (factorial i))))

(define (cos-term i y)
  (ival-div (match (modulo i 4)
              [0 (ival-cos y)]
              [1 (ival-neg (ival-sin y))]
              [2 (ival-neg (ival-cos y))]
              [3 (ival-sin y)])
            (exact->ival (factorial i))))

(define (log-term i y)
  (if (zero? i)
      (ival-log y)
      (ival-div (exact->ival (/ (expt -1 (sub1 i)) i)) (ival-expt y i))))

(define (recip-term i y)
  (ival-div (exact->ival (expt -1 i)) (ival-expt y (add1 i))))

(define ((root-term n) i y)
  (define binomial (/ (for/product ([m (in-range i)]) (- (/ 1 n) m)) (factorial i)))
  (ival-mul (exact->ival binomial)
            (ival-div (if (= n 2)
                          (ival-sqrt y)
                          (ival-cbrt y))
                      (ival-expt y i))))

(define (tm-recip b)
  (define k (tm-order b))
  (tm-shift (tm-compose (tm-shift (tm-normalize b) (- k)) recip-term) (- k)))

(define (tm-root a n)
  (define k (tm-order a))
  (if (and (zero? (modulo k n))
           (or (odd? n) (even? (quotient k n)) (not (bfnegative? (ival-lo (x^ 1))))))
      (tm-shift (tm-compose (tm-shift (tm-normalize a) (- k)) (root-term n)) (quotient k n))
      invalid-model))

(define (tm-log a)
  (define regular (tm-normalize a))
  (if (zero? (tmodel-offset regular))
      (tm-compose regular log-term)
      invalid-model))

;; Whether `coeff` x^k is within `epsilon` relative error of the model for |x| <= radius.
(define (taylor-model-fits? model coeff epsilon radius)
  (parameterize ([bf-precision precision])
    (define deviations
      (cons (ival-mag (ival-sub (first (tmodel-coeffs model)) (ival (bf coeff) (bf coeff))))
            (append (map ival-mag (rest (tmodel-coeffs model)))
                    (list (ival-mag (tmodel-delta model))))))
    (define deviation
      (parameterize ([bf-rounding-mode 'up])
        (for/fold ([acc (bf 0)]) ([m (in-list (reverse deviations))])
          (bf+ m (bf* acc (bf radius))))))
    (define budget
      (parameterize ([bf-rounding-mode 'down])
        (bf (/ (* epsilon (abs (inexact->exact coeff))) (+ 1 epsilon)))))
    (bf<= deviation budget)))

;; The expression must already be Taylor-expanded to the primitives handled here.
;; TODO: fabs?
(define (taylor-model-at batch brf var domain)
  (parameterize ([powers (make-hash (list (cons 0 one-ival) (cons 1 domain)))])
    (define model
      (batch-recurse batch
                     (λ (brf recurse)
                       (match (deref brf)
                         [(== var) (tm-monomial 1 one-ival)]
                         [(? number? n) (tm-monomial 0 (exact->ival n))]
                         [(list 'PI) (tm-monomial 0 (ival-pi))]
                         [(list 'E) (tm-monomial 0 (ival-e))]
                         [`(+ ,a ,b) (tm-add (recurse a) (recurse b))]
                         [`(neg ,a) (tm-neg (recurse a))]
                         [`(* ,a ,b) (tm-mul (recurse a) (recurse b))]
                         [`(/ ,a ,b) (tm-mul (recurse a) (tm-recip (recurse b)))]
                         [`(sqrt ,a) (tm-root (recurse a) 2)]
                         [`(cbrt ,a) (tm-root (recurse a) 3)]
                         [`(exp ,a) (tm-compose (recurse a) exp-term)]
                         [`(sin ,a) (tm-compose (recurse a) sin-term)]
                         [`(cos ,a) (tm-compose (recurse a) cos-term)]
                         [`(log ,a) (tm-log (recurse a))]
                         [_ invalid-model]))))
    (tm-normalize (model brf))))

;; Cancellation can zero every coefficient; doubling the window fixes it
(define (taylor-model batch brf var domain)
  (parameterize ([bf-precision precision])
    (let loop ([n (order)])
      (define model
        (parameterize ([order n])
          (taylor-model-at batch brf var domain)))
      (if (and (ival-zero? (first (tmodel-coeffs model))) (< n max-order))
          (loop (* 2 n))
          model))))
