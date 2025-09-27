#lang racket

(require racket/math
         math/base
         math/flonum
         (only-in math/bigfloat
                  bf>
                  bf<
                  bf=
                  bf>=
                  bf<=
                  bf-
                  bf+
                  bf*
                  bf/
                  bfmin
                  bfmax
                  bfshift
                  bigfloat->flonum))
(require rackunit)
(require "main.rkt"
         "mpfr.rkt")
(provide ival-valid?
         ival-refines?
         function-table
         sample-interval
         slow-tests
         sample-from)

(define (ival-valid? ival)
  (if (ival-err ival)
      (ival-err? ival)
      (if (boolean? (ival-lo ival))
          (or (not (ival-lo ival)) (ival-hi ival))
          (and (bf<= (ival-lo ival) (ival-hi ival))
               (not (and (bfinfinite? (ival-hi ival)) (bf= (ival-hi ival) (ival-lo ival))))
               (<= (mpfr-sign (ival-lo ival)) (mpfr-sign (ival-hi ival)))))))

(define (ival-contains? ival pt)
  (if (bigfloat? pt)
      (cond
        [(bfnan? pt) (ival-err? ival)]
        ;; This could be a real value rounded up, or a true infinity
        ;; In theory, we could check the exact flag to determine this,
        ;; but some of the functions we code up don't set that flag.
        [(bfinfinite? pt) true]
        [else
         (and (not (ival-err ival))
              (if (equal? (bigfloat-precision pt) (bigfloat-precision (ival-lo ival)))
                  (bf<= (ival-lo ival) pt)
                  (bf<= (parameterize ([bf-precision (bigfloat-precision pt)]
                                       [bf-rounding-mode 'down])
                          (bfcopy (ival-lo ival)))
                        pt))
              (if (equal? (bigfloat-precision pt) (bigfloat-precision (ival-hi ival)))
                  (bf<= pt (ival-hi ival))
                  (bf<= pt
                        (parameterize ([bf-precision (bigfloat-precision pt)]
                                       [bf-rounding-mode 'up])
                          (bfcopy (ival-hi ival))))))])
      (and (not (ival-err ival)) (or (equal? pt (ival-lo ival)) (equal? pt (ival-hi ival))))))

(define (value-equals? bf1 bf2 [rnd-mode 'nearest])
  (if (boolean? bf1)
      (equal? bf1 bf2)
      (or (bigfloats-equal? bf1 bf2 rnd-mode) (and (bfnan? bf1) (bfnan? bf2)))))

(define (bigfloats-equal? x y rnd-mode)
  (cond
    [(< (bigfloat-precision x) (bigfloat-precision y))
     (bf= x
          (parameterize ([bf-rounding-mode rnd-mode]
                         [bf-precision (bigfloat-precision x)])
            (bfcopy y)))]
    [(> (bigfloat-precision x) (bigfloat-precision y))
     (bf= y
          (parameterize ([bf-rounding-mode rnd-mode]
                         [bf-precision (bigfloat-precision y)])
            (bfcopy x)))]
    [else (bf= y x)]))

(define (value-lte? bf1 bf2)
  (if (boolean? bf1)
      (or (not bf1) bf2)
      (bf<= bf1 bf2)))

(define (ival-refines? coarse fine)
  (and
   (or (ival-err fine)
       (and ((if (ival-lo-fixed? coarse) value-equals? value-lte?) (ival-lo coarse) (ival-lo fine))
            ((if (ival-hi-fixed? coarse) value-equals? value-lte?) (ival-hi fine) (ival-hi coarse))))
   (if (ival-lo-fixed? coarse)
       (ival-lo-fixed? fine)
       #t)
   (if (ival-hi-fixed? coarse)
       (ival-hi-fixed? fine)
       #t)
   (if (ival-err? fine)
       (ival-err? coarse)
       #t)
   (if (ival-err coarse)
       (ival-err fine)
       #t)))

(define (bfatan2-no0 y x)
  (if (and (bfzero? y) (bfzero? x))
      +nan.bf
      (bfatan2 y x)))

(define ((bftrig-narrow fn) x)
  (if (> (+ (bigfloat-exponent x) (bigfloat-precision x)) (expt 2 20))
      0.bf
      (fn x)))

(define function-table
  (list (list ival-neg bf- '(real) 'real)
        (list ival-fabs bfabs '(real) 'real)
        (list ival-add bf+ '(real real) 'real)
        (list ival-sub bf- '(real real) 'real)
        (list ival-mult bf* '(real real) 'real)
        (list ival-div bf/ '(real real) 'real)
        (list ival-fma bffma '(real real real) 'real)
        (list ival-sqrt bfsqrt '(real) 'real)
        (list ival-hypot bfhypot '(real real) 'real)
        (list ival-rint bfrint '(real) 'real)
        (list ival-round bfround '(real) 'real)
        (list ival-ceil bfceiling '(real) 'real)
        (list ival-floor bffloor '(real) 'real)
        (list ival-trunc bftruncate '(real) 'real)
        (list ival-cbrt bfcbrt '(real) 'real)
        (list ival-exp bfexp '(real) 'real)
        (list ival-exp2 bfexp2 '(real) 'real)
        (list ival-expm1 bfexpm1 '(real) 'real)
        (list ival-log bflog '(real) 'real)
        (list ival-log2 bflog2 '(real) 'real)
        (list ival-log10 bflog10 '(real) 'real)
        (list ival-log1p bflog1p '(real) 'real)
        (list ival-logb bflogb '(real) 'real)
        (list ival-pow bfexpt '(real real) 'real)
        (list ival-sin (bftrig-narrow bfsin) '(real) 'real)
        (list ival-cos (bftrig-narrow bfcos) '(real) 'real)
        (list ival-tan (bftrig-narrow bftan) '(real) 'real)
        (list ival-asin bfasin '(real) 'real)
        (list ival-acos bfacos '(real) 'real)
        (list ival-atan bfatan '(real) 'real)
        (list ival-atan2 bfatan2-no0 '(real real) 'real)
        (list ival-sinh bfsinh '(real) 'real)
        (list ival-cosh bfcosh '(real) 'real)
        (list ival-tanh bftanh '(real) 'real)
        (list ival-asinh bfasinh '(real) 'real)
        (list ival-acosh bfacosh '(real) 'real)
        (list ival-atanh bfatanh '(real) 'real)
        (list ival-fmod bffmod '(real real) 'real)
        (list ival-remainder bfremainder '(real real) 'real)
        (list ival-fmin bfmin '(real real) 'real)
        (list ival-fmax bfmax '(real real) 'real)
        (list ival-copysign bfcopysign '(real real) 'real)
        (list ival-fdim bffdim '(real real) 'real)
        (list ival-< bf< '(real real) 'bool)
        (list ival-<= bf<= '(real real) 'bool)
        (list ival-> bf> '(real real) 'bool)
        (list ival->= bf>= '(real real) 'bool)
        (list ival-== bf= '(real real) 'bool)
        (list ival-!= (negate bf=) '(real real) 'bool)
        (list ival-and and-fn '(bool bool bool) 'bool)
        (list ival-or or-fn '(bool bool bool) 'bool)
        (list ival-not not '(bool) 'bool)
        (list ival-if if-fn '(bool real real) 'real)
        (list ival-erf bferf '(real) 'real)
        (list ival-erfc bferfc '(real) 'real)
        (list ival-lgamma bflog-gamma '(real) 'real)
        (list ival-tgamma bfgamma '(real) 'real)))

(define (sample-precision)
  (random 40 100))

(define (sample-bigfloat)
  (cond
    [(= (random 0 100) 0) ; 4% chance of special value
     (match (random 0 10)
       [0 (bf -inf.0)]
       [1 (bf -1)]
       [2 (bf -0.0)]
       [3 (bf 0.0)]
       [4 (bf 1)]
       [5 (bf +inf.0)]
       [6
        (parameterize ([bf-rounding-mode 'down])
          (pi.bf))]
       [7
        (parameterize ([bf-rounding-mode 'up])
          (pi.bf))]
       [8
        (parameterize ([bf-rounding-mode 'down])
          (bf- (pi.bf)))]
       [9
        (parameterize ([bf-rounding-mode 'up])
          (bf- (pi.bf)))])]
    [else
     (define exponent (random -1023 1023)) ; Pretend-double
     (define significand (bf (random-bits (bf-precision)) (- (bf-precision))))
     (define val (bfshift (bf+ 1.bf significand) exponent))
     (if (= (random 0 2) 1)
         (bf- val)
         val)]))

(define (sample-wide-interval v1)
  (define v2 (sample-bigfloat))
  (ival (bfmin v1 v2) (bfmax v1 v2)))

(define (sample-constant-interval c)
  (ival c c))

(define (sample-narrow-interval v1)
  ;; Biased toward small intervals
  (define size (random 1 (bf-precision)))
  (define delta
    (* (match (random 0 2)
         [0 -1]
         [1 1])
       size))
  (define v2 (bfstep v1 delta))
  (ival (bfmin v1 v2) (bfmax v1 v2)))

(define (sample-interval type)
  (match type
    ['real
     (define mode (random 0 3))
     (define value (sample-bigfloat))
     (define x
       (match mode
         [0
          #:when (not (bfinfinite? value))
          (sample-constant-interval value)]
         [1 (sample-narrow-interval value)]
         [_ (sample-wide-interval value)]))
     (if (ival-err x)
         (sample-interval type)
         x)]
    ['bool
     (match (random 0 3)
       [0 (ival #f)]
       [1 (ival #t)]
       [2 (ival #f #t)])]))

(define (sample-from ival)
  (if (bigfloat? (ival-lo ival))
      (cond
        [(or (bf<= (ival-lo ival) 0.bf (ival-hi ival))
             (and (bfinfinite? (ival-lo ival)) (not (infinite? (bigfloat->flonum (ival-hi ival)))))
             (and (bfinfinite? (ival-hi ival)) (not (infinite? (bigfloat->flonum (ival-lo ival))))))
         (define lo* (bigfloat->flonum (ival-lo ival)))
         (define hi* (bigfloat->flonum (ival-hi ival)))
         (define range (flonums-between lo* hi*))
         (bf (flstep lo* (random-natural (+ range 1))))]
        [else
         (define reduction (if (or (bfinfinite? (ival-lo ival)) (bfinfinite? (ival-hi ival))) 1 0))
         (define range (- (bigfloats-between (ival-lo ival) (ival-hi ival)) reduction))
         (define offset (+ (if (bfinfinite? (ival-lo ival)) 1 0) (random-natural (+ range 1))))
         (bfstep (ival-lo ival) offset)])
      (let ([p (random 0 2)])
        (if (= p 0)
            (ival-lo ival)
            (ival-hi ival)))))

(define-simple-check (check-ival-valid? ival) (ival-valid? ival))

(define-binary-check (check-ival-equals? ival1 ival2)
                     (if (ival-err ival1)
                         (ival-err ival2)
                         (and (value-equals? (ival-lo ival1) (ival-lo ival2) 'down)
                              (value-equals? (ival-hi ival1) (ival-hi ival2) 'up))))

(define num-tests 1000)
(define num-witnesses 10)

;; These fail refinement due to double-rounding sending us down the wrong branch
(define non-refine-tests (list ival-fmod ival-remainder))
;; These are super slow due to trying to find the minimum
(define slow-tests (list ival-lgamma ival-tgamma))
(define num-slow-tests 25)

(define (test-entry ival-fn fn args)
  (define out-prec (sample-precision))
  (define in-precs
    (for/list ([arg args])
      (sample-precision)))

  (define is
    (for/list ([arg args]
               [in-prec in-precs])
      (parameterize ([bf-precision in-prec])
        (sample-interval arg))))
  (define iy
    (parameterize ([bf-precision out-prec])
      (apply ival-fn is)))

  (with-check-info (['intervals is]) (check-ival-valid? iy))

  (define xs #f)
  (define y #f)
  (for ([_ (in-range num-witnesses)])
    (set! xs
          (for/list ([i is]
                     [in-prec in-precs])
            (parameterize ([bf-precision in-prec])
              (sample-from i))))
    (set! y
          (parameterize ([bf-precision out-prec])
            (apply fn xs)))
    (with-check-info (['intervals is] ['points xs] ['precs (list out-prec in-precs)])
                     (check ival-contains? iy y)))

  (unless (set-member? non-refine-tests ival-fn)
    (with-check-info
     (['intervals is] ['points xs] ['iy iy] ['y y] ['precs (list out-prec in-precs)])
     (for ([k (in-naturals)]
           [i is]
           [x xs])
       (define-values (ilo ihi) (ival-split i x))
       (when (and ilo ihi)
         (define iylo
           (parameterize ([bf-precision out-prec])
             (apply ival-fn (list-set is k ilo))))
         (define iyhi
           (parameterize ([bf-precision out-prec])
             (apply ival-fn (list-set is k ihi))))
         (with-check-info (['split-argument k] ['ilo ilo] ['ihi ihi] ['iylo iylo] ['iyhi iyhi])
                          (check-ival-equals? iy
                                              (parameterize ([bf-precision out-prec])
                                                (ival-union iylo iyhi))))))
     (when (or (ival-lo-fixed? iy) (ival-hi-fixed? iy))
       (define iy*
         (parameterize ([bf-precision 128])
           (apply ival-fn is)))
       (check ival-refines? iy iy*)))))

(define (run-tests)
  (check ival-contains? (ival-bool #f) #f)
  (check ival-contains? (ival-bool #t) #t)
  (check ival-contains? (ival-pi) (pi.bf))
  (check ival-contains? (ival-e) (bfexp 1.bf))
  (test-case "mk-ival"
    (for ([i (in-range num-tests)])
      (define pt (sample-bigfloat))
      (with-check-info (['point pt])
                       (check-ival-valid? (mk-ival pt))
                       (check ival-contains? (mk-ival pt) pt))))

  (test-case "ival-error?"
    (let* ([ival1 (mk-ival 1.bf)]
           [ival0 (mk-ival 0.bf)]
           [ivalboth (ival 0.bf 1.bf)]
           [res1 (ival-div ival1 ival1)]
           [res2 (ival-div ival1 ival0)]
           [res3 (ival-div ival1 ivalboth)])
      (check-ival-valid? (ival-error? res1))
      (check-ival-valid? (ival-error? res2))
      (check-ival-valid? (ival-error? res3))
      (check ival-contains? (ival-error? res1) #f)
      (check ival-contains? (ival-error? res2) #t)
      (check ival-contains? (ival-error? res3) #f)
      (check ival-contains? (ival-error? res3) #t)))

  (define (sorted? list cmp)
    (cond
      [(<= (length list) 1) #t]
      [else (and (cmp (first list) (second list)) (sorted? (rest list) cmp))]))

  (test-case "ival-sort"
    (for ([n (in-range num-tests)])
      (let* ([input (for/list ([n (in-range (random 0 30))])
                      (sample-interval 'real))]
             [output (ival-sort input bf<)])
        (check-true (sorted? (map ival-lo output) bf<=))
        (check-true (sorted? (map ival-hi output) bf<=)))))

  (test-case "ival-if"
    ;; Tests that movable conditions for if statements mean movable results
    (ival-lo-fixed? (ival-if (ival #f #t) (ival 0.bf) (ival 1.bf))))

  (for ([entry (in-list function-table)])
    (match-define (list ival-fn fn args _) entry)
    (define N (if (memq ival-fn slow-tests) num-slow-tests num-tests))
    (test-case (~a (object-name ival-fn))
      (for ([n (in-range N)])
        (test-entry ival-fn fn args)))))

(module+ test
  (run-tests))
(module+ main
  (require racket/cmdline)
  (command-line #:args ([fname #f] [n (~a num-tests)])
                (cond
                  [fname
                   (define entry
                     (findf (λ (entry)
                              (equal? (~a (object-name (first entry))) (format "ival-~a" fname)))
                            function-table))
                   (match entry
                     [#f (raise-user-error 'test.rkt "No function named ival-~a" fname)]
                     [(list ival-fn fn itypes otype)
                      (printf "~a on ~a inputs: " (object-name ival-fn) n)
                      (define num-tests (string->number n))
                      (for ([outer (in-range (quotient num-tests 1000))])
                        (for ([inner (in-range (min 1000 (- num-tests (* outer 1000))))])
                          (test-entry ival-fn fn itypes)
                          (when (set-member? slow-tests ival-fn)
                            (printf ".")
                            (flush-output)))
                        (unless (set-member? slow-tests ival-fn)
                          (printf "*")
                          (flush-output)))
                      (newline)])]
                  [else (run-tests)])))
