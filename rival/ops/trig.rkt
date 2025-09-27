#lang racket

(require "arith.rkt"
         "core.rkt"
         "../mpfr.rkt")
(provide ival-sin
         ival-cos
         ival-tan
         ival-cosu
         ival-sinu
         ival-tanu)

(define *rival-precision* (make-parameter (expt 2 20)))

(define (classify-ival-periodic x period)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (define period/4 (integer-length (exact-floor (/ period 4))))
  (define lo-exp (mpfr-exp xlo))
  (define hi-exp (mpfr-exp xhi))
  (define lo-ulp (- lo-exp (bigfloat-precision xlo)))
  (define hi-ulp (- hi-exp (bigfloat-precision xhi)))
  (cond
    [(or (bfinfinite? xlo) (bfinfinite? xhi)) 'too-wide]
    [(and (< lo-exp period/4) (< hi-exp period/4)) 'near-0]
    [(or (> lo-ulp 0) (> hi-ulp 0)) (if (bf=? xlo xhi) 'range-reduce 'too-wide)]
    [else 'range-reduce]))

(define (range-reduce-precision xlo xhi)
  (min (*rival-precision*)
       (max (bf-precision)
            (max (+ (bigfloat-exponent xlo) (bigfloat-precision xlo) (bigfloat-precision xlo))
                 (+ (bigfloat-exponent xhi) (bigfloat-precision xhi) (bigfloat-precision xhi))))))

(define (ival-cosu n)
  (define (ival-cosu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
    (match (classify-ival-periodic x n)
      ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
      ['near-0
       (match (classify-ival x)
         [-1 ((monotonic->ival (curry bfcosu n)) x)]
         [1 ((comonotonic->ival (curry bfcosu n)) x)]
         [else
          (ival (rnd 'down
                     epfn
                     bfmin2
                     (epfn (curry bfcosu n) (ival-lo x))
                     (epfn (curry bfcosu n) (ival-hi x)))
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))])]
      ['range-reduce
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
           (ival-floor (ival-div x (mk-ival (bf (/ n 2)))))))
       (cond
         [(and (bf=? a b) (bfeven? a)) ((comonotonic->ival (curry bfcosu n)) x)]
         [(and (bf=? a b) (bfodd? a)) ((monotonic->ival (curry bfcosu n)) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (ival (endpoint -1.bf #f)
                (rnd 'up
                     epfn
                     bfmax2
                     (epfn (curry bfcosu n) (ival-lo x))
                     (epfn (curry bfcosu n) (ival-hi x)))
                (ival-err? x)
                (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (ival (rnd 'down
                     epfn
                     bfmin2
                     (epfn (curry bfcosu n) (ival-lo x))
                     (epfn (curry bfcosu n) (ival-hi x)))
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))]
         [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))
  (if bfcosu ival-cosu #f))

(define (ival-cos x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (match (classify-ival-periodic x (* 2 pi))
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0
     (match (classify-ival x)
       [-1 ((monotonic->ival bfcos) x)]
       [1 ((comonotonic->ival bfcos) x)]
       [else
        (ival (rnd 'down epfn bfmin2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))])]
    ['range-reduce
     (match-define (ival (endpoint a _) (endpoint b _) _ _)
       (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
         (ival-floor (ival-div x (ival-pi)))))
     (cond
       [(and (bf=? a b) (bfeven? a)) ((comonotonic->ival bfcos) x)]
       [(and (bf=? a b) (bfodd? a)) ((monotonic->ival bfcos) x)]
       [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
        (ival (endpoint -1.bf #f)
              (rnd 'up epfn bfmax2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
              (ival-err? x)
              (ival-err x))]
       [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
        (ival (rnd 'down epfn bfmin2 (epfn bfcos (ival-lo x)) (epfn bfcos (ival-hi x)))
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))]
       [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))

(define (ival-sinu n)
  (define (ival-sinu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

    (match (classify-ival-periodic x n)
      ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
      ['near-0 ((monotonic->ival (curry bfsinu n)) x)]
      ['range-reduce
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
           (ival-round (ival-div x (mk-ival (bf (/ n 2)))))))
       (cond
         [(and (bf=? a b) (bfodd? a)) ((comonotonic->ival (curry bfsinu n)) x)]
         [(and (bf=? a b) (bfeven? a)) ((monotonic->ival (curry bfsinu n)) x)]
         [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
          (ival (endpoint -1.bf #f)
                (rnd 'up
                     epfn
                     bfmax2
                     (epfn (curry bfsinu n) (ival-lo x))
                     (epfn (curry bfsinu n) (ival-hi x)))
                (ival-err? x)
                (ival-err x))]
         [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
          (ival (rnd 'down
                     epfn
                     bfmin2
                     (epfn (curry bfsinu n) (ival-lo x))
                     (epfn (curry bfsinu n) (ival-hi x)))
                (endpoint 1.bf #f)
                (ival-err? x)
                (ival-err x))]
         [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))
  (if bfsinu ival-sinu #f))

(define (ival-sin x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

  (match (classify-ival-periodic x (* 2 pi))
    ['too-wide (ival-then x (mk-big-ival -1.bf 1.bf))]
    ['near-0 ((monotonic->ival bfsin) x)]
    ['range-reduce
     (match-define (ival (endpoint a _) (endpoint b _) _ _)
       (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
         (ival-round (ival-div x (ival-pi)))))
     (cond
       [(and (bf=? a b) (bfodd? a)) ((comonotonic->ival bfsin) x)]
       [(and (bf=? a b) (bfeven? a)) ((monotonic->ival bfsin) x)]
       [(and (bf=? (bfsub b a) 1.bf) (bfodd? a))
        (ival (endpoint -1.bf #f)
              (rnd 'up epfn bfmax2 (epfn bfsin (ival-lo x)) (epfn bfsin (ival-hi x)))
              (ival-err? x)
              (ival-err x))]
       [(and (bf=? (bfsub b a) 1.bf) (bfeven? a))
        (ival (rnd 'down epfn bfmin2 (epfn bfsin (ival-lo x)) (epfn bfsin (ival-hi x)))
              (endpoint 1.bf #f)
              (ival-err? x)
              (ival-err x))]
       [else (ival-then x (mk-big-ival -1.bf 1.bf))])]))

(define (ival-tanu n)
  (define (ival-tanu x)
    (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

    (match (classify-ival-periodic x (/ n 2))
      ['too-wide (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr)]
      ['near-0 ((monotonic->ival (curry bftanu n)) x)]
      ['range-reduce
       (match-define (ival (endpoint a _) (endpoint b _) _ _)
         (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
           (ival-round (ival-div x (mk-ival (bf (/ n 2)))))))

       (if (bf=? a b) ; Same period
           ((monotonic->ival (curry bftanu n)) x)
           (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr))]))
  (if bftanu ival-tanu #f))

(define (ival-tan x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)

  (match (classify-ival-periodic x pi)
    ['too-wide (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr)]
    ['near-0 ((monotonic->ival bftan) x)]
    ['range-reduce
     (match-define (ival (endpoint a _) (endpoint b _) _ _)
       (parameterize ([bf-precision (range-reduce-precision xlo xhi)])
         (ival-round (ival-div x (ival-pi)))))

     (if (bf=? a b) ; Same period
         ((monotonic->ival bftan) x)
         (ival (endpoint -inf.bf (and xlo! xhi!)) (endpoint +inf.bf (and xlo! xhi!)) #t xerr))]))
