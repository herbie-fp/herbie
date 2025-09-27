#lang racket/base

(require racket/match)
(require "core.rkt"
         "../mpfr.rkt")
(provide ival-lgamma
         ival-tgamma)

(define (bigfloat-midpoint lo hi)
  (bfstep lo (inexact->exact (floor (/ (bigfloats-between lo hi) 2)))))

(define (convex-find-min fn xlo xhi)
  ; lgamma is always convex in the same direction
  (let loop ([lo xlo]
             [mlo (bfdiv (bfadd (bfadd xlo xhi) xlo) 3.bf)]
             [mhi (bfdiv (bfadd (bfadd xlo xhi) xhi) 3.bf)]
             [hi xhi])
    (let ([ylo (rnd 'up fn lo)]
          [ymlo (rnd 'down fn mlo)]
          [yhi (rnd 'up fn hi)]
          [ymhi (rnd 'down fn mhi)])
      ;; Invariant: ylo >= ymlo and yhi >= ymhi.
      ;; Base case: ylo and yhi = +inf.bf
      ;; Therefore lgamma decreasing from lo to mlo and increasing from mhi to hi
      (cond
        [(<= (bigfloats-between lo hi) 3)
         (define dy1 (rnd 'up bfsub ymlo ylo))
         (define dy2 (rnd 'up bfsub ymhi yhi))
         ; Overcorrect for possible deviation downward
         (define dy (rnd 'up bfdiv (bfmax2 dy1 dy2) 2.bf))
         (values mlo (rnd 'down bfadd ymlo dy))]
        ; Close enough to exit
        [(<= (bigfloats-between mlo mhi) 1) (loop (bfprev mlo) mlo mhi (bfnext mhi))]
        ; If true, lgamma decreasing from mlo to mhi
        [(bfgt? ymlo ymhi) (loop mlo mhi (bigfloat-midpoint mhi hi) hi)]
        [else (loop lo (bigfloat-midpoint lo mlo) mlo mhi)]))))

;; These both assume that xmin and ymin are not immovable (they are computed with rounding)
(define ((convex fn xmin ymin) i)
  (match-define (ival lo hi err? err) i)
  (cond
    ; Purely increasing
    [(bfgt? (endpoint-val lo) xmin) ((monotonic->ival fn) i)]
    ; Purely decreasing
    [(bflt? (endpoint-val hi) xmin) ((comonotonic->ival fn) i)]
    [else
     (ival-union (ival (endpoint ymin #f) (rnd 'up epfn fn lo) err? err)
                 (ival (endpoint ymin #f) (rnd 'up epfn fn hi) err? err))]))

; Optimized version of `ival-lgamma-basin` for positive values, adds a cache
(define lgamma-pos-xmin #f)
(define lgamma-pos-ymin #f)
(define lgamma-pos-prec #f)

(define (ival-lgamma-pos x)
  (match-define (ival (endpoint xlo xlo!) (endpoint xhi xhi!) xerr? xerr) x)
  (cond
    ; Fast path, gamma is increasing here
    [(bfgte? xlo (bf 1.5)) ((monotonic->ival bflog-gamma) x)]
    ; Another fast path
    [(and (bfgte? xlo 0.bf) (bflte? xhi (bf 1.4))) ((comonotonic->ival bflog-gamma) x)]
    [else
     ;; Gamma has a single minimum for positive inputs, which is about 1.46163
     ;; This computation is common enough that we cache it
     (unless (and lgamma-pos-prec (<= (bf-precision) lgamma-pos-prec))
       (define-values (xmin ymin) (convex-find-min bflog-gamma (bf 1.46163) (bf 1.46164)))
       (set! lgamma-pos-xmin xmin)
       (set! lgamma-pos-ymin ymin)
       (set! lgamma-pos-prec (bf-precision)))
     ((convex bflog-gamma lgamma-pos-xmin lgamma-pos-ymin) x)]))

;; Crude estimate, used only when other option is not available.
;;  Note that G(-n + f) = +- G(f) G(1-f) / G(n + 1 - f).
;; Thus, the min of G over [-n, -n + 1] is greater than
;;   A) the min of G(f) G(1-f) over [0, 1]
;;   B) divided by the max of G(n + 1 - f) over [0, 1]
;; (A) is easy because that function is symmetric over [0, 1];
;;   the min is at 1/2 with value G(1/2)^2 = pi
;; (B) is also easy because G is increasing on [n, n + 1]
;;   since n is positive, so the max is at n + 1
;; Hence, G over [-n, -n + 1] is greater than
;;   pi / G(n + 1);
;; Hence, logG over [-n, -n + 1] is greater than
;;   log pi - logG(n + 1)
(define (ival-lgamma-basin-bound imin)
  (define logpi (rnd 'down bflog (rnd 'down pi.bf)))
  (define logg (rnd 'up bflog-gamma (rnd 'up bfadd 1.bf (bfneg imin))))
  (define bound (rnd 'down bfsub logpi logg))
  (ival (endpoint bound #f) (endpoint +inf.bf #f) #f #f))

;; Here we assume that x is entirely negative
;; and does not cross any integer boundaries.
(define (ival-lgamma-basin x)
  (define imin (bffloor (ival-lo-val x)))
  (define imax (bfceiling (ival-hi-val x)))
  (if (< (bigfloats-between imin imax) 25) ; Value 25 is not verified
      (ival-then x (ival-lgamma-basin-bound imin)) ; Too close, cannot use convex
      (let-values ([(xmin ymin) (convex-find-min bflog-gamma imin imax)])
        ((convex bflog-gamma xmin ymin) x))))

(define (ival-lgamma x)
  ; The starred versions allow #f for an empty interval
  (define (ival-lo-val* x)
    (if x
        (ival-lo-val x)
        0.bf))
  (define (ival-split* i x)
    (if i
        (ival-split i x)
        (values #f #f)))
  (define (ival-union* a b)
    (if (and a b)
        (ival-union a b)
        (or a b)))

  (define-values (xneg xpos) (ival-split x 0.bf))
  (define-values (xnegl xrest) (ival-split* xneg (bfceiling (ival-lo-val* xneg))))
  (define-values (xnegr xdrop) (ival-split* xrest (rnd 'up bfadd 1.bf (ival-lo-val* xrest))))

  (define negy
    (and xneg
         (or (ival-union* (and xnegl (ival-lgamma-basin xnegl)) (and xnegr (ival-lgamma-basin xnegr)))
             ;; This case only happens if xnegr = #f meaning lo = rnd[up](lo + 1) meaning lo = -inf
             (mk-big-ival -inf.bf +inf.bf))))

  (ival-union* (and xpos (ival-lgamma-pos xpos)) negy))

(define (exact-bffloor x)
  (parameterize ([bf-precision (bigfloat-precision x)])
    (bffloor x)))

(define (ival-tgamma x)
  (define logy (ival-lgamma x))
  (unless logy
    (error 'ival-lgamma "Invalid input to ival-lgamma: ~a" x))
  (define absy (ival-exp logy))
  (define lo (ival-lo-val x))
  (define hi (ival-hi-val x))
  (cond
    [(bfgte? lo 0.bf) absy]
    [(not (bf=? (exact-bffloor lo) (exact-bffloor hi)))
     (ival (endpoint -inf.bf (ival-lo-fixed? x))
           (endpoint +inf.bf (ival-hi-fixed? x))
           #t
           (ival-err x))]
    [(and (not (bfpositive? lo)) (bf=? lo hi) (bfinteger? lo)) ival-illegal]
    [(bfeven? (exact-bffloor lo)) absy]
    [else (ival-neg absy)]))
