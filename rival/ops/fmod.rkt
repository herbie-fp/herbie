#lang racket/base

(require "core.rkt"
         "../mpfr.rkt")
(provide ival-fmod
         ival-remainder)

(define (bfmul* a b)
  (if (or (bfzero? a) (bfzero? b))
      0.bf
      (bfmul a b)))

;; WARNING: double rounding issues below, it does not refine
;;
;;
;; (define x
;;   (parameterize ([bf-precision 88])
;;     (ival (bf "2.023002359077489336695397166e258"))))
;;
;; (define y
;;   (parameterize ([bf-precision 91])
;;     (ival 0.bf (bf "6.4878140443992047719110337054e233"))))
;;
;; (define yhi
;;   (parameterize ([bf-precision 91])
;;     (ival 0.bf (bf "6.4878140443992047719110335995e233"))))
;;
;;
;; Here yhi refines y, so you'd think fmod(x, yhi) refines fmod(x, y)
;; but in fact it doesn't! Due to double-rounding, in fmox(x, y),
;; we don't think there's an intersection along the top edge, but in
;; fmod(x, yhi) we falsely do, which leads to a bigger interval.
;;
;; For this reason ival-fmod and ival-remainder skip refinement tests.
;;
;; Ideally we'd fix this and get even tighter bounds, but maybe it
;; doesn't matter?

;; Assumes both `x` and `y` are entirely positive
(define (ival-fmod-pos x y err? err)
  (define a (rnd 'down bftruncate (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
    [(bf=? a b) ; No intersection along `y.hi` edge
     (define c (rnd 'down bftruncate (bfdiv (ival-hi-val x) (ival-hi-val y))))
     (define d (rnd 'up bftruncate (bfdiv (ival-hi-val x) (ival-lo-val y))))
     (cond
       [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
        (define lo (rnd 'down bffmod (ival-lo-val x) (ival-hi-val y)))
        (define hi (rnd 'up bffmod (ival-hi-val x) (ival-lo-val y)))
        (ival (endpoint lo #f) (endpoint hi #f) err? err)]
       [else
        (ival (endpoint 0.bf #f)
              (endpoint (rnd 'up bfdiv (ival-hi-val x) (rnd 'down bfadd c 1.bf)) #f)
              err?
              err)])]
    [else (ival (endpoint 0.bf #f) (endpoint (ival-hi-val y) #f) err? err)]))

(define (ival-fmod x y)
  (define err?
    (or (ival-err? x)
        (ival-err? y)
        (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err
    (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-exact-fabs y))
  (cond
    [(= (mpfr-sign (ival-hi-val x)) -1) (ival-neg (ival-fmod-pos (ival-exact-neg x) y* err? err))]
    [(= (mpfr-sign (ival-lo-val x)) 1) (ival-fmod-pos x y* err? err)]
    [else
     (define-values (neg pos) (split-ival x 0.bf))
     (ival-union (ival-fmod-pos pos y* err? err)
                 (ival-neg (ival-fmod-pos (ival-exact-neg neg) y* err? err)))]))

(define (ival-remainder-pos x y err? err)
  ;; Assumes both `x` and `y` are entirely positive
  (define a (rnd 'down bfround (bfdiv (ival-lo-val x) (ival-hi-val y))))
  (define b (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
  (cond
    [(bf=? a b) ; No intersection along `y.hi` edge
     (define c (rnd 'down bfround (bfdiv (ival-hi-val x) (ival-hi-val y))))
     (define d (rnd 'up bfround (bfdiv (ival-hi-val x) (ival-lo-val y))))
     (cond
       [(bf=? c d) ; No intersection along `x.hi` either; use top-left/bottom-right point
        (define lo (rnd 'down bfremainder (ival-lo-val x) (ival-hi-val y)))
        (define hi (rnd 'up bfremainder (ival-hi-val x) (ival-lo-val y)))
        (ival (endpoint lo #f) (endpoint hi #f) err? err)]
       [else
        ;; NOPE! need to subtract half.bf one way, add it another!
        (define y*-hi (rnd 'up bfdiv (bfdiv (ival-hi-val x) (rnd 'down bfadd c half.bf)) 2.bf))
        (define y*-lo
          (rnd 'down
               bfmax2
               (bfsub (ival-lo-val x) (rnd 'up bfmul c (ival-hi-val y)))
               (bfneg (bfdiv (ival-hi-val y) 2.bf))))
        (ival (endpoint (rnd 'down bfmin2 y*-lo (bfneg y*-hi)) #f) (endpoint y*-hi #f) err? err)])]
    [else
     (define y* (rnd 'up bfdiv (ival-hi-val y) 2.bf))
     (ival (endpoint (rnd 'down bfneg y*) #f) (endpoint y* #f) err? err)]))

;; Seems unnecessary
(define (ival-remainder x y)
  (define err?
    (or (ival-err? x)
        (ival-err? y)
        (and (bflte? (ival-lo-val y) 0.bf) (bfgte? (ival-hi-val y) 0.bf))))
  (define err
    (or (ival-err x) (ival-err y) (and (bf=? (ival-lo-val y) 0.bf) (bf=? (ival-hi-val y) 0.bf))))
  (define y* (ival-exact-fabs y))
  (cond
    [(= (mpfr-sign (ival-hi-val x)) -1)
     (ival-neg (ival-remainder-pos (ival-exact-neg x) y* err? err))]
    [(= (mpfr-sign (ival-lo-val x)) 1) (ival-remainder-pos x y* err? err)]
    [else
     (define-values (neg pos) (split-ival x 0.bf))
     (ival-union (ival-remainder-pos pos y* err? err)
                 (ival-neg (ival-remainder-pos (ival-exact-neg neg) y* err? err)))]))
