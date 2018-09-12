#lang racket

(require math/bigfloat)
(require "common.rkt")

(struct ival (lo hi err? err) #:transparent)

(provide (contract-out
          [struct ival ([lo bigfloat?] [hi bigfloat?] [err? boolean?] [err boolean?])]
          [mk-ival (-> real? ival?)]
          [ival-pi (-> ival?)]
          [ival-e  (-> ival?)]
          [ival-bool (-> boolean? ival?)]
          [ival-add (-> ival? ival? ival?)]
          [ival-sub (-> ival? ival? ival?)]
          [ival-neg (-> ival? ival?)]
          [ival-mult (-> ival? ival? ival?)]
          [ival-div (-> ival? ival? ival?)]
          [ival-sqrt (-> ival? ival?)]
          [ival-exp (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
          #;[ival-pow (-> ival? ival? ival?)]))

(define (mk-ival x)
  (define err? (or (nan? x) (infinite? x)))
  (define x* (bf x)) ;; TODO: Assuming that float precision < bigfloat precision
  (ival x* x* err? err?))

(define (ival-pi)
  (ival (rnd 'down (λ () pi.bf)) (rnd 'up (λ () pi.bf)) #f #f))

(define (ival-e)
  (ival (rnd 'down bfexp 1.bf) (rnd 'up bfexp 1.bf) #f #f))

(define (ival-bool b)
  (ival b b #f #f))

(define-syntax-rule (rnd mode op args ...)
  (parameterize ([bf-rounding-mode mode])
    (op args ...)))

(define (ival-neg x)
  ;; No rounding, negation is exact
  (ival (bf- (ival-hi x)) (bf- (ival-lo x)) (ival-err? x) (ival-err x)))

(define (ival-add x y)
  (ival (rnd 'down bf+ (ival-lo x) (ival-lo y))
        (rnd 'up bf+ (ival-hi x) (ival-hi y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-sub x y)
  (ival (rnd 'down bf- (ival-lo x) (ival-hi y))
        (rnd 'up bf- (ival-hi x) (ival-lo y))
        (or (ival-err? x) (ival-err? y))
        (or (ival-err x) (ival-err y))))

(define (ival-mult x y)
  (define err? (or (ival-err? x) (ival-err? y)))
  (define err (or (ival-err x) (ival-err y)))
  (cond
   [(bf>= (ival-lo x) 0.bf)
    (ival (rnd 'down bf* (ival-lo x) (ival-lo y))
          (rnd 'up bf* (ival-hi x) (ival-hi y))
          err? err)]
   [(bf<= (ival-hi x) 0.bf)
    (ival (rnd 'down bf* (ival-lo x) (ival-hi y))
          (rnd 'up bf* (ival-hi x) (ival-lo y))
          err? err)]
   [(or (bf>= (ival-lo y) 0.bf) (bf<= (ival-hi y) 0.bf)) ; x stradles 0
    (ival-mult y x)]
   [else ; y also stradles 0
    (ival (bfmin (rnd 'down bf* (ival-lo x) (ival-hi y))
                 (rnd 'down bf* (ival-hi x) (ival-lo y)))
          (bfmin (rnd 'up bf* (ival-lo x) (ival-lo y))
                 (rnd 'up bf* (ival-hi x) (ival-hi y)))
          err? err)]))

(define (ival-div x y)
  (define err (or (ival-err x) (ival-err y)
                  (and (bf= (ival-lo y) 0.bf) (bf= (ival-hi y) 0.bf))))
  (define err? (or err (ival-err? x) (ival-err? y)
                   (and (bf<= (ival-lo y) 0.bf) (bf>= (ival-hi y) 0.bf))))
  (cond
   [(bf>= (ival-lo x) 0.bf)
    (ival (rnd 'down bf* (ival-lo x) (ival-hi y))
          (rnd 'up bf* (ival-hi x) (ival-lo y))
          err? err)]
   [(bf<= (ival-hi x) 0.bf)
    (ival (rnd 'down bf* (ival-lo x) (ival-lo y))
          (rnd 'up bf* (ival-hi x) (ival-hi y))
          err? err)]
   [(or (bf>= (ival-lo y) 0.bf) (bf<= (ival-hi y) 0.bf)) ; x stradles 0
    (ival-mult y x)]
   [else ; y also stradles 0
    (ival (bfmin (rnd 'down bf* (ival-lo x) (ival-lo y))
                 (rnd 'down bf* (ival-hi x) (ival-hi y)))
          (bfmin (rnd 'up bf* (ival-lo x) (ival-hi y))
                 (rnd 'up bf* (ival-hi x) (ival-lo y)))
          err? err)]))

(define (ival-exp x)
  (ival (rnd 'down bfexp (ival-lo x)) (rnd 'up bfexp (ival-hi x)) (ival-err? x) (ival-err x)))

(define (ival-log x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bflog (ival-lo x)) (rnd 'up bflog (ival-hi x))
        err? err))

(define (ival-sqrt x)
  (define err (or (ival-err x) (bf<= (ival-hi x) 0.bf)))
  (define err? (or err (ival-err? x) (bf<= (ival-lo x) 0.bf)))
  (ival (rnd 'down bfsqrt (ival-lo x)) (rnd 'up bfsqrt (ival-hi x))
        err? err))
