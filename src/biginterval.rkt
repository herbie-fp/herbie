#lang racket

(require math/bigfloat)
(require "common.rkt" "programs.rkt" "errors.rkt")

(struct ival (lo hi err? err) #:transparent)

(provide (contract-out
          [struct ival ([lo bigfloat?] [hi bigfloat?] [err? boolean?] [err boolean?])]
          [mk-ival (-> real? ival?)]
          [ival-add (-> ival? ival? ival?)]
          [ival-sub (-> ival? ival? ival?)]
          [ival-neg (-> ival? ival?)]
          [ival-mult (-> ival? ival? ival?)]
          [ival-exp (-> ival? ival?)]
          [ival-log (-> ival? ival?)]
          [ival-sqrt (-> ival? ival?)]
          #;[ival-pow (-> ival? ival? ival?)]
          [ival-div (-> ival? ival? ival?)])
         ival-compile
         evaluate-compiled
         supported-expr?
         histogram)

(define (mk-ival x)
  (define err? (or (nan? x) (infinite? x)))
  (define x* (bf x)) ;; TODO: Assuming that float precision < bigfloat precision
  (ival x* x* err? err?))

(define (ival-pi)
  (ival (rnd 'down (λ () pi.bf)) (rnd 'up (λ () pi.bf)) #f #f))

(define (ival-e)
  (ival (rnd 'down bfexp 1.bf) (rnd 'up bfexp 1.bf) #f #f))

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

(define const-mapping
  (hash 'PI ival-pi 'E ival-e))

(define fn-mapping
  (hash '- ival-sub '+ ival-add '* ival-mult '/ ival-div 'exp ival-exp 'log ival-log 'sqrt ival-sqrt))

(define histogram (make-hash))

(define (ival-compile prog)
  (eval
   (list 'lambda
         (program-variables prog)
         (let loop ([expr (program-body prog)])
           (match expr
             [(? real?) (list mk-ival expr)]
             [(? constant?) (list (dict-ref const-mapping expr))]
             [(? variable?)
              (list mk-ival expr)]
             [(list '- arg)
              (list ival-neg (loop arg))]
             [(list op args ...)
              (define argvals (map loop args))
              (cons (dict-ref fn-mapping op) argvals)])))
   common-eval-ns))

(define (evaluate-compiled fn vals #:precision [precision 80])
  (let loop ([precision precision])
    (parameterize ([bf-precision precision])
      (when (> precision (*max-mpfr-prec*))
        (raise-herbie-error "Exceeded MPFR precision limit."
                            #:url "faq.html#mpfr-prec-limit"))
      (match-define (ival lo hi err? err) (apply fn vals))
      (cond
       [err
        (hash-update! histogram precision add1 0)
        +nan.0]
       [(and (= (bigfloat->flonum lo) (bigfloat->flonum hi)) (not err?))
        (hash-update! histogram precision add1 0)
        (bigfloat->flonum lo)]
       [else
        (loop (inexact->exact (round (* precision 2))))]))))

(define (supported-expr? expr)
  (match expr
    [(list op args ...)
     (and (hash-has-key? fn-mapping op)
          (andmap supported-expr? args))]
    [(or (? variable?) (? constant?)) true]))
