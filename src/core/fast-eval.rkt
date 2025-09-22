#lang racket

(require racket/match
         racket/list
         math/bigfloat
         "../utils/float.rkt"
         "../utils/errors.rkt"
         "../syntax/types.rkt"
         "rival.rkt")

(provide fast-real-apply)

(define FAST-PREC-LOW 256)
(define FAST-PREC-HIGH 384)

(define (bf-eval expr env)
  (match expr
    [(? number?) (bf (inexact->exact expr))]
    [(? boolean?) expr]
    [(? symbol?)
     (hash-ref env expr (λ () (raise-herbie-sampling-error (format "Unknown variable: ~a" expr))))]
    [`(if ,c ,t ,e)
     (if (bf-true? (bf-eval-bool c env))
         (bf-eval t env)
         (bf-eval e env))]
    [`(neg ,x) (bf- 0.bf (bf-eval x env))]
    [`(+ ,x ,y) (bf+ (bf-eval x env) (bf-eval y env))]
    [`(- ,x ,y) (bf- (bf-eval x env) (bf-eval y env))]
    [`(* ,x ,y) (bf* (bf-eval x env) (bf-eval y env))]
    [`(/ ,x ,y) (bf/ (bf-eval x env) (bf-eval y env))]
    [`(fma ,a ,b ,c) (bf+ (bf* (bf-eval a env) (bf-eval b env)) (bf-eval c env))]
    [`(abs ,x) (bfabs (bf-eval x env))]
    [`(fabs ,x) (bfabs (bf-eval x env))]
    [`(sqrt ,x) (bfsqrt (bf-eval x env))]
    [`(exp ,x) (bfexp (bf-eval x env))]
    [`(log ,x) (bflog (bf-eval x env))]
    [`(sin ,x) (bfsin (bf-eval x env))]
    [`(cos ,x) (bfcos (bf-eval x env))]
    [`(tan ,x) (bftan (bf-eval x env))]
    [`(asin ,x) (bfasin (bf-eval x env))]
    [`(acos ,x) (bfacos (bf-eval x env))]
    [`(atan ,x) (bfatan (bf-eval x env))]
    [`(atan2 ,y ,x) (bfatan2 (bf-eval y env) (bf-eval x env))]
    [`(pow ,x ,y) (bfexpt (bf-eval x env) (bf-eval y env))]
    [`(hypot ,x ,y)
     (bfsqrt (bf+ (bf* (bf-eval x env) (bf-eval x env)) (bf* (bf-eval y env) (bf-eval y env))))]
    [`(min ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf< vx vy) vx vy))]
    [`(max ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf> vx vy) vx vy))]
    [`(fmin ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf< vx vy) vx vy))]
    [`(fmax ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf> vx vy) vx vy))]
    [`(copysign ,x ,y)
     (define xv (bf-eval x env))
     (define yv (bf-eval y env))
     (if (bfnegative? yv)
         (bf- 0.bf (bfabs xv))
         (bfabs xv))]
    [else (raise-herbie-sampling-error (format "Unsupported operation in fast evaluator: ~s" expr))]))

(define (bf-eval-bool expr env)
  (match expr
    [(? boolean?) expr]
    [`(TRUE) #t]
    [`(FALSE) #f]
    [`(not ,c) (not (bf-eval-bool c env))]
    [`(and . ,cs) (andmap (λ (c) (bf-eval-bool c env)) cs)]
    [`(or . ,cs) (ormap (λ (c) (bf-eval-bool c env)) cs)]
    [`(<= ,x ,y) (bf<= (bf-eval x env) (bf-eval y env))]
    [`(< ,x ,y) (bf< (bf-eval x env) (bf-eval y env))]
    [`(>= ,x ,y) (bf>= (bf-eval x env) (bf-eval y env))]
    [`(> ,x ,y) (bf> (bf-eval x env) (bf-eval y env))]
    [`(= ,x ,y) (bf= (bf-eval x env) (bf-eval y env))]
    [else (raise-herbie-sampling-error (format "Unsupported boolean in fast evaluator: ~s" expr))]))

(define (bf-true? b)
  (and (boolean? b) b))

(define (pt->bf vars var-reprs pt)
  (for/hash ([v (in-vector vars)]
             [repr (in-vector var-reprs)]
             [val (in-vector pt)])
    (values v ((representation-repr->bf repr) val))))

(define (round-bf repr x)
  (match (representation-type repr)
    ['bool x]
    ['real
     (define r ((representation-bf->repr repr) x))
     ((representation-repr->bf repr) r)]))

;; Attempt a fast evaluation; on success returns (values 'valid exs)
;; exs are representation-typed values, just like Rival returns.
;; On precondition false => (values 'invalid #f)
;; On domain/unsupported/instability => (values 'fallback #f)
(define (fast-real-apply compiler pt)
  (match-define (real-compiler pre vars var-reprs exprs reprs _ _) compiler)
  (define env (pt->bf vars var-reprs pt))

  (define pre-ok?
    (with-handlers ([exn:fail? (λ (e) #f)])
      (parameterize ([bf-precision FAST-PREC-LOW])
        (bf-true? (bf-eval-bool pre env)))))
  (unless pre-ok?
    (values 'invalid #f))

  (define unsupported? #f)
  (define domain-ok? #t)
  (define (eval-at-prec prec)
    (parameterize ([bf-precision prec])
      (for/list ([expr (in-list exprs)])
        (with-handlers ([exn:fail? (λ (_)
                                     (set! unsupported? #t)
                                     0.bf)])
          (with-handlers ([exn:fail:contract? (λ (_)
                                                (set! domain-ok? #f)
                                                0.bf)])
            (bf-eval expr env))))))

  (define xs-low (eval-at-prec FAST-PREC-LOW))
  (when (or (not domain-ok?) unsupported?)
    ; Back to rival if we cant be in the domain or not support
    (values 'fallback #f))
  (define xs-high (eval-at-prec FAST-PREC-HIGH))
  (when (or (not domain-ok?) unsupported?)
    (values 'fallback #f))

  (define stable?
    (for/and ([x0 (in-list xs-low)]
              [x1 (in-list xs-high)]
              [repr (in-vector reprs)])
      (let ([rt0 (round-bf repr x0)]
            [rt1 (round-bf repr x1)])
        (cond
          [(eq? (representation-type repr) 'bool) (equal? rt0 rt1)]
          [else (bf= rt0 rt1)]))))

  (if (not stable?)
      (values 'fallback #f) ; Back to rival
      (let ([exs (for/list ([x (in-list xs-high)]
                            [repr (in-vector reprs)])
                   (match (representation-type repr)
                     ['bool x]
                     ['real ((representation-bf->repr repr) x)]))])
        (values 'valid exs))))
