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
     (hash-ref env
               expr
               (λ () (raise-herbie-sampling-error (format "Unknown variable or constant: ~a" expr))))]
    [`(if ,c ,t ,e)
     (if (bf-true? (bf-eval-bool c env))
         (bf-eval t env)
         (bf-eval e env))]
    [`(cast ,x) (bf-eval x env)]
    [`(then ,_ ,b) (bf-eval b env)]
    [`(error ,_) (raise-herbie-sampling-error "error called in fast evaluator")]
    [`(neg ,x) (bf- 0.bf (bf-eval x env))]
    [`(abs ,x) (bfabs (bf-eval x env))]
    [`(fabs ,x) (bfabs (bf-eval x env))]
    [`(sqrt ,x) (bfsqrt (bf-eval x env))]
    [`(cbrt ,x) (bfexpt (bf-eval x env) (bf/ 1.bf 3.bf))]
    [`(exp ,x) (bfexp (bf-eval x env))]
    [`(exp2 ,x) (bfexpt 2.bf (bf-eval x env))]
    [`(expm1 ,x) (bf- (bfexp (bf-eval x env)) 1.bf)]
    [`(log ,x) (bflog (bf-eval x env))]
    [`(log2 ,x)
     (define vx (bf-eval x env))
     (bf/ (bflog vx) (bflog 2.bf))]
    [`(log10 ,x)
     (define vx (bf-eval x env))
     (bf/ (bflog vx) (bflog 10.bf))]
    [`(log1p ,x) (bflog (bf+ 1.bf (bf-eval x env)))]
    [`(sin ,x) (bfsin (bf-eval x env))]
    [`(cos ,x) (bfcos (bf-eval x env))]
    [`(tan ,x) (bftan (bf-eval x env))]
    [`(asin ,x) (bfasin (bf-eval x env))]
    [`(acos ,x) (bfacos (bf-eval x env))]
    [`(atan ,x) (bfatan (bf-eval x env))]
    [`(sinh ,x) (bfsinh (bf-eval x env))]
    [`(cosh ,x) (bfcosh (bf-eval x env))]
    [`(tanh ,x) (bftanh (bf-eval x env))]
    [`(asinh ,x) (bfasinh (bf-eval x env))]
    [`(acosh ,x) (bfacosh (bf-eval x env))]
    [`(atanh ,x) (bfatanh (bf-eval x env))]
    [`(+ ,x ,y) (bf+ (bf-eval x env) (bf-eval y env))]
    [`(- ,x ,y) (bf- (bf-eval x env) (bf-eval y env))]
    [`(* ,x ,y) (bf* (bf-eval x env) (bf-eval y env))]
    [`(/ ,x ,y) (bf/ (bf-eval x env) (bf-eval y env))]
    [`(fma ,a ,b ,c) (bf+ (bf* (bf-eval a env) (bf-eval b env)) (bf-eval c env))]
    [`(pow ,x ,y) (bfexpt (bf-eval x env) (bf-eval y env))]
    [`(pow2 ,x) (bfexpt 2.bf (bf-eval x env))]
    [`(atan2 ,y ,x) (bfatan2 (bf-eval y env) (bf-eval x env))]
    [`(hypot ,x ,y)
     (define vx (bf-eval x env))
     (define vy (bf-eval y env))
     (bfsqrt (bf+ (bf* vx vx) (bf* vy vy)))]
    [`(fdim ,x ,y)
     (define vx (bf-eval x env))
     (define vy (bf-eval y env))
     (if (bf> (bf- vx vy) 0.bf)
         (bf- vx vy)
         0.bf)]
    [`(fmin ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf< vx vy) vx vy))]
    [`(fmax ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf> vx vy) vx vy))]
    [`(min ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf< vx vy) vx vy))]
    [`(max ,x ,y)
     (let ([vx (bf-eval x env)]
           [vy (bf-eval y env)])
       (if (bf> vx vy) vx vy))]
    [`(copysign ,x ,y)
     (define xv (bf-eval x env))
     (define yv (bf-eval y env))
     (if (bfnegative? yv)
         (bf- 0.bf (bfabs xv))
         (bfabs xv))]
    ; Unimplemented numeric ops fall back to rival
    [`(ceil ,_) (raise-herbie-sampling-error "ceil unsupported in fast evaluator")]
    [`(floor ,_) (raise-herbie-sampling-error "floor unsupported in fast evaluator")]
    [`(trunc ,_) (raise-herbie-sampling-error "trunc unsupported in fast evaluator")]
    [`(rint ,_) (raise-herbie-sampling-error "rint unsupported in fast evaluator")]
    [`(round ,_) (raise-herbie-sampling-error "round unsupported in fast evaluator")]
    [`(fmod ,_ ,_) (raise-herbie-sampling-error "fmod unsupported in fast evaluator")]
    [`(remainder ,_ ,_) (raise-herbie-sampling-error "remainder unsupported in fast evaluator")]
    [`(erf ,_) (raise-herbie-sampling-error "erf unsupported in fast evaluator")]
    [`(erfc ,_) (raise-herbie-sampling-error "erfc unsupported in fast evaluator")]
    [`(lgamma ,_) (raise-herbie-sampling-error "lgamma unsupported in fast evaluator")]
    [`(tgamma ,_) (raise-herbie-sampling-error "tgamma unsupported in fast evaluator")]
    [`(logb ,_) (raise-herbie-sampling-error "logb unsupported in fast evaluator")]
    [else (raise-herbie-sampling-error (format "Unsupported operation in fast evaluator: ~s" expr))]))

(define (bf-eval-bool expr env)
  (match expr
    [(? boolean?) expr]
    [`(TRUE) #t]
    [`(FALSE) #f]
    [`(not ,c) (not (bf-eval-bool c env))]
    [`(and . ,cs) (andmap (λ (c) (bf-eval-bool c env)) cs)]
    [`(or . ,cs) (ormap (λ (c) (bf-eval-bool c env)) cs)]
    [`(assert ,c) (bf-eval-bool c env)]
    [`(<= ,x ,y) (bf<= (bf-eval x env) (bf-eval y env))]
    [`(< ,x ,y) (bf< (bf-eval x env) (bf-eval y env))]
    [`(>= ,x ,y) (bf>= (bf-eval x env) (bf-eval y env))]
    [`(> ,x ,y) (bf> (bf-eval x env) (bf-eval y env))]
    [`(= ,x ,y) (bf= (bf-eval x env) (bf-eval y env))]
    [`(== ,x ,y) (bf= (bf-eval x env) (bf-eval y env))]
    [`(!= ,x ,y) (not (bf= (bf-eval x env) (bf-eval y env)))]
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

(define (fast-real-apply compiler pt)
  ; Fallback to rival
  (with-handlers ([exn:fail? (λ (_) (values 'fallback #f))])
    (match-define (real-compiler pre vars var-reprs exprs reprs _ _) compiler)
    (define env (pt->bf vars var-reprs pt))

    (define pre-ok?
      (parameterize ([bf-precision FAST-PREC-LOW])
        (bf-true? (bf-eval-bool pre env))))
    (unless pre-ok?
      (values 'invalid #f))

    (define (eval-at-prec prec)
      (parameterize ([bf-precision prec])
        (for/list ([expr (in-list exprs)])
          (bf-eval expr env))))

    (define xs-low (eval-at-prec FAST-PREC-LOW))
    (define xs-high (eval-at-prec FAST-PREC-HIGH))

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
        (values 'fallback #f) ; Fallback to rival
        (let ([exs (for/list ([x (in-list xs-high)]
                              [repr (in-vector reprs)])
                     (match (representation-type repr)
                       ['bool x]
                       ['real ((representation-bf->repr repr) x)]))])
          (values 'valid exs)))))
