#lang racket

(require math/bigfloat)
(require "../main.rkt"
         biginterval)

(provide interval-evaluate)

(define all-constants
  `((PI ,ival-pi ,(lambda () (iv pi.bf pi.bf)))
    (E ,ival-e ,(λ () (iv (bfexp 1.bf) (bfexp 1.bf))))
    (INFINITY ,(λ () (mk-ival +inf.bf)) ,(λ () (iv +inf.bf +inf.bf)))
    (NAN ,(λ () (mk-ival +nan.bf)) ,(λ () (iv +nan.bf +nan.bf)))
    (TRUE ,(λ () (ival-bool true)) #f)
    (FALSE ,(λ () (ival-bool false)) #f)))

(define all-ops
  `((+ ,ival-add ,ivadd) (- ,ival-sub ,ivsub)
                         (neg ,ival-neg ,ivneg)
                         (* ,ival-mult ,ivmul)
                         (/ ,ival-div ,ivdiv)
                         (acos ,ival-acos ,ivacos)
                         (acosh ,ival-acosh ,ivacosh)
                         (asin ,ival-asin ,ivasin)
                         (asinh ,ival-asinh ,ivasinh)
                         (atan ,ival-atan ,ivatan)
                         (atan2 ,ival-atan2 ,ivatan2)
                         (atanh ,ival-atanh ,ivatanh)
                         (cbrt ,ival-cbrt ,ivcbrt)
                         (ceil ,ival-ceil #f)
                         (copysign ,ival-copysign #f)
                         (cos ,ival-cos ,ivcos)
                         (cosh ,ival-cosh ,ivcosh)
                         (erf ,ival-erf #f)
                         (erfc ,ival-erfc #f)
                         (exp ,ival-exp ,ivexp)
                         (exp2 ,ival-exp2 ,ivexp2)
                         (expm1 ,ival-expm1 ,ivexpm1)
                         (fabs ,ival-fabs ,ivfabs)
                         (fdim ,ival-fdim #f)
                         (floor ,ival-floor #f)
                         (fma ,ival-fma #f)
                         (fmax ,ival-fmax #f)
                         (fmin ,ival-fmin #f)
                         (fmod ,ival-fmod #f)
                         (hypot ,ival-hypot ,ivhypot)
                         (log ,ival-log ,ivlog)
                         (log10 ,ival-log10 ,ivlog10)
                         (log1p ,ival-log1p ,ivlog1p)
                         (log2 ,ival-log2 ,ivlog2)
                         (logb ,ival-logb #f)
                         (pow ,ival-pow #f)
                         (remainder ,ival-remainder #f)
                         (rint ,ival-rint #f)
                         (round ,ival-round #f)
                         (sin ,ival-sin ,ivsin)
                         (sinh ,ival-sinh ,ivsinh)
                         (sqrt ,ival-sqrt ,ivsqrt)
                         (tan ,ival-tan ,ivtan)
                         (tanh ,ival-tanh ,ivtanh)
                         (trunc ,ival-trunc #f)
                         (if ,ival-if #f)
                         (== ,ival-== #f)
                         (!= ,ival-!= #f)
                         (< ,ival-< #f)
                         (> ,ival-> #f)
                         (<= ,ival-<= #f)
                         (>= ,ival->= #f)
                         (not ,ival-not #f)
                         (and ,ival-and #f)
                         (or ,ival-or #f)))

(define op-table
  (make-hash (for/list ([op-list all-ops])
               (cons (first op-list) (cons (second op-list) (third op-list))))))

(define constant-table
  (make-hash (for/list ([op-list all-constants])
               (cons (first op-list) (cons (second op-list) (third op-list))))))

(define (convert-symbol unprocessed-symbol)
  (string->symbol (first (string-split (symbol->string unprocessed-symbol) "."))))

(define (get-ival-from-table table symbol use-mpfi?)
  (cond
    [(not (hash-has-key? table symbol)) (error (format "Didn't find in table ~a" symbol))]
    [(and use-mpfi? (cdr (hash-ref table symbol))) (cdr (hash-ref table symbol))]
    [(and (not use-mpfi?) (car (hash-ref table symbol))) (car (hash-ref table symbol))]
    [else (lambda xs #f)]))

(define (convert-number expression use-mpfi?)
  (if use-mpfi?
      (iv expression)
      (mk-ival (bf expression))))

(define (interval-evaluate expression variables point use-mpfi?)
  (cond
    [(list? expression)
     (define children
       (map (lambda (exp) (interval-evaluate exp variables point use-mpfi?)) (rest expression)))
     (if (andmap identity children)
         (apply (get-ival-from-table op-table (first expression) use-mpfi?) children)
         #f)]
    [(member expression variables)
     (convert-number (list-ref point (index-of variables expression)) use-mpfi?)]
    [(number? expression) (convert-number expression use-mpfi?)]
    [(get-ival-from-table constant-table expression use-mpfi?)
     ((get-ival-from-table constant-table expression use-mpfi?))]
    [else (error (format "Undefined symbol ~a" expression))]))
