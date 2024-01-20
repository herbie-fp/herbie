#lang racket

(require racket/set math/bigfloat)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "common.rkt"
         "ground-truth.rkt" "syntax/sugar.rkt")

(provide actual-errors predicted-errors)

(define (actual-errors expr pcontext)
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr)
                                            (*context*))))))
  
  (define pt-worst-subexprs
    (foldr append
           '()
           (for/list ([pt-errors (in-list pt-errorss)]
                      [(pt _) (in-pcontext pcontext)])
             (define sub-error (map cons subexprs pt-errors))
             (define filtered-sub-error
               (filter (lambda (p) (> (cdr p) 16)) sub-error))
             (define mapped-sub-error
               (map (lambda (p) (cons (car p) pt))
                    filtered-sub-error))
             (if (empty? mapped-sub-error)
                 (list (cons #f pt))
                 mapped-sub-error))))

  (for/hash ([group (in-list (group-by car pt-worst-subexprs))])
    (let ([key (caar group)])
      (values key (map cdr group)))))

(define (same-sign? a b)
  (or (and (positive? a) (positive? b))
      (and (negative? a) (negative? b))))
 
(define (predicted-errors expr ctx pctx)
  
  (define old-precision (bf-precision))
  (bf-precision 1000)
  (define cond-thres (bf 100))
  
  (define subexprs
    (all-subexpressions-rev expr (context-repr ctx)))
  (define subexprs-list (map car subexprs))
  (define spec-list (map prog->spec subexprs-list))
 
  (define ctx-list
    (for/list ([subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (cdr subexpr)])))

  (define repr-hash
    (make-immutable-hash (map cons
                              subexprs-list
                              (map context-repr ctx-list))))
 
  (define subexprs-fn (eval-progs-real spec-list ctx-list))
 
  (define error-count-hash
    (make-hash (map (lambda (x) (cons x '()))
                    (cons #f subexprs-list))))
  
  (for ([(pt _) (in-pcontext pctx)])
    (define has-errored? #f)
    (define (mark-erroneous! expr)
      (hash-update! error-count-hash expr (lambda (x) (set-add x pt)))
      (set! has-errored? expr))
    
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons subexprs-list exacts)))
    (define (exacts-ref subexpr)
      (define exacts-val (hash-ref exacts-hash subexpr))
      (if (boolean? exacts-val)
          exacts-val
          (bigfloat->flonum ((representation-repr->bf
                              (hash-ref repr-hash subexpr))
                             exacts-val))))
 
    (for/list ([subexpr subexprs-list])
      (define subexpr-val (exacts-ref subexpr))
      
      (match subexpr
        [(list (or '+.f64 '+.f32) larg rarg)
         #:when (or (list? larg) (list? rarg))
         (define larg-val (exacts-ref larg))
         (define rarg-val (exacts-ref rarg))
         (define x+y (+ larg-val rarg-val))
         (define cond-x (bfabs (bf/ (bf larg-val)
                                    (bf subexpr-val))))
         (define cond-y (bfabs (bf/ (bf rarg-val)
                                    (bf subexpr-val))))
         
         (cond
           ; Condition number hallucination
           ; Both R(x + y) and R(x) + R(y) underflow
           ; This causes the condition number to jump up,
           ; with no real error
           [(and (= x+y 0.0) (zero? subexpr-val)) #f]
           
           ; nan rescue:
           ; R(+-inf) + R(-+inf) = nan, but should actually
           ; be inf 
           [(and (infinite? larg-val)
                 (infinite? rarg-val)
                 (not (same-sign? larg-val rarg-val))
                 (not (nan? subexpr-val)))
            (mark-erroneous! subexpr)]
           
           ; inf rescue:
           ; R(inf) + y = non inf value (inf rescue)
           [(and (or (infinite? larg-val)
                     (infinite? rarg-val))
                 (not (infinite? subexpr-val)))
            (mark-erroneous! subexpr)]
           
           ; High condition number:
           ; CN(+, x, y) = |x / x + y| 
           [(or (bf> cond-x cond-thres)
                (bf> cond-y cond-thres))
            (mark-erroneous! subexpr)]
           [else #f])]
        
        [(list (or '-.f64 '-.f32) larg rarg)
         #:when (or (list? larg) (list? rarg))
         (define larg-val (exacts-ref larg))
         (define rarg-val (exacts-ref rarg))
         (define x-y (- larg-val rarg-val))
         (define cond-x (bfabs (bf/ (bf larg-val)
                                    (bf subexpr-val))))
         (define cond-y (bfabs (bf/ (bf rarg-val)
                                    (bf subexpr-val))))
         
         (cond
           ; Condition number hallucination:
           ; When x - y correctly underflows, CN is high
           ; even though the answer is correct
           [(and (= x-y 0.0) (zero? subexpr-val)) #f]

           ; nan rescue:
           ; inf - inf = nan but should actually get an inf
           [(and (infinite? larg-val)
                 (infinite? rarg-val)
                 (same-sign? larg-val rarg-val)
                 (not (nan? subexpr-val)))
            (mark-erroneous! subexpr)]

           ; inf rescue
           ; If x or y overflow and the other arg rescues
           ; it
           [(and (or (infinite? larg-val)
                     (infinite? rarg-val))
                 (not (infinite? subexpr-val)))
            (mark-erroneous! subexpr)]

           ; High condition number:
           ; CN(+, x, y) = |x / x - y|
           [(or (bf> cond-x cond-thres)
                (bf> cond-y cond-thres))
            (mark-erroneous! subexpr)]
           [else #f])]

        [(list (or 'sin.f64 'sin.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         (define x.bf (bf arg-val))
         (define cot-x (bfabs (bfcot x.bf)))
         (define cond-no (bf* (bfabs x.bf) cot-x))

         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x.bf) cond-thres))
            (mark-erroneous! subexpr)]
           [(and (bf> cond-no cond-thres)
                 (bf> cot-x cond-thres))
            (mark-erroneous! subexpr)]
           [else #f])]

        [(list (or 'cos.f64 'cos.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         (define x.bf (bf arg-val))
         (define tan-x (bfabs (bftan x.bf)))
         (define cond-no (bf* (bfabs x.bf) tan-x))

         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x.bf) cond-thres))
            (mark-erroneous! subexpr)]
           [(and (bf> cond-no cond-thres)
                 (bf> tan-x cond-thres))
            (mark-erroneous! subexpr)]
           [else #f])]

        [(list (or 'tan.f64 'tan.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         (define x.bf (bf arg-val))
         (define tot-x (bfabs (bf+ (bfcot x.bf) (bftan x.bf))))
         (define cond-no (bf* (bfabs x.bf) tot-x))

         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x.bf) cond-thres))
            (mark-erroneous! subexpr)]
           [(and (bf> cond-no cond-thres)
                 (bf> tot-x cond-thres))
            (mark-erroneous! subexpr)]
           [else #f])]
        
        [(list (or 'sqrt.f64 'sqrt.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))

         ; Under/overflow rescue:
         (and (or (zero? arg-val)
                  (infinite? arg-val))
              (not (= subexpr-val arg-val))
              (mark-erroneous! subexpr))]

        [(list (or 'cbrt.f64 'cbrt.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         
         ; Under/overflow rescue:
         (and (or (zero? arg-val)
                  (infinite? arg-val))
              (not (= subexpr-val arg-val))
              (mark-erroneous! subexpr))]

        [(list (or '/.f64 '/.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         
         (cond
           ;; if the numerator underflows and the denominator:
           ;; - underflows, nan could be rescued
           [(and (zero? x) (zero? y)
                 (not (nan? subexpr-val)))
            (eprintf "u/u\n")
            (mark-erroneous! subexpr)]
           ;; - is small enough, 0 underflow could be rescued
           [(and (zero? x)
                 (not (zero? subexpr-val)))
            (eprintf "u/n\n")
            (mark-erroneous! subexpr)]
           ;; - overflows, no rescue is possible

           ;; if the numerator overflows and the denominator:
           ;; - overflows, nan could be rescued
           [(and (infinite? x) (infinite? y)
                 (not (nan? subexpr-val)))
            (eprintf "o/o\n")
            (mark-erroneous! subexpr)]
           ;; - is large enough, inf overflow can be rescued
           [(and (infinite? x)
                 (not (infinite? subexpr-val)))
            (eprintf "o/n\n")
            (mark-erroneous! subexpr)]
           ;; - underflow, no rescue is possible

           ;; if the numerator is normal and the denominator:
           ;; - overflows, then a rescue is possible
           [(and (infinite? y)
                 (not (zero? subexpr-val)))
            (eprintf "n/o\n")
            (mark-erroneous! subexpr)]
           ;; - underflows, then a rescue is possible
           [(and (zero? y)
                 (not (infinite? subexpr-val)))
            (eprintf "n/u\n")
            (mark-erroneous! subexpr)]
           ;; - is normal, then no rescue is possible
           [else  (eprintf "n/n\n") #f])]

        [(list (or '*.f64 '*.f32) x-ex y-ex)
         #:when (or (list? x-ex) [list? y-ex])
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         
         (cond
           ;; if one operand underflows and the other overflows, then nan must
           ;; be rescued.
           [(and (infinite? x)
                 (zero? y)
                 (not (nan? subexpr-val)))
            (mark-erroneous! subexpr)]
           [(and (zero? x)
                 (infinite? y)
                 (not (nan? subexpr-val)))
            (mark-erroneous! subexpr)]

           ;; If one operand is normal and the other overflows then, inf rescue
           ;; could occur
           [(and (or (infinite? x)
                     (infinite? y))
                 (not (infinite? subexpr-val)))
            (mark-erroneous! subexpr)]

           [(and (or (zero? x)
                     (zero? y))
                 (not (zero? subexpr-val)))
            (mark-erroneous! subexpr)]

           ;; If both normal then no error
           [else #f])]
        
        [(list (or 'log.f64 'log.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         (define cond-num (bfabs (bf/ 1.bf
                                      (bf subexpr-val))))
         (cond
           ; Condition number hallucination:
           ; Condition number is high when x = 1,
           ; but x is exactly 1, so there is no error
           [(and (= arg-val 1.0) (= subexpr-val 0.0)) #f]
           
           ; overflow rescue:
           [(infinite? arg-val) (mark-erroneous! subexpr)]
           
           ; underflow rescue:
           [(zero? arg-val) (mark-erroneous! subexpr)]

           ; High Condition Number:
           ; CN(log, x) = |1 / log(x)|
           [(bf> cond-num cond-thres)
            (mark-erroneous! subexpr)]
           
           [else #f])]
        
        [(list (or 'exp.f64 'exp.f32) arg)
         #:when (list? arg)
         (define arg-val (exacts-ref arg))
         
         (cond
           ; Condition Number Hallucination:
           ; When x is large enough that exp(x) overflows,
           ; condition number is also high.
           [(and (infinite? (exp arg-val))
                 (infinite? subexpr-val))
            #f]

           ; Condition Number Hallucination:
           ; When x is large enough (negative) that exp(x)
           ; underflows, condition number is also high
           [(and (= (exp arg-val) 0.0)
                 (= subexpr-val 0.0))
            #f]

           ; High Condition Number:
           ; CN(exp, x) = |x|
           [(> (abs arg-val) 100)
            (mark-erroneous! subexpr)]
           
           [else #f])]

        ; FIXME need to rework from scratch
        [(list (or 'pow.f64 'pow.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define x.bf (bf x))
         (define y.bf (bf y))
         (define x^y (expt x y))
         (define cond-x (bfabs y.bf))
         (define cond-y (bfabs (bf* y.bf (bflog x.bf))))

         (cond
           ;; Hallucination:
           ;; x has a large exponent and y is 1. The ylogx is large but there is
           ;; no error because the answer is exactly x
           [(and (= y 1.0)
                 (= x subexpr-val)) #f]
           
           ;; Hallucination:
           ;; if x is large enough that x^y overflows, the condition number also
           ;; is very large, but the answer correctly overflows
           [(and (> y 1)
                 (infinite? x^y)
                 (infinite? subexpr-val)) #f]

           ;; if x is small enough and y is large enough that x^y underflows,
           ;; the condition number also gets very large, but the answer
           ;; correctly underflows
           [(and (> y 1)
                 (zero? x^y)
                 (zero? subexpr-val)) #f]

           [(and (or (zero? x)
                     (infinite? x))
                 (not (= subexpr-val x^y)))
            (mark-erroneous! subexpr)]
            
           [(or (bf> cond-x cond-thres)
                (bf> cond-y cond-thres))
            (mark-erroneous! subexpr)]
           
           [else #f])]
        
        [(list (or 'acos.f64 'acos.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define x.bf (bf x))
         (define cond-x (bfabs
                         (bf/ x.bf
                              (bf* (bfsqrt
                                    (bf- 1.bf
                                         (bf* x.bf x.bf)))
                                   (bf subexpr-val)))))
         
         (cond
           ; Condition number hallucinations:
           ; acos(1) == 0
           [(and (= x 1.0) (= subexpr-val 0.0)) #f]

           ; acos(-1) == pi
           [(and (= x -1.0) (= subexpr-val pi)) #f]
           
           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)acos(x))|
           [(bf> cond-x cond-thres)
            (mark-erroneous! subexpr)]
           
           [else #f])]

        [(list (or 'asin.f64 'asin.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define x.bf (bf x))
         (define cond-x (bfabs
                         (bf/ x.bf
                              (bf* (bfsqrt
                                    (bf- 1.bf
                                         (bf* x.bf x.bf)))
                                   (bf subexpr-val)))))
         
         (cond
           ; Condition Number hallucinations:
           ; asin(1) == pi/2
           [(and (= x 1.0) (= subexpr-val (/ pi 2.0))) #f]

           ; asin(-1) == -pi/2
           [(and (= x -1.0)
                 (= subexpr-val (/ (- pi) 2.0))) #f]

           ; asin(0) == 0
           [(and (= x 0.0) (= subexpr-val 0.0)) #f]
           
           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)asin(x))|
           [(bf> cond-x cond-thres) (mark-erroneous! subexpr)]
           [else #f])]
        [_ #f]))
    (unless has-errored?
      (mark-erroneous! #f)))
  (bf-precision old-precision)
  error-count-hash)
