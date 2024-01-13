#lang racket

(require racket/flonum racket/set)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt"
         "common.rkt" "ground-truth.rkt")

(provide actual-errors predicted-errors)

(define (actual-errors expr pcontext)
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr) (*context*))))))
  
  (define pt-worst-subexprs
    (foldr append
           '()
           (for/list ([pt-errors (in-list pt-errorss)]
                      [(pt _) (in-pcontext pcontext)])
             (define sub-error (map cons subexprs pt-errors))
             (define filtered-sub-error (filter (lambda (p) (> (cdr p) 16))
                                                sub-error))
             (define mapped-sub-error (map (lambda (p) (cons (car p) pt))
                                           filtered-sub-error))
             (if (empty? mapped-sub-error)
                 (list (cons #f pt))
                 mapped-sub-error))))

  (for/hash ([group (in-list (group-by car pt-worst-subexprs))])
    (let ([key (caar group)])
      (values key (map cdr group)))))

(define is-inexact? list?)
(define (same-sign? a b)
  (or (and (positive? a) (positive? b))
      (and (negative? a) (negative? b))))
 
(define (predicted-errors expr ctx pctx)
  
  (define subexprs
    (all-subexpressions-rev expr (context-repr ctx)))
  (define subexprs-list (map car subexprs))
 
  (define ctx-list
    (for/list ([subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (cdr subexpr)])))
 
  (define subexprs-fn (eval-progs-real subexprs-list ctx-list))
 
  (define error-count-hash
    (make-hash (map (lambda (x) (cons x '()))
                    (cons #f subexprs-list))))
 
  (define (mark-erroneous! expr pt)
    (hash-update! error-count-hash expr (lambda (x) (set-add x pt))))
  
  (for ([(pt _) (in-pcontext pctx)])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons subexprs-list exacts)))
 
    (define uflow-hash (make-hash (map (lambda (x) (cons x #f))
                                       subexprs-list)))
    (define (underflow? subexpr) (hash-ref uflow-hash subexpr))
    (define oflow-hash (make-hash (map (lambda (x) (cons x #f))
                                       subexprs-list)))
    (define (overflow? subexpr) (hash-ref oflow-hash subexpr))
 
    (unless
        (ormap identity
               (for/list ([subexpr subexprs-list])
                 (define subexpr-val (hash-ref exacts-hash subexpr))

                 (cond
                   ; NOTE need better way to see if a calculation
                   ; underflowed
                   [(fl= subexpr-val 0.0)
                    (hash-set! uflow-hash subexpr #t)]
                   [(infinite? subexpr-val)
                    (hash-set! oflow-hash subexpr #t)])
                 
                 (match subexpr
                   [(list (or '+.f64 '+.f32) larg rarg)
                    #:when (or (is-inexact? larg) (is-inexact? rarg))
                    (define larg-val (hash-ref exacts-hash larg))
                    (define rarg-val (hash-ref exacts-hash rarg))
                    (define x+y (+ larg-val rarg-val))
                    (define cond-x (abs (/ larg-val subexpr-val)))
                    (define cond-y (abs (/ rarg-val subexpr-val)))
                    
                    (cond
                      ; Condition number hallucination
                      ; Both R(x + y) and R(x) + R(y) underflow
                      ; This causes the condition number to jump up,
                      ; with no real error
                      [(and (= x+y 0.0) (underflow? subexpr)) #f]

                      ; nan rescue:
                      ; R(+-inf) + R(-+inf) = nan, but should actually
                      ; be inf 
                      [(and (infinite? larg-val)
                            (infinite? rarg-val)
                            (not (same-sign? larg-val rarg-val))
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]

                      ; inf rescue:
                      ; R(inf) + y = non inf value (inf rescue)
                      [(and (or (infinite? larg-val)
                                (infinite? rarg-val))
                            (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; High condition number:
                      ; CN(+, x, y) = |x / x + y| 
                      [(or (> cond-x 1e2) (> cond-y 1e2))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list (or '-.f64 '-.f32) larg rarg)
                    #:when (or (is-inexact? larg) (is-inexact? rarg))
                    (define larg-val (hash-ref exacts-hash larg))
                    (define rarg-val (hash-ref exacts-hash rarg))
                    (define x-y (- larg-val rarg-val))
                    (define cond-x (abs (/ larg-val subexpr-val)))
                    (define cond-y (abs (/ rarg-val subexpr-val)))
                    
                    (cond
                      ; Condition number hallucination:
                      ; When x - y correctly underflows, CN is high
                      ; even though the answer is correct
                      [(and (= x-y 0.0) (underflow? subexpr)) #f]

                      ; nan rescue:
                      ; inf - inf = nan but should actually get an inf
                      [(and (infinite? larg-val)
                            (infinite? rarg-val)
                            (same-sign? larg-val rarg-val)
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]

                      ; inf rescue
                      ; If x or y overflow and the other arg rescues
                      ; it
                      [(and (or (infinite? larg-val)
                                (infinite? rarg-val))
                            (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]

                      ; High condition number:
                      ; CN(+, x, y) = |x / x - y|
                      [(or (> cond-x 1e2) (> cond-y 1e2))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   #|
                   TODO Make this actually work
                   |#
                   [(list (or 'sin.f64 'sin.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val  (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list (or 'cos.f64 'cos.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val  (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list (or 'tan.f64 'tan.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list (or 'sqrt.f64 'sqrt.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))

                    ; Under/overflow rescue:
                    (and (or (underflow? arg)
                             (overflow? arg))
                         (not (= subexpr-val arg-val))
                         (mark-erroneous! subexpr pt))]

                   [(list (or 'cbrt.f64 'cbrt.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    
                    ; Under/overflow rescue:
                    (and (or (underflow? arg)
                             (overflow? arg))
                         (not (= subexpr-val arg-val))
                         (mark-erroneous! subexpr pt))]
                   
                   [(list (or '/.f64 '/.f32) x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define x (hash-ref exacts-hash x-ex))
                    (define y (hash-ref exacts-hash y-ex))
                    (define x/y (/ x y))
                    (define y-oflow? (infinite? y-ex))
                    (define x-uflow? (underflow? x-ex))
                    (define x-oflow? (infinite? x-ex))
                    (define y-uflow? (underflow? y-ex))
                    
                    (cond
                      
                      ; Why am I doing this?
                      [(and (= x/y 0.0) (underflow? subexpr)) #f]
                      
                      ; Why am I doing this?
                      [(and (infinite? x/y) (overflow? subexpr)) #f]
                      
                      ; x underflows and y rescues it
                      [(and x-uflow? (not (= subexpr-val x)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; y underflows and x rescues it
                      [(and y-uflow? (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; x overflows and y rescues it
                      [(and x-oflow? (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; y overflows and x rescues it
                      [(and y-oflow? (not (= (abs subexpr-val) +0.0)))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]

                   [(list (or '*.f64 '*.f32) x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define y-oflow? (infinite? y-ex))
                    (define x-uflow? (underflow? x-ex))
                    (define x-oflow? (infinite? x-ex))
                    (define y-uflow? (underflow? y-ex))
                    (cond
                      ; 0 * inf ~ nan, but if they rescue each other,
                      ; then it should not be nan
                      [(and x-uflow?
                            y-oflow?
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; inf * 0 ~ nan, but if they rescue each other,
                      ; then it should not be nan
                      [(and x-oflow?
                            y-uflow?
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; x underflows and y is normal, then if x != 0,
                      ; then error
                      [(and x-uflow? (not (= (abs subexpr-val) 0.0)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; x overflows and y is normal, then if x != inf,
                      ; then error
                      [(and x-oflow? (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; x is normal and y underflows, then if x != 0,
                      ; then error
                      [(and y-uflow? (not (= (abs subexpr-val) 0.0)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; x is normal and y overflows, then if x != inf,
                      ; then error
                      [(and y-oflow? (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list (or 'log.f64 'log.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    (define cond-num (abs (/ 1 subexpr-val)))
                    (define arg-oflow? (overflow? arg))
                    (define arg-uflow? (underflow? arg))
                    (cond
                      ; Condition number hallucination:
                      ; Condition number is high when x = 1,
                      ; but x is exactly 1, so there is no error
                      [(and (= arg-val 1.0) (= subexpr-val 0.0)) #f]
                      
                      ; overflow rescue:
                      [arg-oflow? (mark-erroneous! subexpr pt)]
                      
                      ; underflow rescue:
                      [arg-uflow? (mark-erroneous! subexpr pt)]

                      ; High Condition Number:
                      ; CN(log, x) = |1 / log(x)|
                      [(> cond-num 1e2) (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list (or 'exp.f64 'exp.f32) arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    
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
                      [(> (abs arg-val) 1e2)
                       (mark-erroneous! subexpr pt)]
                      
                      [else #f])]

                   ; FIXME need to rework from scratch
                   [(list (or 'pow.f64 'pow.f32) x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define x (hash-ref exacts-hash x-ex))
                    (define y (hash-ref exacts-hash y-ex))
                    (define cond-x (abs y))
                    (define cond-y (abs (* y (log x))))

                    ; pow has a lot of problems
                    (cond
                      [(and (= x 1.0) (= subexpr-val 1.0)) #f]
                      [(and (= y 1.0) (= subexpr-val x)) #f]
                      [(and (= y 0.0) (= subexpr-val 0.0)) #f]
                      ; skip if both overflow
                      [(and (or (overflow? x-ex)
                                (underflow? x-ex))
                            (not (= subexpr-val x)))
                       (mark-erroneous! subexpr pt)]
                      ; CN_x(pow, x, y) = y
                      ; CN_y(pow, x, y) = ylog(x)
                      [(or (> cond-x 1e2) (> cond-y 1e2))
                       (mark-erroneous! subexpr pt)]
                      [else  #f])]
                   
                   [(list (or 'acos.f64 'acos.f32) x-ex)
                    #:when (is-inexact? x-ex)
                    (define x (hash-ref exacts-hash x-ex))
                    (define cond_x (abs (/ x
                                           (* (sqrt (- 1 (* x x)))
                                              subexpr-val))))
                    
                    (cond
                      ; Condition number hallucinations:
                      ; acos(1) == 0
                      [(and (= x 1.0) (= subexpr-val 0.0)) #f]

                      ; acos(-1) == pi
                      [(and (= x -1.0) (= subexpr-val pi)) #f]
                      
                      ; High Condition Number:
                      ; CN(acos, x) = |x / (√(1 - x^2)acos(x))|
                      [(> cond_x 100) (mark-erroneous! subexpr pt)]
                      [else #f])]

                   [(list (or 'asin.f64 'asin.f32) x-ex)
                    #:when (is-inexact? x-ex)
                    (define x (hash-ref exacts-hash x-ex))
                    (define cond_x (abs (/ x
                                           (* (sqrt (- 1 (* x x)))
                                              subexpr-val))))
                    
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
                      [(> cond_x 100) (mark-erroneous! subexpr pt)]
                      [else #f])]
                   [_ #f])))
      (hash-update! error-count-hash #f (lambda (x) (set-add x pt)))))
  error-count-hash)
