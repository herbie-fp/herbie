#lang racket

(require racket/flonum racket/set)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "common.rkt"
         "ground-truth.rkt")

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
    (make-hash (map (lambda (x) (cons x '())) (cons #f subexprs-list))))
 
  (define (mark-erroneous! expr pt)
    (hash-update! error-count-hash expr (lambda (x) (set-add x pt))))
  
  (for ([(pt _) (in-pcontext pctx)])
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons subexprs-list exacts)))
 
    (define uflow-hash (make-hash (map (lambda (x) (cons x #f)) subexprs-list)))
    (define (underflow? subexpr) (hash-ref uflow-hash subexpr))
    (define oflow-hash (make-hash (map (lambda (x) (cons x #f)) subexprs-list)))
    (define (overflow? subexpr) (hash-ref oflow-hash subexpr))
 
    (unless
        (ormap identity
               (for/list ([subexpr subexprs-list])
                 (define subexpr-val (hash-ref exacts-hash subexpr))

                 (cond
                   ;; NOTE need better way to see if a calculation underflowed
                   [(fl= subexpr-val 0.0)
                    (hash-set! uflow-hash subexpr #t)]
                   [(infinite? subexpr-val)
                    (hash-set! oflow-hash subexpr #t)])
                 
                 (match subexpr
                   [(list '+.f64 larg rarg)
                    #:when (or (is-inexact? larg) (is-inexact? rarg))
                    (define larg-val (hash-ref exacts-hash larg))
                    (define rarg-val (hash-ref exacts-hash rarg))
                    (define x+y (+ larg-val rarg-val))
                    (define cond-x (abs (/ larg-val x+y)))
                    (define cond-y (abs (/ rarg-val x+y)))
                    
                    (cond
                      ; When both underflow
                      [(and (= x+y 0.0) (underflow? subexpr)) #f]

                      ; inf + inf should give an inf not a nan when they have
                      ; different signs
                      [(and (infinite? larg-val)
                            (infinite? rarg-val)
                            (not (same-sign? larg-val rarg-val))
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]

                      ; if either arg is inf, and the answer is not, then inf was
                      ; rescued
                      [(and (or (infinite? larg-val)
                                (infinite? rarg-val))
                            (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      
                      ; CN(+, x, y) = x / x + y 
                      [(or (> cond-x 1e2) (> cond-y 1e2))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list '-.f64 larg rarg)
                    #:when (or (is-inexact? larg) (is-inexact? rarg))
                    (define larg-val (hash-ref exacts-hash larg))
                    (define rarg-val (hash-ref exacts-hash rarg))
                    (define x-y (- larg-val rarg-val))
                    (define cond-x (abs (/ larg-val x-y)))
                    (define cond-y (abs (/ rarg-val x-y)))
                    
                    (cond
                      ; Both underflow (I forgot why I am doing this ;-;)
                      [(and (= x-y 0.0) (underflow? subexpr)) #f]

                      ; inf - inf or -inf - -inf should give an inf not a nan
                      [(and (infinite? larg-val)
                            (infinite? rarg-val)
                            (same-sign? larg-val rarg-val)
                            (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]

                      ; If x or y overflow and the other arg rescues it
                      [(and (or (infinite? larg-val)
                                (infinite? rarg-val))
                            (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
		      
                      ; CN(+, x, y) = x / x - y
                      [(or (> cond-x 1e2) (> cond-y 1e2))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   #|
                   TODO Make this actually work
                   |#
                   [(list 'sin.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val  (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list 'cos.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val  (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list 'tan.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val (flabs (hash-ref exacts-hash arg)))
                    (and (> arg-val 1e30) (mark-erroneous! subexpr pt))]
                   
                   [(list 'sqrt.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    ; Check if over/underflow is rescued
                    (and (or (underflow? arg)
                             (overflow? arg))
                         (not (= subexpr-val arg-val))
                         (mark-erroneous! subexpr pt))]

                   [(list 'cbrt.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    ; see sqrt
                    (and (or (underflow? arg)
                             (overflow? arg))
                         (not (= subexpr-val arg-val))
                         (mark-erroneous! subexpr pt))]
                   
                   [(list '/.f64 x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define x (hash-ref exacts-hash x-ex))
                    (define y (hash-ref exacts-hash y-ex))
                    (define x/y (/ x y))
                    (define y-oflow? (overflow? y-ex))
                    (define x-uflow? (underflow? x-ex))
                    (define x-oflow? (overflow? x-ex))
                    (define y-uflow? (underflow? y-ex))
                    
                    (cond
                      ; both underflow
                      [(and (= x/y 0.0) (underflow? subexpr)) #f]
                      ; both overflow
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

                   [(list '*.f64 x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define y-oflow? (overflow? y-ex))
                    (define x-uflow? (underflow? x-ex))
                    (define x-oflow? (overflow? x-ex))
                    (define y-uflow? (underflow? y-ex))
                    (cond
                      ; 0 * inf ~ nan, but if they rescue each other, them it should not be nan
                      [(and x-uflow? y-oflow? (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      ; inf * 0 ~ nan, but if they rescue each other, then it should not be nan
                      [(and x-oflow? y-uflow? (not (nan? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      ; x underflows and y is normal, then if x != 0, then error
                      [(and x-uflow? (not (= (abs subexpr-val) 0.0)))
                       (mark-erroneous! subexpr pt)]
                      ; x overflows and y is normal, then if x != inf, then error
                      [(and x-oflow? (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      ; x is normal and y underflows, then if x != 0, then error
                      [(and y-uflow? (not (= (abs subexpr-val) 0.0)))
                       (mark-erroneous! subexpr pt)]
                      ; x is normal and y overflows, then if x != inf, then error
                      [(and y-oflow? (not (infinite? subexpr-val)))
                       (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list 'log.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    (define cond-num (abs (/ 1 subexpr-val)))
                    (define arg-oflow? (overflow? arg))
                    (define arg-uflow? (underflow? arg))
                    (cond
                      [(and (= arg-val 1.0) (= subexpr-val 0.0)) #f]
                      ; overflow rescue
                      [arg-oflow? (mark-erroneous! subexpr pt)]
                      ; underflow rescue
                      [arg-uflow? (mark-erroneous! subexpr pt)]
                      ; CN(log, x, y) = 1 / log(x)
                      [(> cond-num 1e2) (mark-erroneous! subexpr pt)]
                      [else #f])]
                   
                   [(list 'exp.f64 arg)
                    #:when (is-inexact? arg)
                    (define arg-val (hash-ref exacts-hash arg))
                    (cond
                      ; if both overflow skip
                      [(and (overflow? subexpr) (infinite? (exp arg-val)))
                       #f]
                      ; condition number
                      [(> arg-val 1e2) (mark-erroneous! subexpr pt)]
                      [else #f])]

                   ; FIXME need to rework from scratch
                   [(list 'pow.f64 x-ex y-ex)
                    #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
                    (define x (hash-ref exacts-hash x-ex))
                    (define y (hash-ref exacts-hash y-ex))
                    (define x^y (expt x y))
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
                   [_ #f])))
      (hash-update! error-count-hash #f (lambda (x) (set-add x pt)))))
  error-count-hash)
