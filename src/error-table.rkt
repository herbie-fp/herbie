#lang racket

(require racket/flonum racket/set)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "timeline.rkt"
         "common.rkt" "ground-truth.rkt")

(provide actual-errors predicted-errors)

;- Error Listing ---------------------------------------------------------------

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


#; (define (is-exact? expr) (compose list? not))
(define is-inexact? list?)

(define (very-close? a b)
  (define x (min a b))
  (define y (max a b))
  (cond
    [(and (fl< x 0.0) (fl> y 0.0))
     #f]
    [else
     (and (fl< (flabs a) (fl* (flabs b) 2.0))
          (fl> (flabs a) (fl/ (flabs b) 2.0)))]))
 
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
 
    (unless (ormap identity (for/list ([subexpr subexprs-list])
      (define subexpr-val (hash-ref exacts-hash subexpr))
 
      (cond
        ;; NOTE: need better way to see if a calculation underflowed
        [(fl= subexpr-val 0.0)
         (hash-set! uflow-hash subexpr #t)]
        [(fl= (flabs subexpr-val) +inf.0)
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
           ;; If R(x - y) underflows and R(x) - R(y) underflows, then skip
           [(and (= x+y 0.0) (underflow? subexpr)) #f]
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
           ;; If R(x - y) underflows and R(x) - R(y) underflows, then skip
           [(and (= x-y 0.0)
                 (underflow? subexpr))
            #f]
           [(or (> cond-x 1e2) (> cond-y 1e2))
            (mark-erroneous! subexpr pt)]
           [else #f])]
 
        #|
        TODO: We are not looking at the cases where for sin/cos/tan x
        x is very close to nPI or nPI + PI/2 or nPI/2
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
         (and (or (underflow? arg)
                  (overflow? arg))
              (not (= subexpr-val arg-val))
              (mark-erroneous! subexpr pt))]
        #|
        TODO: remaining cases for which rescuing underflow/overflow can occur
        a / b
        - a overflows and b is large
        |#
        [(list '/.f64 x-ex y-ex)
         #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
         (define x (hash-ref exacts-hash x-ex))
         (define y (hash-ref exacts-hash y-ex))
         (define x/y (/ x y))
         ;;(define x-oflow? (overflow? x-ex))
         (define y-oflow? (overflow? y-ex))
         (define x-uflow? (underflow? x-ex))
         (define y-uflow? (underflow? y-ex))

         (cond
           [(and (= x/y 0.0) (underflow? subexpr)) #f]
           [(and (= (abs x/y) +inf.0) (overflow? subexpr)) #f]
           [(and x-uflow? (not (= subexpr-val x)))
            (mark-erroneous! subexpr pt)]
           [(and y-uflow? (not (= (abs subexpr-val) +nan.0)))
            (mark-erroneous! subexpr pt)]
           [(and y-oflow? (not (= (abs subexpr-val) +0.0)))
            (mark-erroneous! subexpr pt)]
           [else #f])]
 
        [(list 'log.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (hash-ref exacts-hash arg))
         (define cond-num (abs (/ 1 (log arg-val))))
         (define arg-oflow? (overflow? arg))
         (define arg-uflow? (underflow? arg))
         (cond
           [arg-oflow? (mark-erroneous! subexpr)]
           [arg-uflow? (mark-erroneous! subexpr)]
           [(> cond-num 1e2) (mark-erroneous! subexpr pt)]
           #;[(very-close? arg-val 1.0) (mark-erroneous! subexpr pt)]
           [else #f])]

        [(list 'exp.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (hash-ref exacts-hash arg))
         (cond
           ;; if R(exp(x)) overflows and exp(R(x)) overflows then just skip
           [(and (overflow? subexpr) (= (exp arg-val) +inf.0))
            #f]
           [(> (exp arg-val) 1e2) (mark-erroneous! subexpr pt)]
           [else #f])]

        [(list 'pow.f64 x-ex y-ex)
         #:when (or (is-inexact? x-ex) (is-inexact? y-ex))
         (define x (hash-ref exacts-hash x-ex))
         (define y (hash-ref exacts-hash y-ex))
         (define x^y (expt x y))
         (define cond-x (abs y))
         (define cond-y (abs (* y (log x))))
         
         (cond
           [(and (= x 1.0) (= subexpr-val 1.0)) #f]
           [(and (overflow? subexpr) (= (abs x^y) +inf.0))
            #f]
           [(and (underflow? subexpr) (= (abs x^y) +0.0))
            #f]
           [(or (> cond-x 1e2) (> cond-y 1e2))
            (mark-erroneous! subexpr pt)]
           [else #f])]

        ;; TODO: Multiplication definitely can rescue oflow/uflow
        
        [_ #f])))
      (hash-update! error-count-hash #f (lambda (x) (set-add x pt)))))
  error-count-hash)
