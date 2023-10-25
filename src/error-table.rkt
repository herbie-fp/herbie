#lang racket

(require racket/flonum racket/set)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "timeline.rkt"
         "common.rkt" "ground-truth.rkt")

(provide actual-errors predicted-errors)

;- Error Listing ---------------------------------------------------------------

#|
NOTE: This is not the correct definition of exactness. We need to also
      include the notion of introduced error as well. Some time later
      will need to calculate subepxrs to some arb precision, to
      compare to the correctly rounded floating point value.
|#

(define (actual-errors expr pcontext)
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr) (*context*))))))
  
  (define pt-worst-subexprs
    (foldr append '() (for/list ([pt-errors (in-list pt-errorss)]
                                 [(pt _) (in-pcontext pcontext)])
                        (define sub-error (map cons subexprs pt-errors))
                        (define filtered-sub-error (filter (lambda (p) (> (cdr p) 2)) sub-error))
                        (define mapped-sub-error (map (lambda (p) (cons (car p) pt)) filtered-sub-error))
                        (if (empty? mapped-sub-error) (list (cons #f pt)) mapped-sub-error))))

  (for/hash ([group (in-list (group-by car pt-worst-subexprs))])
    (let ([key (caar group)])
      (values key (map cdr group)))))


(define (is-exact? expr) (compose list? not))
(define is-inexact? list?)
 
 
;; NOTE: Current implementation works, look at Python's math.isclose later
;; NOT SURE IF THIS IS CORRECT ANYMORE
;; For example: very-close? 0 0.0000001
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

  ;; map car subexprs
  (define subexprs-list (map car subexprs)
    #;(for/list ([subexpr (in-list subexprs)])
      (car subexpr)))
 
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
    (define oflow-hash (make-hash (map (lambda (x) (cons x #f)) subexprs-list)))
 
    (unless (ormap identity (for/list ([subexpr subexprs-list])
      (define subexpr-val (flabs (hash-ref exacts-hash subexpr)))
 
      (cond
        ;; NOTE: This is wrong, we are not differentiating between exact and
        ;;       inexact 0.0
        [(fl= subexpr-val 0.0)
         (hash-set! uflow-hash subexpr #t)]
        [(fl= subexpr-val +inf.0)
         (hash-set! oflow-hash subexpr #t)])
 
      (match subexpr
        #|
        TODO: -/+ suffers pretty badly because of are bad definition of exactness
        |#
        [(list '+.f64 larg rarg)
         #:when (or (is-inexact? larg) (is-inexact? rarg))
         (define larg-val (hash-ref exacts-hash larg))
         (define rarg-val (hash-ref exacts-hash rarg))
         (cond
           [(fl= subexpr-val 0.0) #f #;(mark-erroneous! subexpr)]
           [(very-close? larg-val (fl* -1.0 rarg-val))
            (mark-erroneous! subexpr pt)]
           [else #f])]
         
        [(list '-.f64 larg rarg)
         #:when (or (is-inexact? larg) (is-inexact? rarg))
         (define larg-val (hash-ref exacts-hash larg))
         (define rarg-val (hash-ref exacts-hash rarg))
         (cond
           [(fl= subexpr-val 0.0) #f #;(mark-erroneous! subexpr)]
           [(very-close? larg-val rarg-val)
            (mark-erroneous! subexpr pt)]
           [else #f])]
 
        #|
        TODO: We are not looking at the cases where for sin/cos/tan x
        x is very close to nPI or nPI + PI/2 or nPI/2
        |#
        [(list 'sin.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val  (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr pt))]
 
        [(list 'cos.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val  (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr pt))]
 
        [(list 'tan.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr pt))]
 
        #|
        TODO: refine understanding of when an overflow/underflow can be rescued
        surely, something thats larger than 1e600 cannot be rescued
        |#
        [(list 'sqrt.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (hash-ref exacts-hash arg))
         (and (or (hash-ref uflow-hash arg)
                  (hash-ref oflow-hash arg))
              (mark-erroneous! subexpr pt))]
        #|
        TODO: remaining cases for which rescuing underflow/overflow can occur
        a / b
        - a overflows and b is large
        - a is small and b underflows
        - a is large and b overflows
        |#
        [(list '/.f64 larg rarg)
         #:when (or (is-inexact? larg) (is-inexact? rarg))
         (define larg-val (flabs (hash-ref exacts-hash larg)))
         (define rarg-val (flabs (hash-ref exacts-hash rarg)))
         (define larg-uflow? (hash-ref uflow-hash larg))
         (define larg-oflow? (hash-ref oflow-hash larg))
         (define rarg-uflow? (hash-ref uflow-hash rarg))
         (define rarg-oflow? (hash-ref oflow-hash rarg))
         (cond
           [(and larg-uflow? (fl< rarg-val 1e-150)) (mark-erroneous! subexpr pt)]
           [(and (fl< larg-val 1e-150) rarg-uflow?) (mark-erroneous! subexpr pt)]
           [else #f])]
 
        #|
        TODO: log is very good at rescuing underflow/overflows
        |#
        [(list 'log.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (hash-ref exacts-hash arg))
         (define arg-oflow? (hash-ref oflow-hash arg))
         (define arg-uflow? (hash-ref uflow-hash arg))
         (cond
           [arg-oflow? (mark-erroneous! subexpr)]
           [arg-uflow? (mark-erroneous! subexpr)]
           [(very-close? arg-val 1.0) (mark-erroneous! subexpr pt)]
           [else #f])]

        #|
        TODO: I'm not sure of the limit
        |#
        [(list 'exp.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e308) (mark-erroneous! subexpr pt))]
        
        #|
        TODO:
        - Need to figure out the exact error conditions for pow. Lot of things
        can happen. It can have high condition number, it can also rescue
        underflows and overflows. This gets even more complicated with a
        variable exponent
        - We know for a fact exp errors for high inputs. But high inputs for exp
        also overflow. At what limit does exp not overflow and still error?
        - Multiplication definitely can rescue oflows/uflows
        |#
        
        [_ #f])))
      (hash-update! error-count-hash #f (lambda (x) (set-add x pt)))))
  error-count-hash)
