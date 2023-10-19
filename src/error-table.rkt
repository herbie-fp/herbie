#lang racket

(require racket/flonum)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "timeline.rkt"
         "common.rkt" "ground-truth.rkt")

(provide group-errors list-all-errors)

(define (group-errors expr pcontext)  
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr) (*context*))))))

  (define pt-worst-subexprs
    (for/list ([pt-errors (in-list pt-errorss)])
      (match-define (cons worst-subexpr pt-max-error)
        (argmax cdr (map cons subexprs pt-errors)))
      (if (> pt-max-error 2) worst-subexpr #f)))

  (for/hash ([group (in-list (group-by identity pt-worst-subexprs))])
    (values (first group) (length group))))

;- Error Listing ---------------------------------------------------------------
 
;; Using for debugging
(define (print-errors expr pcontext)
  (define (zip l1 l2) (map list l1 l2))
  (define points (for/list ([(pt _) (in-pcontext pcontext)]) pt))
  (define local-errors (car (compute-local-errors (list expr) (*context*))))
 
  (define max-per-point (make-hash))
  (define errpts-per-subexpr (make-hash))
 
  (for ([pt points])
    (hash-set! max-per-point pt (list 0 0)))
 
  (for ([(sub-expr errors) local-errors])
    (hash-set! errpts-per-subexpr sub-expr null)
    (for ([pair (zip points errors)])
      (define pt (car pair))
      (define err (cadr pair))
      (define old-error (hash-ref max-per-point pt))
      (if (> err (cadr old-error))
        (hash-set! max-per-point pt (list sub-expr err))
        null)))
 
  (hash-set! errpts-per-subexpr #f null)
 
  (for ([(k v) max-per-point])
    (define sub-expr (car v))
    (define err (cadr v))
    (if (> err 2)
      (hash-update! errpts-per-subexpr sub-expr (curry cons k))
      (hash-update! errpts-per-subexpr #f (curry cons k)))))
 
 
#|
NOTE: This is not the correct definition of exactness. We need to also
      include the notion of introduced error as well. Some time later
      will need to calculate subepxrs to some arb precision, to
      compare to the correctly rounded floating point value.
|#
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
 
(define (list-all-errors expr ctx pctx)
  ;; (print-errors expr pctx)
  
  (define subexprs
    (all-subexpressions-rev expr (context-repr ctx)))

  ;; map car subexprs
  (define subexprs-list
    (for/list ([subexpr (in-list subexprs)])
      (car subexpr)))
 
  (define ctx-list
    (for/list ([subexpr (in-list subexprs)])
      (struct-copy context ctx [repr (cdr subexpr)])))
 
  (define subexprs-fn (eval-progs-real subexprs-list ctx-list))
 
  (define error-count-hash
    (make-hash (map (lambda (x) (cons x 0)) (cons #f subexprs-list))))
 
  (define (mark-erroneous! expr)
    (hash-update! error-count-hash expr (lambda (x) (+ x 1))))
  
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
        TODO: Suffers pretty badly because of are bad definition of exactness
        |#
        [(list '+.f64 larg rarg)
         #:when (or (is-inexact? larg) (is-inexact? rarg))
         (define larg-val (hash-ref exacts-hash larg))
         (define rarg-val (hash-ref exacts-hash rarg))
         (cond
           [(fl= subexpr-val 0.0) #f #;(mark-erroneous! subexpr)]
           [(very-close? larg-val (fl* -1.0 rarg-val))
            (mark-erroneous! subexpr)]
           [else #f])]
         
        [(list '-.f64 larg rarg)
         #:when (or (is-inexact? larg) (is-inexact? rarg))
         (define larg-val (hash-ref exacts-hash larg))
         (define rarg-val (hash-ref exacts-hash rarg))
         (cond
           [(fl= subexpr-val 0.0) #f #;(mark-erroneous! subexpr)]
           [(very-close? larg-val rarg-val)
            (mark-erroneous! subexpr)]
           [else #f])]
 
        #|
        TODO: We are not looking at the cases where for sin/cos/tan x
        x is very close to nPI or nPI + PI/2 or nPI/2
        |#
        [(list 'sin.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val  (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr))]
 
        [(list 'cos.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val  (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr))]
 
        [(list 'tan.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (flabs (hash-ref exacts-hash arg)))
         (and (fl> arg-val 1e30) (mark-erroneous! subexpr))]
 
        #|
        TODO: refine understanding of when an overflow/underflow can be rescued
        surely, something thats larger than 1e600 cannot be rescued
        |#
        [(list 'sqrt.f64 arg)
         #:when (is-inexact? arg)
         (define arg-val (hash-ref exacts-hash arg))
         (and (or (hash-ref uflow-hash arg)
                  (hash-ref oflow-hash arg))
              (mark-erroneous! subexpr))]
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
         (cond
           [(and larg-uflow? (fl< rarg-val 1e-150)) (mark-erroneous! subexpr)]
           [_ #f])]
 
        #|
        TODO: log is very goo at rescuing underflow/overflows
        |#
        [(list 'log.f64 arg)
         #:when (is-inexact? arg)
         (define argval (hash-ref exacts-hash arg))
         (and (very-close? argval 1.0) (mark-erroneous! subexpr))]
 
        #|
        TODO:
        - need to figure out the exact error conditions for pow. Lot of things
        can happen. It can have high condition number, it can also rescue
        underflows and overflows. This gets even more complicated with a
        variable exponent
        - We know for a fact exp errors for high inputs. But high inputs for exp
        also overflow. At what limit does exp not overflow and still error?
        |#
        
        [_ #f])))
      (hash-update! error-count-hash #f (lambda (x) (+ x 1)))))
  error-count-hash)
