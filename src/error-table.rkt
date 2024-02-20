#lang racket

(require racket/set math/bigfloat racket/hash)
(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "common.rkt"
         "ground-truth.rkt" "syntax/sugar.rkt")

(provide actual-errors predicted-errors make-flow-table)

(define (constant? expr)
  (cond
    [(list? expr) (andmap constant? (rest expr))]
    [(symbol? expr) #f]
    [else #t]))

(define (actual-errors expr pcontext)
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr)
                                            (*context*))))))

  (define pt-worst-subexpr
    (append* (reap [sow]
                   (for ([pt-errors (in-list pt-errorss)]
                              [(pt _) (in-pcontext pcontext)])
                     (define sub-error (map cons subexprs pt-errors))
                     (define filtered-sub-error
                       (filter (lambda (p) (> (cdr p) 16)) sub-error))
                     (define mapped-sub-error
                       (map (lambda (p) (cons (car p) pt))
                            filtered-sub-error))
                      (unless (empty? mapped-sub-error)
                        (sow mapped-sub-error))))))
  
  (for/hash ([group (in-list (group-by car pt-worst-subexpr))])
    (let ([key (caar group)])
      (values key (map cdr group)))))

(define (same-sign? a b)
  (or (and (bfpositive? a) (bfpositive? b))
      (and (bfnegative? a) (bfnegative? b))))
 
(define (predicted-errors expr ctx pctx)
  (define cond-thres (bf 100))
  (define maybe-cond-thres (bf 32))
  
  (define subexprs
    (all-subexpressions expr (context-repr ctx)))
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
    (make-hash (map (lambda (x) (cons x '())) subexprs-list)))
  (define explanations-hash (make-hash))
  (define point-error-hash (make-hash))
  (define uflow-hash (make-hash))
  (define oflow-hash (make-hash)) 
  (define maybe-explanations-hash (make-hash))
  (define maybe-point-error-hash (make-hash))
  
  (for ([(pt _) (in-pcontext pctx)])
    (define (mark-erroneous! expr expl)
      (hash-update! error-count-hash expr (lambda (x) (set-add x pt)))
      (hash-update! explanations-hash (cons expr expl) (lambda (x) (+ 1 x)) 0)
      (hash-update! point-error-hash pt (lambda (x) (or true x)) #f))
    
    (define (mark-maybe! expr [expl 'sensitivity])
      (hash-update! maybe-explanations-hash
                    (cons expr expl) (lambda (x) (+ 1 x)) 0)
      (hash-update! maybe-point-error-hash pt (lambda (x) (or true x)) #f))
    
    (define exacts (apply subexprs-fn pt))
    (define exacts-hash
      (make-immutable-hash (map cons subexprs-list exacts)))
    (define (exacts-ref subexpr)
      (define exacts-val (hash-ref exacts-hash subexpr))
      ((representation-repr->bf
        (hash-ref repr-hash subexpr))
       exacts-val))
    
    (for/list ([subexpr (in-list subexprs-list)])
      (define subexpr-val (exacts-ref subexpr))

      (define (update-flow-hash flow-hash pred? . children)
        (define child-set (foldl
                           (lambda (a b)
                             (hash-union a b #:combine +))
                           (make-immutable-hash)
                           (map (lambda (a)
                                  (hash-ref flow-hash a (make-immutable-hash)))
                                children)))
        (define parent-set (hash-ref flow-hash subexpr (make-immutable-hash)))
        (define parent+child-set
          (hash-union parent-set child-set #:combine (lambda (_ v) v)))
        (define new-parent-set
          (if (pred? subexpr-val)
              (hash-update parent+child-set subexpr
                           (lambda (x) (+ x 1)) 0)
              parent+child-set))
        (hash-set! flow-hash subexpr new-parent-set))

      (match subexpr
        [(list _ x-ex y-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex y-ex)
         (update-flow-hash uflow-hash bfzero? x-ex y-ex)]
        [(list _ x-ex)
         (update-flow-hash oflow-hash bfinfinite? x-ex)
         (update-flow-hash uflow-hash bfzero? x-ex)]
        [_ #f])
      
      (match subexpr
        [(list (or '+.f64 '+.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define x+y (bigfloat->flonum (bf+ x y)))
         (define cond-x (bfabs (bf/ x
                                    subexpr-val)))
         (define cond-y (bfabs (bf/ y
                                    subexpr-val)))
         
         (cond
           ; Condition number hallucination
           ; Both R(x + y) and R(x) + R(y) underflow
           ; This causes the condition number to jump up,
           ; with no real error
           [(and (= x+y 0.0) (bfzero? subexpr-val)) #f]
           
           ; nan rescue:
           ; R(+-inf) + R(-+inf) = nan, but should actually
           ; be inf
           [(and (bfinfinite? x)
                 (bfinfinite? y)
                 (not (same-sign? x y))
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'nan-rescue)]
           
           ; inf rescue:
           ; R(inf) + y = non inf value (inf rescue)
           [(and (bfinfinite? x)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'oflow-left)]
           [(and (bfinfinite? y)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'oflow-right)]
           
           ; High condition number:
           ; CN(+, x, y) = |x / x + y|
           [(or (bf> cond-x cond-thres)
                (bf> cond-y cond-thres))
            (mark-erroneous! subexpr 'cancellation)]

           ; Maybe
           [(or (bf> cond-x maybe-cond-thres)
                (bf> cond-y maybe-cond-thres))
            (mark-maybe! subexpr 'cancellation)]
           [else #f])]
        
        [(list (or '-.f64 '-.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define x-y (bigfloat->flonum (bf- x y)))
         (define cond-x (bfabs (bf/ x
                                    subexpr-val)))
         (define cond-y (bfabs (bf/ y
                                    subexpr-val)))
         
         (cond
           ; Condition number hallucination:
           ; When x - y correctly underflows, CN is high
           ; even though the answer is correct
           [(and (= x-y 0.0) (bfzero? subexpr-val)) #f]

           ; nan rescue:
           ; inf - inf = nan but should actually get an inf
           [(and (bfinfinite? x)
                 (bfinfinite? y)
                 (same-sign? x y)
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'nan-rescue)]

           ; inf rescue
           ; If x or y overflow and the other arg rescues
           ; it
           [(and (bfinfinite? x)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'oflow-left)]
           [(and (bfinfinite? y)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'oflow-right)]

           ; High condition number:
           ; CN(+, x, y) = |x / x - y|
           [(or (bf> cond-x cond-thres)
                (bf> cond-y cond-thres))
            (mark-erroneous! subexpr 'cancellation)]

           ; Maybe
           [(or (bf> cond-x maybe-cond-thres)
                (bf> cond-y maybe-cond-thres))
            (mark-maybe! subexpr 'cancellation)] 
           [else #f])]

        #;[(list (or 'sin.f64 'sin.f32) (list (or '+.f64 '+.f32) x-ex y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define cot-x (bfcot x))
         (define cot-y (bfcot y))
         (define cot-x+y (bf/ (bf- (bf* cot-x cot-y) 1.bf)
                              (bf+ cot-x cot-y)))
         (define cond-x (bf* x cot-x+y))
         (define cond-y (bf* y cot-x+y))
         (when (or (bf> cond-x cond-thres)
                   (bf> cond-y cond-thres))
           (mark-erroneous! subexpr 'sensitivity))]
        
        [(list (or 'sin.f64 'sin.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cot-x (bfabs (bfcot x)))
         (define cond-no (bf* (bfabs x) cot-x))
         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x) cond-thres))
            (mark-erroneous! subexpr 'sensitivity)]
           [(and (bf> cond-no cond-thres)
                 (bf> cot-x cond-thres))
            (mark-erroneous! subexpr 'cancellation)]

           [(and (bf> cond-no maybe-cond-thres)
                 (bf> (bfabs x) maybe-cond-thres))
            (mark-maybe! subexpr 'sensitivity)]
           [(and (bf> cond-no maybe-cond-thres)
                 (bf> cot-x maybe-cond-thres))
            (mark-maybe! subexpr 'cancellation)]
           [else #f])]

        [(list (or 'cos.f64 'cos.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define tan-x (bfabs (bftan x)))
         (define cond-no (bf* (bfabs x) tan-x))

         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x) cond-thres))
            (mark-erroneous! subexpr 'sensitivity)]
           [(and (bf> cond-no cond-thres)
                 (bf> tan-x cond-thres))
            (mark-erroneous! subexpr 'cancellation)]

           [(and (bf> cond-no maybe-cond-thres)
                 (bf> (bfabs x) maybe-cond-thres))
            (mark-maybe! subexpr 'sensitivity)]
           [(and (bf> cond-no maybe-cond-thres)
                 (bf> tan-x maybe-cond-thres))
            (mark-maybe! subexpr 'cancellation)]
           [else #f])]

        [(list (or 'tan.f64 'tan.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define tot-x (bfabs (bf+ (bfcot x) (bftan x))))
         (define cond-no (bf* (bfabs x) tot-x))

         (cond
           [(and (bf> cond-no cond-thres)
                 (bf> (bfabs x) cond-thres))
            (mark-erroneous! subexpr 'sensitivity)]
           [(and (bf> cond-no cond-thres)
                 (bf> tot-x cond-thres))
            (mark-erroneous! subexpr 'cancellation)]

           [(and (bf> cond-no maybe-cond-thres)
                 (bf> (bfabs x) maybe-cond-thres))
            (mark-maybe! subexpr 'sensitivity)]
           [(and (bf> cond-no maybe-cond-thres)
                 (bf> tot-x maybe-cond-thres))
            (mark-maybe! subexpr 'cancellation)]
           [else #f])]
        
        [(list (or 'sqrt.f64 'sqrt.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         
         (cond
           ;; Underflow rescue:
           [(and (bfzero? x)
                 (not (bf= subexpr-val x)))
            (mark-erroneous! subexpr 'uflow-rescue)]

           ;; Overflow rescue:
           [(and (bfinfinite? x)
                 (not (bf= subexpr-val x)))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or 'cbrt.f64 'cbrt.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))

         (cond
           ;; Underflow rescue:
           [(and (bfzero? x)
                 (not (bf= subexpr-val x)))
            (mark-erroneous! subexpr 'uflow-rescue)]

           ;; Overflow rescue:
           [(and (bfinfinite? x)
                 (not (bf= subexpr-val x)))
            (mark-erroneous! subexpr 'oflow-rescue)])]

        [(list (or '/.f64 '/.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         
         (cond
           ;; if the numerator underflows and the denominator:
           ;; - underflows, nan could be rescued
           [(and (bfzero? x) (bfzero? y)
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'u/u)]
           ;; - is small enough, 0 underflow could be rescued
           [(and (bfzero? x)
                 (not (bfzero? subexpr-val)))
            (mark-erroneous! subexpr 'u/n)]
           ;; - overflows, no rescue is possible

           ;; if the numerator overflows and the denominator:
           ;; - overflows, nan could be rescued
           [(and (bfinfinite? x) (bfinfinite? y)
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'o/o)]
           ;; - is large enough, inf overflow can be rescued
           [(and (bfinfinite? x)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'o/n)]
           ;; - underflow, no rescue is possible

           ;; if the numerator is normal and the denominator:
           ;; - overflows, then a rescue is possible
           [(and (bfinfinite? y)
                 (not (bfzero? subexpr-val)))
            (mark-erroneous! subexpr 'n/o)]
           ;; - underflows, then a rescue is possible
           [(and (bfzero? y)
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'n/u)]
           ;; - is normal, then no rescue is possible
           [else #f])]

        [(list (or '*.f64 '*.f32) x-ex y-ex)
         #:when (or (list? x-ex) [list? y-ex])
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         
         (cond
           ;; if one operand underflows and the other overflows, then nan must
           ;; be rescued.
           [(and (bfinfinite? x)
                 (bfzero? y)
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'o*u)]
           [(and (bfzero? x)
                 (bfinfinite? y)
                 (not (bfnan? subexpr-val)))
            (mark-erroneous! subexpr 'u*o)]

           ;; If one operand is normal and the other overflows then, inf rescue
           ;; could occur
           [(and (or (bfinfinite? x)
                     (bfinfinite? y))
                 (not (bfinfinite? subexpr-val)))
            (mark-erroneous! subexpr 'n*o)]

           [(and (or (bfzero? x)
                     (bfzero? y))
                 (not (bfzero? subexpr-val)))
            (mark-erroneous! subexpr 'n*u)]

           ;; If both normal then no error
           [else #f])]
        
        [(list (or 'log.f64 'log.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cond-num (bfabs (bf/ 1.bf
                                      subexpr-val)))
         (cond
           ; Condition number hallucination:
           ; Condition number is high when x = 1,
           ; but x is exactly 1, so there is no error
           [(and (bf= x 1.bf) (bfzero? subexpr-val)) #f]
           
           ; overflow rescue:
           [(bfinfinite? x) (mark-erroneous! subexpr 'oflow-rescue)]
           
           ; underflow rescue:
           [(bfzero? x) (mark-erroneous! subexpr 'uflow-rescue)]

           ; High Condition Number:
           ; CN(log, x) = |1 / log(x)|
           [(bf> cond-num cond-thres)
            (mark-erroneous! subexpr 'sensitivity)]
           
           [(bf> cond-num maybe-cond-thres)
            (mark-maybe! subexpr 'sensitivity)]
           
           [else #f])]
        
        [(list (or 'exp.f64 'exp.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define exp-x (bigfloat->flonum (bfexp x)))
         
         (cond
           ; Condition Number Hallucination:
           ; When x is large enough that exp(x) overflows,
           ; condition number is also high.
           [(and (infinite? exp-x)
                 (bfinfinite? subexpr-val))
            #f]

           ; Condition Number Hallucination:
           ; When x is large enough (negative) that exp(x)
           ; underflows, condition number is also high
           [(and (zero? exp-x)
                 (bfzero? subexpr-val))
            #f]

           ; High Condition Number:
           ; CN(exp, x) = |x|
           [(bf> (bfabs x) cond-thres)
            (mark-erroneous! subexpr 'sensitivity)]

           [(bf> (bfabs x) maybe-cond-thres)
            (mark-maybe! subexpr 'sensitivity)]
           
           [else #f])]

        ; FIXME need to rework from scratch
        [(list (or 'pow.f64 'pow.f32) x-ex y-ex)
         #:when (or (list? x-ex) (list? y-ex))
         (define x (exacts-ref x-ex))
         (define y (exacts-ref y-ex))
         (define x^y (bigfloat->flonum (bfexpt x y)))
         (define cond-x (bfabs y))
         (define cond-y (bfabs (bf* y (bflog x))))

         (cond
           ;; Hallucination:
           ;; x has a large exponent and y is 1. The ylogx is large but there is
           ;; no error because the answer is exactly x
           [(and (bf= y 1.bf)
                 (bf= x subexpr-val)) #f]

           ;; Hallucination:
           ;; y is large but x is exactly 1
           [(and (= (bigfloat->flonum x) 1.0)
                 (= (bigfloat->flonum subexpr-val) 1.0))
            #f]

           ;; Hallucination:
           ;; y is large but x is zero
           [(and (bfzero? x)
                 (bfzero? subexpr-val))
            #f]
           
           ;; Hallucination:
           ;; if x is large enough that x^y overflows, the condition number also
           ;; is very large, but the answer correctly overflows
           [(and (bf> y 1.bf)
                 (infinite? x^y)
                 (bfinfinite? subexpr-val)) #f]

           ;; if x is small enough and y is large enough that x^y underflows,
           ;; the condition number also gets very large, but the answer
           ;; correctly underflows
           [(and (bf> y 1.bf)
                 (zero? x^y)
                 (bfzero? subexpr-val)) #f]

           [(and (bf< y -1.bf)
                 (zero? x^y)
                 (bfzero? subexpr-val)) #f]

           [(and (bf< y -1.bf)
                 (infinite? x^y)
                 (bfinfinite? subexpr-val)) #f]

           [(and (or (bfzero? x)
                     (bfinfinite? x))
                 (not (bf= subexpr-val x)))
            (mark-erroneous! subexpr 'rescue)]
           
           [(and (or (bf> cond-x cond-thres)
                     (bf> cond-y cond-thres))
                 (not (constant? y-ex)))
            (mark-erroneous! subexpr 'sensitivity)]
           
           [(and (or (bf> cond-x maybe-cond-thres)
                     (bf> cond-y maybe-cond-thres))
                 (not (constant? y-ex)))
            (mark-maybe! subexpr 'sensitivity)]
           
           [else #f])]
        
        [(list (or 'acos.f64 'acos.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cond-x (bfabs
                         (bf/ x
                              (bf* (bfsqrt
                                    (bf- 1.bf
                                         (bf* x x)))
                                   subexpr-val))))
         (define acos-x (bfacos x))
         #;(eprintf "~a,~a,~a,~a,"
                  pt
                  x
                  (bigfloat->flonum acos-x)
                  (bigfloat->flonum subexpr-val))
         (cond
           ; Condition number hallucinations:
           ; acos(1) == 0
           [(and (bf= x 1.bf) (bfzero? subexpr-val)) #f]

           ; acos(-1) == pi
           [(and (bf= x -1.bf) (bf= subexpr-val pi.bf)) #f]
           
           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)acos(x))|
           [(bf> cond-x cond-thres) 
            (mark-erroneous! subexpr 'sensitivity)]

           [(bf> cond-x maybe-cond-thres) 
            (mark-maybe! subexpr 'sensitivity)]
           
           [else #f])]

        [(list (or 'asin.f64 'asin.f32) x-ex)
         #:when (list? x-ex)
         (define x (exacts-ref x-ex))
         (define cond-x (bfabs
                         (bf/ x
                              (bf* (bfsqrt
                                    (bf- 1.bf
                                         (bf* x x)))
                                   subexpr-val))))
         
         (cond
           ; Condition Number hallucinations:
           ; asin(1) == pi/2
           [(and (bf= (bfabs x) 1.bf) (bf= (bfabs subexpr-val) (bf/ pi.bf 2.bf))) #f]
           [(and (bfzero? x) (bfzero? subexpr-val)) #f]
           ; High Condition Number:
           ; CN(acos, x) = |x / (√(1 - x^2)asin(x))|
           [(bf> cond-x cond-thres) (mark-erroneous! subexpr 'sensitivity)]

           [(bf> cond-x maybe-cond-thres) (mark-maybe! subexpr 'sensitivity)]
           
           [else #f])]
        [_ #f])))
  (values error-count-hash explanations-hash point-error-hash oflow-hash uflow-hash maybe-explanations-hash maybe-point-error-hash))

(define (flow-list flow-hash expr type)
  (for/list ([(k v) (in-dict (hash-ref flow-hash expr))])
    (list (~a k) type v)))

(define (make-flow-table oflow-hash uflow-hash expr expl)
  (match (list expl expr)
    [(list 'oflow-rescue _) (flow-list oflow-hash expr "overflow")]
    [(list 'uflow-rescue _) (flow-list uflow-hash expr "underflow")]
    [(list 'u/u (list _ num den))
     (append (flow-list uflow-hash num "underflow")
             (flow-list uflow-hash den "underflow"))]
    [(list 'u/n (list _ num _))
     (flow-list uflow-hash num "underflow")]
    [(list 'o/o (list _ num den))
     (append (flow-list oflow-hash num "overflow")
             (flow-list oflow-hash den "overflow"))]
    [(list 'o/n (list _ num _))
      (flow-list oflow-hash num "overflow")]
    [(list 'n/o (list _ _ den))
     (flow-list oflow-hash den "overflow")]
    [(list 'n/u (list _ _ den))
     (flow-list uflow-hash den "underflow")]
    [(list 'o*u (list _ left right))
     (append (flow-list oflow-hash left "overflow")
             (flow-list uflow-hash right "underflow"))]
    [(list 'u*o (list _ left right))
     (append (flow-list uflow-hash left "underflow")
             (flow-list oflow-hash right "overflow"))]
    [(list 'nan-rescue (list _ left right))
     (append (flow-list oflow-hash left "overflow")
             (flow-list oflow-hash right "overflow"))]
    [(list 'oflow-left (list left _))
     (flow-list oflow-hash left "overflow")]
    [(list 'oflow-right (list _ right))
     (flow-list oflow-hash right "overflow")]
    [_ '()]))
