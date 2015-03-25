#lang racket

(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")

(provide loop-errors loop-error-score*)

(define (loop-error-at-point loop-expr vars pt)
  (let loop ([errs '()] [loop-expr-fl loop-expr] [loop-expr-bf loop-expr])
    (let ([loop-expr-fl* (step-loop loop-expr-fl vars pt mode:fl)]
          [loop-expr-bf* (step-loop loop-expr-bf vars pt mode:bf)])
      (if (not (and loop-expr-fl* loop-expr-bf*)) (reverse errs)
          (loop (cons (let ([approx (take-cur-val loop-expr-fl* vars pt mode:fl)]
                            [exact (take-cur-val loop-expr-bf* vars pt mode:bf)])
                        (if (real? approx)
                            (add1 (abs (ulp-difference approx exact)))
                            (add1 (expt 2 (*bit-width*)))))
                      errs)
                loop-expr-fl*
                loop-expr-bf*)))))

(define (step-loop loop-expr vars pt mode)
  (define (eval expr)
    (if (eq? mode mode:bf)
        ((exact-eval expr vars) pt)
        ((eval-prog `(λ ,vars ,expr) mode) pt)))
  (match loop-expr
    [`(do-list ([,accs ,inits ,updates] ...)
               ([,items ,lsts] ...)
               ,ret-expr)
     (if (for/or ([lst lsts]) (null? (eval lst))) #f
         (let ([inits* (for/list ([acc accs] [init inits] [update updates])
                         (eval `(let ([,acc ,init])
                                  (let ,(for/list ([item items] [lst lsts])
                                          (list item (eval `(car ,lst))))
                                    ,update))))]
               [lsts* (for/list ([lst lsts])
                        `(cdr ,lst))])
           `(do-list ,(map list accs inits* updates)
                     ,(map list items lsts*)
                     ,ret-expr)))]))

(define (take-cur-val loop-expr vars pt mode)
  (define (eval expr)
    ((eval-prog `(λ ,vars ,expr) mode) pt))
  (match loop-expr
    [`(do-list ([,accs ,inits ,updates] ...)
               ([,items ,lsts] ...)
               ,ret-expr)
     (eval `(let ,(for/list ([acc accs] [init inits])
                    (list acc init))
              (let ,(for/list ([item items] [lst lsts])
                      (list item lst))
                ,ret-expr)))]))

(define (loop-errors loop-expr vars)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    (loop-error-at-point loop-expr vars p)))

(define (loop-error-score* loop-expr vars)
  (define (make-pt err-lst)
    (for/list ([(err i) (in-indexed err-lst)])
      (list i (sqr err))))
  (let ([err-lsts (loop-errors loop-expr vars)])
    (* (apply max (map length err-lsts))
       (best-fit-slope
        (apply
         append
         (map make-pt err-lsts))))))

(define-syntax-rule (for/avg ([items lsts]...)
                             body)
  (/ (for/sum ([items lsts] ...)
       body)
     (apply min (map length (list lsts ...)))))
    
(define (best-fit-slope pts)
  (/ (- (for/avg ([pt pts])
                 (* (car pt) (cadr pt)))
        (* (for/avg ([pt pts])
                    (car pt))
           (for/avg ([pt pts])
                    (cadr pt))))
     (- (for/avg ([pt pts])
                 (sqr (car pt)))
        (sqr (for/avg ([pt pts])
                      (car pt))))))
