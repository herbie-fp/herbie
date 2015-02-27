#lang racket

(require "common.rkt")
(require "programs.rkt")

(provide add-error-term)

(define (add-error-term loop-expr)
  (match-let ([(list accs inits update-exprs ret-expr)
               (loop-common-parts loop-expr)])
    (filter
     identity
     (for/list ([(acc expr) (in-parallel accs update-exprs)])
       
       (define (add-et acc add)
         (let ([error-term (gensym "e")])
           (make-loop
            loop-expr
            (append accs (list error-term))
            (append inits (list 0.0))
            (append (for/list ([uexpr update-exprs])
                      (if (eq? uexpr expr)
                          `(+ ,acc (- ,add ,error-term))
                          uexpr))
                    ;; long live kahan
                    `((- (- (+ ,acc (- ,add ,error-term)) ,acc) (- ,add ,error-term))))
            ret-expr)))
       (match expr
         [`(+ ,a ,b)
          (cond [(eq? a acc) (add-et acc b)]
                [(eq? b acc) (add-et acc a)]
                [#t #f])]
         [_ #f])))))
