#lang racket

(require "programs.rkt")
(require "common.rkt")
(require "alternative.rkt")

(provide factor-common-subexprs factor-common-subexprs*)

(struct cse-info (expr count-table))
(define (factor-common-subexprs altn)
  (alt-event `(λ ,(program-variables (alt-program altn))
                ,(factor-common-subexprs* (program-body (alt-program altn))))
             'factored-common-subexprs
             (list altn)))
(define (factor-common-subexprs* expr)
  (let ([ci
         (expression-induct
          expr
          #:constant (λ (c) (cse-info c (hash)))
          #:variable (λ (v) (cse-info v (hash)))
          #:primitive (λ (expr) (let ([uexpr (cons (car expr) (map cse-info-expr (cdr expr)))])
                                  (cse-info uexpr
                                            (hash-set (apply merge-counts*
                                                             (map cse-info-count-table (cdr expr)))
                                                      uexpr 1))))
          #:predicate (λ (expr) (cse-info (cons (car expr) (map cse-info-expr (cdr expr))) (hash)))
          #:let (λ (lexpr)
                  (match-let ([`(let ([,vars ,vals] ...) ,body) lexpr])
                    (let ([uexpr `(let ,(map list vars (map cse-info-expr vals))
                                    ,(cse-info-expr body))])
                      (cse-info uexpr (hash-set (apply merge-counts* (cse-info-count-table body)
                                                       (map cse-info-count-table vals))
                                                uexpr 1)))))
          #:loop (λ (loop)
                   (match loop
                     [`(do ([,accs ,inits ,updaters] ...)
                           ,cond
                         ,ret-expr)
                      (let ([uexpr
                             `(do ,(map list accs (map cse-info-expr inits)
                                        (map (compose factor-common-subexprs* cse-info-expr)
                                             updaters))
                                  ,(factor-common-subexprs* (cse-info-expr cond))
                                ,(factor-common-subexprs* (cse-info-expr ret-expr)))])
                        (cse-info uexpr (hash-set (apply merge-counts* (map cse-info-count-table inits))
                                                  uexpr 1)))]
                     [`(do-list ([,accs ,inits ,updaters] ...)
                                ([,items ,lsts] ...)
                                ,ret-expr)
                      (let ([uexpr
                             `(do-list ,(map list accs (map cse-info-expr inits)
                                             (map (compose factor-common-subexprs* cse-info-expr)
                                                  updaters))
                                       ,(map list items (map cse-info-expr lsts))
                                       ,(factor-common-subexprs* (cse-info-expr ret-expr)))])
                        (cse-info uexpr (hash-set (apply merge-counts*
                                                         (append (map cse-info-count-table inits)
                                                                 (map cse-info-count-table lsts)))
                                                  uexpr 1)))])))])
    (match-let ([(list best-count best-subexpr)
                 (max-score-expr (cse-info-count-table ci))])
      (if (or (not best-subexpr)
              ((* best-count (expr-size best-subexpr)) . < .
               (*common-subexpr-factoring-limit*)))
          (cse-info-expr ci)
          (factor-common-subexprs* (factor-subexpr (cse-info-expr ci) best-subexpr))))))

(define (max-score-expr ct)
  (call-with-values
      (λ ()
        (for/fold ([best-count 0] [best-expr #f])
            ([(expr count) (in-hash ct)])
          (if (and (count . > . 1)
                   (or (not best-expr)
                       ((* (expr-size expr) count) . > . (* (expr-size best-expr) best-count))))
              (values count expr)
              (values best-count best-expr))))
    list))
(define (merge-counts t1 t2)
  (for/fold ([res t1])
      ([(expr count) (in-hash t2)])
    (hash-update res expr (curry + count) 0)))
(define (merge-counts* . ts)
  (for/fold ([res (car ts)])
      ([other (cdr ts)])
    (merge-counts res other)))
(define (factor-subexpr expr subexpr)
  (let ([name (gensym 'expr)])
    (define (replace-if-equal expr)
      (if (equal? subexpr expr)
          name expr))
    `(let ([,name ,subexpr])
       ,(expression-induct
         expr
         #:constant replace-if-equal
         #:variable replace-if-equal
         #:primitive replace-if-equal
         #:predicate replace-if-equal
         #:let replace-if-equal
         #:loop replace-if-equal))))
