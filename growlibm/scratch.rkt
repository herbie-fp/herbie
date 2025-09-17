#lang racket

(require
  "../src/api/sandbox.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/reports/common.rkt"
  "../src/syntax/platform-language.rkt")

(activate-platform! "herbie10")

;;; (define expr '(*.f64 (cos.f64 (*.f64 #s(literal -6.2831854820251465 binary64) z0)) (sqrt.f64 z1)))
;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; (displayln (cost-proc expr (get-representation 'binary64)))



(define (get-simplified-expr ctx expr)
  (define rules (*rules*))
;;;   (displayln ctx)
  ;;; (displayln expr)
  ;;;   (*context* ctx)

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch (list expr)))

  (define runner (make-egraph batch (list (context-repr ctx)) schedule ctx))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  (debatchref (first (first batchrefss))))

  (define (get-simplified-exprs ctx exprs)
  (define rules (*rules*))
;;;   (displayln ctx)
  ;;; (displayln expr)
  ;;;   (*context* ctx)

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch exprs))

  (define runner (make-egraph batch (list (context-repr ctx)) schedule ctx))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  batchrefss)

(define (check-equal ctx expr1 expr2 expr3)
  (define rules (*rules*))
  ;;;   (displayln ctx)
  ;;; (displayln expr)
  ;;;   (*context* ctx)

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch (list expr1 expr2)))

  (define runner (make-egraph batch (list (context-repr ctx)) schedule ctx))
  ; batchrefss is a (listof (listof batchref))
  (define equal (egraph-equal? runner expr1 expr2))
  equal)

(define expr '(sin (* z0 PI)))
(define expr2 '(sin (* PI z0)))
(define expr3 '(+ z0 PI))
(define ctx (context
             (free-variables expr)
             (get-representation 'binary64)
             (make-list (length (free-variables expr))
                        (get-representation 'binary64))))

;;; (displayln (get-simplified-expr ctx expr))
;;; (displayln (get-simplified-expr ctx expr2))
(displayln  (debatchref (first (first (get-simplified-exprs ctx (list expr expr2 expr3))))))
;;; (displayln  (check-equal ctx expr expr2 expr3))