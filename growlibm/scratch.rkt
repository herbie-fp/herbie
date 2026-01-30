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


(define (best-exprs exprs ctxs)
  (define rules (*rules*))
  ;;;   (*context* (first ctxs))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch exprs))

  (define runner (make-egraph batch (map context-repr ctxs) schedule (second ctxs)))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  batchrefss)

(define expr1 '(*.f64 z0 z0))
(define expr2 '(/.f64 z0 z1))
(define (get-ctx expr)
  (define free-vars  (sort (free-variables expr) symbol<?))
  (context
   free-vars
   (get-representation 'binary64)
   (make-list (length free-vars)
              (get-representation 'binary64))))

(define exprs (list expr1 expr2))
(define ctxs (map get-ctx exprs))
;;; (best-exprs exprs ctxs)

(register-op '(sqrt (+ (* z0 z0) (* z1 z1))) 'hypoot)

;; Build a test from an expression and run Herbie on it.
(define (expr->test expr
                    #:name [name "scratch"]
                    #:precision [precision (*default-precision*)])
  (define vars (sort (free-variables expr) symbol<?))
  (define default-repr (get-representation precision))
  (define default-ctx
    (context vars default-repr (make-list (length vars) default-repr)))
  (define impl-expr
    (cond
      [(impl-prog? expr) expr]
      [(spec-prog? expr) (fpcore->prog expr default-ctx)]
      [else (raise-arguments-error 'expr->test "not a Herbie expression" "expr" expr)]))
  (define out-repr (repr-of impl-expr default-ctx))
  (define out-repr-name (representation-name out-repr))
  (define var-repr-names
    (for/list ([var (in-list vars)])
      (cons var out-repr-name)))
  (define spec impl-expr)
  (test name
        #f
        vars
        impl-expr
        '()
        #t
        spec
        '(TRUE)
        out-repr-name
        var-repr-names))

(define (run-herbie-expr expr
                         #:seed [seed #f]
                         #:name [name "scratch"]
                         #:precision [precision (*default-precision*)])
  (define test (expr->test expr #:name name #:precision precision))
  (define result (run-herbie 'improve test #:seed seed))
  (match (job-result-status result)
    ['success
     (define backend (job-result-backend result))
     (define end (improve-result-end backend))
     (define end-best (first end))
     (displayln end-best)
     (define final-error (errors-score (alt-analysis-errors end-best)))
     (printf "final-error-bits: ~a\n" final-error)]
    ['failure
     (define backend (job-result-backend result))
     (when (exn? backend)
       (printf "herbie-error: ~a\n" (exn-message backend)))]
    ['timeout (printf "herbie-timeout\n")])
  result)

(void (run-herbie-expr '(+ (sqrt (+ (* z0 z0) (* z1 z1))) z0)))
