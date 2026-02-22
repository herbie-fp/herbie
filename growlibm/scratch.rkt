#lang racket

(require
  "../src/api/sandbox.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/syntax/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/reports/common.rkt"
  "../src/utils/common.rkt")

(activate-platform! "no-accelerators")

;;; (define expr '(*.f64 (cos.f64 (*.f64 #s(literal -6.2831854820251465 binary64) z0)) (sqrt.f64 z1)))
;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; (displayln (cost-proc expr (get-representation 'binary64)))
(define (get-ctx expr)
  (define free-vars  (sort (free-variables expr) symbol<?))
  (context
   free-vars
   (get-representation 'binary64)
   (make-list (length free-vars)
              (get-representation 'binary64))))

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


;;; (define expr1 '(*.f64 z0 z0))
;;; (define expr2 '(/.f64 z0 z1))
;;; (define (get-ctx expr)
;;;   (define free-vars  (sort (free-variables expr) symbol<?))
;;;   (context
;;;    free-vars
;;;    (get-representation 'binary64)
;;;    (make-list (length free-vars)
;;;               (get-representation 'binary64))))

;;; (define exprs (list expr1 expr2))
;;; (define ctxs (map get-ctx exprs))
;;; (best-exprs exprs ctxs)

;;; (register-op! (*active-platform*) '(/ (+ (sqrt (* (+ z2 z1) (- z1 z2))) z1) z0) 'hehe 0)

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

(define (run-herbie-expr expr platform
                         #:seed [seed #f]
                         #:name [name "scratch"]
                         #:precision [precision (*default-precision*)])
  (parameterize ([*active-platform* platform])
    (define test (expr->test expr #:name name #:precision precision))
    (define result (run-herbie 'improve test #:seed seed))
    (disable-flag! 'generate 'taylor)
    (match (job-result-status result)
      ['success
       (define backend (job-result-backend result))
       (define end (improve-result-end backend))
       (define end-best (first end))
       (define final-error (errors-score (alt-analysis-errors end-best)))
       final-error]
      [_
       (raise-arguments-error 'run-herbie-expr "Herbie run failed" "expr" expr)])))

;;; (void (run-herbie-expr '(cbrt (/ (+ (sqrt (* (+ z2 z1) (- z1 z2))) z1) z0))))

;;; (define (register-op! platform fpcore name cost)
;;;   (parameterize ([*active-platform* platform])
;;;     (define impl (fpcore->prog fpcore (get-ctx fpcore)))
;;;     (define spec (prog->spec impl))
;;;     (define ctx (get-ctx spec))
;;;     (define vars (context-vars ctx))
;;;     (define name* (string->symbol name))

;;;     (define op-impl
;;;       (create-operator-impl!
;;;        name*
;;;        ctx
;;;        #:spec spec
;;;        #:impl (from-rival)
;;;        #:fpcore `(! :precision binary64 (,name* ,@vars))
;;;        #:cost cost))
;;;     (platform-register-implementation! platform op-impl)
;;;     (void)))

(define base-platform (platform-copy (*active-platform*)))

;;; (define (implies expr1 expr2)
;;;   (define platform-a (platform-copy base-platform))
;;;   (register-op! platform-a expr1 "expr1" 0)
;;;   (define before-err1 (run-herbie-expr expr2 base-platform))
;;;   (define after-err1 (run-herbie-expr expr2 platform-a))
  
;;;   (define platform-b (platform-copy base-platform))
;;;   (register-op! platform-b expr2 "expr2" 0)
;;;   (define before-err2 (run-herbie-expr expr1 base-platform))
;;;   (define after-err2 (run-herbie-expr expr1 platform-b))
  
;;;   (displayln (format "~a, before: ~a, after: ~a" expr2 before-err1 after-err1))
;;;   (displayln (format "~a, before: ~a, after: ~a" expr1 before-err2 after-err2)))

;;; (implies '(* 2 z0) '(z0 2))

(define expr1 '(if.f64 (or (>.f64 (fabs.f64 g) #s(literal 0 binary64)) (>.f64 (fabs.f64 (cos.f64 (+.f64 phi0 (/.f64 y esp)))) #s(literal 0 binary64))) (atan2.f64 g (cos.f64 (+.f64 phi0 (/.f64 y esp)))) #s(literal 0 binary64)))

(define (eliminate-ifs expr)
  (define comparison-bases
    '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64
            <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op) (member op comparison-bases))

  (define (pure-math? e)
    (let check ([e e])
      (match e
        [(or `(if.f32 ,_ ,_ ,_)
             `(if.f64 ,_ ,_ ,_)) #f]
        [(list (? comparison-op?) _ _) #f]
        [(list _ args ...)
         (andmap check args)]
        [_ #t])))

  (reap [sow]
        (let loop ([expr expr])
          (match expr
            [(or `(if ,test ,t ,f)
                 `(if.f32 ,test ,t ,f)
                 `(if.f64 ,test ,t ,f))
             (loop test) (loop t) (loop f)]

            [(list (? comparison-op?) lhs rhs)
             (loop lhs) (loop rhs)]

            [(list op args ...)
             (if (pure-math? expr)
                 (sow expr)
                 (for ([arg args])
                   (loop arg)))]

            [_ (void)]))))

(displayln (eliminate-ifs expr1))