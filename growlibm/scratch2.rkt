#lang racket

(require racket/list
         "../src/api/sandbox.rkt"
         "../src/core/points.rkt"
         "../src/core/rules.rkt"
         "../src/config.rkt"
         "../src/core/batch.rkt"
         "../src/core/egg-herbie.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/sugar.rkt"
         "../src/core/programs.rkt"
         "../src/syntax/syntax.rkt"
         "../src/utils/common.rkt")

(activate-platform! "no-accelerators")


(define (all-subexpressions* expr)
  (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (and (symbol? op)
         (member op comparison-bases)))
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (match expr
              [(or `(if ,test ,t ,f)
                   `(if.f32 ,test ,t ,f)
                   `(if.f64 ,test ,t ,f))
               (loop test)
               (loop t)
               (loop f)]
              [(approx _ impl)
               (loop impl)]
              [(list (? comparison-op?) lhs rhs)
               (loop lhs)
               (loop rhs)]
              [_
               (sow expr)
               (match expr
                 [(? number?) (void)]
                 [(? literal?) (void)]
                 [(? symbol?) (void)]
                 [(list _ args ...)
                  (for ([arg args])
                    (loop arg))]
                 [_ (void)])]))))
  (remove-duplicates subexprs))

(define (remove-approxes expr)
  (match expr
    [(approx _ impl) (remove-approxes impl)]
    [(list op args ...) (cons op (map remove-approxes args))]
    [_ expr]))

(map displayln (all-subexpressions* '(if.f64 (<=.f64 (*.f64 v sinrot) #s(literal -99999999999999996863366107917975552 binary64)) (-.f64 #s(approx (* (- u u0) cosrot) (*.f64 cosrot u)) (*.f64 v sinrot)) (if.f64 (<=.f64 (*.f64 v sinrot) #s(literal 199999999999999995497619646912068059136 binary64)) #s(approx (- (* (- u u0) cosrot) (* v sinrot)) (*.f64 cosrot (-.f64 u u0))) (-.f64 #s(approx (* (- u u0) cosrot) (*.f64 cosrot u)) (*.f64 v sinrot))))))
