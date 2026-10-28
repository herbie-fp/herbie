#lang racket

(require "syntax.rkt"
         "platform.rkt"
         "block.rkt"
         "sugar.rkt"
         "types.rkt")

(module+ test
  (require rackunit
           "../config.rkt"
           "../syntax/load-platform.rkt")

  (activate-platform! (*platform-name*))
  (define f64 (get-representation 'binary64))

  (define sin-proc (impl-info 'sin.f64 'fl))
  (check-equal? (sin-proc 0.0) 0.0 "sin(0) = 0")

  (define sincos-proc (impl-info 'sincos.f64 'fl))
  (check-equal? (sincos-proc 0.0) #(0.0 1.0) "sincos(0) = [0, 1]")

  ; get-fpcore-impl
  (define (get-impl op props itypes)
    (get-fpcore-impl op props itypes))

  (check-equal? (get-impl '+ '((:precision . binary64)) (list f64 f64)) '+.f64)
  (check-equal? (get-impl '+ '((:precision . binary64) (:description . "test")) (list f64 f64))
                '+.f64)
  (check-equal? (get-impl 'sin '((:precision . binary64)) (list f64)) 'sin.f64)
  (check-equal? (get-impl 'sincos '((:precision . binary64)) (list f64)) 'sincos.f64)

  (define ctx (context '(x) f64 (list f64)))
  (check-equal? (fpcore->prog '(ref (sincos x) 0) ctx)
                '(ref.0.array<binary64:binary64> (sincos.f64 x)))
  (check-equal? (fpcore->prog '(ref (sincos x) 1) ctx)
                '(ref.1.array<binary64:binary64> (sincos.f64 x)))
  ; fpcore->spec
  (check-equal? (fpcore->spec '(log1p x)) '(log (+ 1 x)))
  (check-equal? (fpcore->spec '(hypot x y)) '(sqrt (+ (* x x) (* y y))))
  (check-equal? (fpcore->spec '(fma x y z)) '(+ (* x y) z))

  ; let RHSs use the incoming environment; let* RHSs use prior bindings.
  (check-equal? (fpcore->spec '(let ([a x]
                                     [b (+ a 1)])
                                 (* a b)))
                '(* x (+ a 1)))
  (check-equal? (fpcore->spec '(let* ([a x]
                                      [b (+ a 1)])
                                 (* a b)))
                '(* x (+ x 1)))
  (check-equal? (fpcore->spec '(let () (+ x 1))) '(+ x 1))
  (check-equal? (fpcore->spec '(let ([a x])
                                 (let ([a y]
                                       [b a])
                                   (+ a b))))
                '(+ y x))

  ; Direct implementation lowering preserves parallel and sequential scope.
  (define ctx-with-a (context '(a x) f64 (list f64 f64)))
  (define one (literal 1 'binary64))
  (check-equal? (fpcore->prog '(let ([a x]
                                     [b (+ a 1)])
                                 (+ a b))
                              ctx-with-a)
                (list '+.f64 'x (list '+.f64 'a one)))
  (check-equal? (fpcore->prog '(let* ([a x]
                                      [b (+ a 1)])
                                 (+ a b))
                              ctx)
                (list '+.f64 'x (list '+.f64 'x one)))

  ; Repeated uses share one node in the emitted block.
  (define-values (let-block let-root)
    (fpcore->spec-block '(let ([a (+ x y)]) (+ (* a a) (* a a)))
                        #:ctx (context '(x y) f64 (list f64 f64))))
  (check-equal? (block-length let-block) 5)
  (match (val-def let-root)
    [(list '+ left right) (check-equal? (val-idx left) (val-idx right))]
    [_ (fail "let lowering did not emit a binary addition")])

  ; A let-bound implementation is lowered once in its binding context.
  (define-values (rounded-block rounded-root)
    (fpcore->prog-block '(let ([a (+ x 1)]) (+ (! :precision binary32 a) (! :precision binary64 a)))
                        ctx))
  (match (val-def rounded-root)
    [(list '+.f64 left right)
     (check-equal? (val-idx left) (val-idx right))
     (match (val-def left)
       [(list '+.f64 x one)
        (check-equal? (val-def x) 'x)
        (check-equal? (val-def one) (literal 1 'binary64))]
       [_ (fail "let binding was not lowered in its original precision")])]
    [_ (fail "let-bound value was not shared across rounding annotations")])

  (void))
