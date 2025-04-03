#lang racket

(require rackunit
         "egglog-herbie.rkt"
         "../syntax/syntax.rkt")

(define (test-e1->expr)
  (check-equal? (e1->expr '(Num (bigrat (from-string "3") (from-string "4")))) 3/4)

  (check-equal? (e1->expr '(Var "x")) 'x)

  (check-equal? (e1->expr '(If (Var "x")
                               (Num (bigrat (from-string "1") (from-string "2")))
                               (Num (bigrat (from-string "0") (from-string "5")))))
                '(if x 1/2 0))

  (check-equal? (e1->expr '(Add (Num (bigrat (from-string "1") (from-string "1")))
                                (Num (bigrat (from-string "2") (from-string "1")))))
                '(+ 1 2))

  (check-equal? (e1->expr '(Mul (Num (bigrat (from-string "2") (from-string "1")))
                                (Num (bigrat (from-string "3") (from-string "1")))))
                '(* 2 3))

  (check-equal?
   (e1->expr '(Add (Mul (Var "a") (Div (Num (bigrat (from-string "3") (from-string "2"))) (Var "b")))
                   (Sub (Num (bigrat (from-string "5") (from-string "6")))
                        (Mul (Var "c") (Num (bigrat (from-string "-1") (from-string "4")))))))
   '(+ (* a (/ 3/2 b)) (- 5/6 (* c -1/4))))

  (check-equal? (e1->expr '(Div (Mul (Sub (Var "x") (Var "y"))
                                     (Add (Num (bigrat (from-string "7") (from-string "5")))
                                          (Var "z")))
                                (Num (bigrat (from-string "2") (from-string "3")))))
                '(/ (* (- x y) (+ 7/5 z)) 2/3))

  (check-equal? (e1->expr '(Eq (Add (Mul (Var "p") (Num (bigrat (from-string "1") (from-string "2"))))
                                    (Div (Var "q")
                                         (Num (bigrat (from-string "3") (from-string "4")))))
                               (Num (bigrat (from-string "5") (from-string "6")))))
                '(== (+ (* p 1/2) (/ q 3/4)) 5/6))

  (check-equal? (e1->expr '(Gte (Div (Var "r") (Add (Var "s") (Var "t")))
                                (Sub (Num (bigrat (from-string "8") (from-string "9")))
                                     (Mul (Num (bigrat (from-string "1") (from-string "3")))
                                          (Var "u")))))
                '(>= (/ r (+ s t)) (- 8/9 (* 1/3 u)))))

(define (test-e2->expr)
  (check-equal? (e2->expr '(Num (bigrat (from-string "5") (from-string "6")))) 5/6)

  (check-equal? (e2->expr '(Var "y")) 'y)

  (check-equal? (e2->expr '(IfTy (Var "y")
                                 (Num (bigrat (from-string "1") (from-string "2")))
                                 (Num (bigrat (from-string "0") (from-string "1")))))
                '(if y 1/2 0))

  (check-equal? (e2->expr '(Mulf64Ty (Num (bigrat (from-string "2") (from-string "1")))
                                     (Num (bigrat (from-string "3") (from-string "1")))))
                '(*.f64 2 3))

  (check-equal? (e2->expr '(Approx (Add (Num (bigrat (from-string "1") (from-string "1")))
                                        (Num (bigrat (from-string "2") (from-string "1"))))
                                   (Mulf64Ty (Num (bigrat (from-string "3") (from-string "1")))
                                             (Num (bigrat (from-string "1") (from-string "1"))))))
                '#s(approx (+ 1 2) (*.f64 3 1)))

  ;; Complex 1
  (check-equal?
   (e2->expr
    '(Approx
      (Add (Sin (Add (Var "x") (Var "eps")))
           (Mul (Num (bigrat (from-string "-1") (from-string "1"))) (Sin (Var "x"))))
      (Fmaf64Ty
       (Fmaf64Ty (Sinf64Ty (Varbinary64 "x"))
                 (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                 (Mulf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                           (Cosf64Ty (Varbinary64 "x"))))
       (Mulf64Ty (Varbinary64 "eps") (Varbinary64 "eps"))
       (Mulf64Ty (Cosf64Ty (Varbinary64 "x")) (Varbinary64 "eps")))))
   '#s(approx (+ (sin (+ x eps)) (* -1 (sin x)))
              (fma.f64 (fma.f64 (sin.f64 x)
                                (fma.f64 (*.f64 eps 1/24) eps -1/2)
                                (*.f64 (*.f64 eps -1/6) (cos.f64 x)))
                       (*.f64 eps eps)
                       (*.f64 (cos.f64 x) eps))))

  ;; Complex 2
  (check-equal?
   (e2->expr
    '(Mulf64Ty
      (Fmaf64Ty
       (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                 (Sinf64Ty (Varbinary64 "x")))
       (Varbinary64 "eps")
       (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                     (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                           (Varbinary64 "eps")
                           (Numbinary64 (bigrat (from-string "1") (from-string "1"))))
                 (Cosf64Ty (Varbinary64 "x"))))
      (Varbinary64 "eps")))
   '(*.f64 (fma.f64 (*.f64 (fma.f64 (*.f64 eps 1/24) eps -1/2) (sin.f64 x))
                    eps
                    (*.f64 (fma.f64 (*.f64 eps -1/6) eps 1) (cos.f64 x)))
           eps))

  ;; Complex 3
  (check-equal?
   (e2->expr
    '(Fmaf64Ty
      (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                    (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                          (Varbinary64 "eps")
                          (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
                (Sinf64Ty (Varbinary64 "x")))
      (Varbinary64 "eps")
      (Mulf64Ty (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                    (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                          (Varbinary64 "eps")
                          (Numbinary64 (bigrat (from-string "1") (from-string "1"))))
                (Cosf64Ty (Varbinary64 "x")))))
   '(fma.f64 (*.f64 (fma.f64 (*.f64 eps 1/24) eps -1/2) (sin.f64 x))
             eps
             (*.f64 (fma.f64 (*.f64 eps -1/6) eps 1) (cos.f64 x))))

  ;; Complex 4
  (check-equal?
   (e2->expr
    '(Fmaf64Ty (Sinf64Ty (Varbinary64 "x"))
               (Fmaf64Ty (Mulf64Ty (Varbinary64 "eps")
                                   (Numbinary64 (bigrat (from-string "1") (from-string "24"))))
                         (Varbinary64 "eps")
                         (Numbinary64 (bigrat (from-string "-1") (from-string "2"))))
               (Mulf64Ty (Mulf64Ty (Varbinary64 "eps")
                                   (Numbinary64 (bigrat (from-string "-1") (from-string "6"))))
                         (Cosf64Ty (Varbinary64 "x")))))
   '(fma.f64 (sin.f64 x) (fma.f64 (*.f64 eps 1/24) eps -1/2) (*.f64 (*.f64 eps -1/6) (cos.f64 x))))

  (check-equal? (e2->expr '(Approx (Add (Sin (Add (Var "x") (Var "eps"))))
                                   (Mulf64Ty (Num (bigrat (from-string "-1") (from-string "1")))
                                             (Cosf64Ty (Var "x")))))
                '#s(approx (+ (sin (+ x eps))) (*.f64 -1 (cos.f64 x))))

  (check-equal? (e2->expr '(Mulf32Ty (Num (bigrat (from-string "3") (from-string "2")))
                                     (Addf32Ty (Num (bigrat (from-string "4") (from-string "5")))
                                               (Var "z"))))
                '(*.f32 3/2 (+.f32 4/5 z)))

  (check-equal? (e2->expr '(IfTy (Var "cond")
                                 (Num (bigrat (from-string "7") (from-string "8")))
                                 (Num (bigrat (from-string "-2") (from-string "3")))))
                '(if cond 7/8 -2/3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing API
;; Most calls should be done for testing purposes through these two methods
;;  - `populate-e->id-tables`: Used for testing e1-> expr and e2-> expr.
;;                             Populates the e1->id and e2->id tables
(define (populate-e->id-tables)
  (begin

    (for ([op (in-list (all-operators))])
      (hash-set! (e1->id) (serialize-op op) op))

    (for-each (λ (pair) (hash-set! (e2->id) (car pair) (cdr pair)))
              '((Acosf32Ty . acos.f32) (Acosf64Ty . acos.f64)
                                       (Acoshf32Ty . acosh.f32)
                                       (Acoshf64Ty . acosh.f64)
                                       (Addf32Ty . +.f32)
                                       (Addf64Ty . +.f64)
                                       (AndboolTy . and.bool)
                                       (Asinf32Ty . asin.f32)
                                       (Asinf64Ty . asin.f64)
                                       (Asinhf32Ty . asinh.f32)
                                       (Asinhf64Ty . asinh.f64)
                                       (Atan2f32Ty . atan2.f32)
                                       (Atan2f64Ty . atan2.f64)
                                       (Atanf32Ty . atan.f32)
                                       (Atanf64Ty . atan.f64)
                                       (Atanhf32Ty . atanh.f32)
                                       (Atanhf64Ty . atanh.f64)
                                       (Cbrtf32Ty . cbrt.f32)
                                       (Cbrtf64Ty . cbrt.f64)
                                       (Ceilf32Ty . ceil.f32)
                                       (Ceilf64Ty . ceil.f64)
                                       (Copysignf32Ty . copysign.f32)
                                       (Copysignf64Ty . copysign.f64)
                                       (Cosf32Ty . cos.f32)
                                       (Cosf64Ty . cos.f64)
                                       (Coshf32Ty . cosh.f32)
                                       (Coshf64Ty . cosh.f64)
                                       (Divf32Ty . /.f32)
                                       (Divf64Ty . /.f64)
                                       (Ef32Ty . E.f32)
                                       (Ef64Ty . E.f64)
                                       (Eqf32Ty . ==.f32)
                                       (Eqf64Ty . ==.f64)
                                       (Erfcf32Ty . erfc.f32)
                                       (Erfcf64Ty . erfc.f64)
                                       (Erff32Ty . erf.f32)
                                       (Erff64Ty . erf.f64)
                                       (Exp2f32Ty . exp2.f32)
                                       (Exp2f64Ty . exp2.f64)
                                       (Expf32Ty . exp.f32)
                                       (Expf64Ty . exp.f64)
                                       (Expm1f32Ty . expm1.f32)
                                       (Expm1f64Ty . expm1.f64)
                                       (Fabsf32Ty . fabs.f32)
                                       (Fabsf64Ty . fabs.f64)
                                       (FalseboolTy . FALSE.bool)
                                       (Fdimf32Ty . fdim.f32)
                                       (Fdimf64Ty . fdim.f64)
                                       (Floorf32Ty . floor.f32)
                                       (Floorf64Ty . floor.f64)
                                       (Fmaf32Ty . fma.f32)
                                       (Fmaf64Ty . fma.f64)
                                       (Fmaxf32Ty . fmax.f32)
                                       (Fmaxf64Ty . fmax.f64)
                                       (Fminf32Ty . fmin.f32)
                                       (Fminf64Ty . fmin.f64)
                                       (Fmodf32Ty . fmod.f32)
                                       (Fmodf64Ty . fmod.f64)
                                       (Gtef32Ty . >=.f32)
                                       (Gtef64Ty . >=.f64)
                                       (Gtf32Ty . >.f32)
                                       (Gtf64Ty . >.f64)
                                       (Hypotf32Ty . hypot.f32)
                                       (Hypotf64Ty . hypot.f64)
                                       (Infinityf32Ty . INFINITY.f32)
                                       (Infinityf64Ty . INFINITY.f64)
                                       (Lgammaf32Ty . lgamma.f32)
                                       (Lgammaf64Ty . lgamma.f64)
                                       (Log10f32Ty . log10.f32)
                                       (Log10f64Ty . log10.f64)
                                       (Log1pf32Ty . log1p.f32)
                                       (Log1pf64Ty . log1p.f64)
                                       (Log2f32Ty . log2.f32)
                                       (Log2f64Ty . log2.f64)
                                       (Logbf32Ty . logb.f32)
                                       (Logbf64Ty . logb.f64)
                                       (Logf32Ty . log.f32)
                                       (Logf64Ty . log.f64)
                                       (Ltef32Ty . <=.f32)
                                       (Ltef64Ty . <=.f64)
                                       (Ltf32Ty . <.f32)
                                       (Ltf64Ty . <.f64)
                                       (Mulf32Ty . *.f32)
                                       (Mulf64Ty . *.f64)
                                       (Nanf32Ty . NAN.f32)
                                       (Nanf64Ty . NAN.f64)
                                       (Negf32Ty . neg.f32)
                                       (Negf64Ty . neg.f64)
                                       (Neqf32Ty . !=.f32)
                                       (Neqf64Ty . !=.f64)
                                       (NotboolTy . not.bool)
                                       (OrboolTy . or.bool)
                                       (Pif32Ty . PI.f32)
                                       (Pif64Ty . PI.f64)
                                       (Powf32Ty . pow.f32)
                                       (Powf64Ty . pow.f64)
                                       (Remainderf32Ty . remainder.f32)
                                       (Remainderf64Ty . remainder.f64)
                                       (Rintf32Ty . rint.f32)
                                       (Rintf64Ty . rint.f64)
                                       (Roundf32Ty . round.f32)
                                       (Roundf64Ty . round.f64)
                                       (Sinf32Ty . sin.f32)
                                       (Sinf64Ty . sin.f64)
                                       (Sinhf32Ty . sinh.f32)
                                       (Sinhf64Ty . sinh.f64)
                                       (Sqrtf32Ty . sqrt.f32)
                                       (Sqrtf64Ty . sqrt.f64)
                                       (Subf32Ty . -.f32)
                                       (Subf64Ty . -.f64)
                                       (Tanf32Ty . tan.f32)
                                       (Tanf64Ty . tan.f64)
                                       (Tanhf32Ty . tanh.f32)
                                       (Tanhf64Ty . tanh.f64)
                                       (Tgammaf32Ty . tgamma.f32)
                                       (Tgammaf64Ty . tgamma.f64)
                                       (TrueboolTy . TRUE.bool)
                                       (Truncf32Ty . trunc.f32)
                                       (Truncf64Ty . trunc.f64)))))

; run-tests
(module+ test
  (require rackunit
           "../syntax/load-plugin.rkt"
           "egglog-herbie.rkt")

  (load-herbie-builtins)
  (populate-e->id-tables)
  (test-e1->expr)
  (test-e2->expr))

;; run-sample-egglog
(module+ test
  (require rackunit
           "egglog-herbie.rkt"
           "../syntax/load-plugin.rkt"
           "../syntax/types.rkt"
           "batch.rkt"
           "rules.rkt"
           "../config.rkt")

  (load-herbie-builtins)

  (define batch
    (progs->batch (list '(-.f64 (sqrt.f64 (+.f64 x #s(literal 1 binary64))) (sqrt.f64 x)))))

  (define roots (batch-roots batch))

  (*context* (make-debug-context '(x)))

  (define reprs (make-list (vector-length (batch-roots batch)) (context-repr (*context*))))

  (define rules (*rules*))
  (define schedule
    `((lift . ((iteration . 1) (scheduler . simple))) (,rules . ((node . ,(*node-limit*))))
                                                      (lower . ((iteration . 1) (scheduler .
                                                                                           simple)))))

  (run-egglog-multi-extractor (egglog-runner batch roots reprs schedule (*context*)) batch))
