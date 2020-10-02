#lang racket

(module+ test (require rackunit)
              (require "rules.rkt" "../programs.rkt" "../core/simplify.rkt" "../interface.rkt"))



(module+ test
  (define big `(*
     (+ x (tan x))
     (-
      (*
       (+ x (tan x))
       (-
        (*
         (+ x (sin x))
         (/
          (* (* (sin x) (cos x)) -2)
          (pow (cos x) 4)))
        (* (+ x (tan x)) (sin x))))
      (*
       (-
        (* (+ x (tan x)) (+ 1 (cos x)))
        (*
         (+ 1 (/ 1 (pow (cos x) 2)))
         (+ x (sin x))))
       (+ 2 (/ 2 (pow (cos x) 2)))))))
  (define small `(*
    (* (+ x (tan x)) (+ x (tan x)))
    (* (+ x (tan x)) (+ x (tan x)))))
  (define derivatives
          `(((d a a) . 1) 
            ((d y x) . 0)
            ((d (+ 1 (* 2 x)) x) . 2)
            ((d (+ 1 (* y x)) x) . y)
            ((d (pow x 3) x) . (* 3 (* x x)))
            ((d (- (pow x 3) (* 7 (pow x 2))) x)
             . (* x (+ (* 3 x) -14)))
            ((subst (+ a a) a 4) . 8)
            ((subst c a 4) . c)
            ((subst (subst (/ c d) c 4) d 5) . 4/5)
            ((subst (d (sin x) x) x PI) . -1)
            ((subst (d (- (pow x 3) (* 7 (pow x 2))) x) x 1)
             . -11)
            ((subst (d (log (/ (- x 1) (+ x 1))) x) x 0)
             . -2)
            ;; test limit functionality
            ((subst (/ (+ x 4) (+ a 2)) x 2)
             . (/ 6 (+ a 2)))
            ((lim (d (sin x) x) (d x x) 
                  (subst (d (sin x) x) x 0)
                  (subst (d x x) x 0)
                  x 0) . 1)
            ((subst (/ (sin x) x) x 0) . 1)
            ((subst (/ (+ x (sin x)) (+ x (tan x))) x 0)
             . 1)
            ((subst (d (/ (+ x (sin x)) (+ x (tan x))) x) x 0)
             . 0)
            ((subst (d (d (d (d (pow (+ x (tan x)) 4) x) x) x) x) x 0)
             . 384)
            #;((lim 
                (d ,big x)
                (d ,small x)
               0
               0
               x
               0)
              . -1/2)
            
            ((subst (d (d (/ (+ x (sin x)) (+ x (tan x))) x) x) x 0)
             . -1/2)
            ((subst (/ (* 2 (pow x 2)) (pow x 2)) x 0)
             . 2)
            ((lim (* 18 (pow x 10)) (pow x 4) 0 0 x 0)
             . 0)
            ))


  (define vars `(x y a b c d))
  (define var-reprs
    (apply hash
      (apply append
        (for/list ([v vars])
          (list v (*output-repr*))))))

  (define derivatives-in-repr
    (for/list ([pair derivatives])
      (cons
        (desugar-program (car pair) (*output-repr*) var-reprs #:full #f)
        (desugar-program (cdr pair) (*output-repr*) var-reprs #:full #f))))

  (pretty-print derivatives-in-repr)

  (for ([rule (*differentiation-rules*)])
    (println (format "~a     ~a" (rule-input rule) (rule-output rule))))

  (for ([pair derivatives-in-repr])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))