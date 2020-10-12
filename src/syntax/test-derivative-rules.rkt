#lang racket

(module+ test (require rackunit)
              (require "rules.rkt" "../programs.rkt" "../core/simplify.rkt" "../interface.rkt"))

(module+ test
  (define derivatives
          `(((d a a) . 1) 
            ((d y x) . 0)
            ((d (+ 1 (* 2 x)) x) . 2)
            ((d (+ 1 (* y x)) x) . y)
            ((d (pow x 3) x) . (* 3 (pow x 2)))
            ((d (- (pow x 3) (* 7 (pow x 2))) x)
             . (- (* 3 (pow x 2)) (* 7 (* 2 x))))
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
            
            ((subst (d (d (/ (+ x (sin x)) (+ x (tan x))) x) x) x 0)
             . -1/2)
            ((subst (/ (* 2 (pow x 2)) (pow x 2)) x 0)
             . 2)
            ((subst (/ (* 2 (pow x 10)) (pow x 7)) x 0)
             . 0)

            ;; test consolidation of fractions
            ((/ 1 (/ 1 x))
             . x)
            ((subst (d (- (/ 1 x) (/ 1 (tan x))) x) x 0)
             . 1/3)

            ;; here we approximate the third derivative
            #;((subst (d (d (d (- (/ 1 x) (/ 1 (tan x))) x) x) x) x 0)
             . 18/23)
            
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

  #;(pretty-print derivatives-in-repr)

  #;(for ([rule (*differentiation-rules*)])
    (println (format "~a  ~a     ~a" (rule-name rule) (rule-input rule) (rule-output rule))))

  (for ([pair derivatives-in-repr])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))