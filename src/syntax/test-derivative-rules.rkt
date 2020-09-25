#lang racket

(module+ test (require rackunit)
              (require "rules.rkt" "../programs.rkt" "../core/simplify.rkt" "../interface.rkt"))



(module+ test
  (define derivatives
          `(((d a a) . 1)
            ((d y x) . 0)
            ((d (+ 1 (* 2 x)) x) . 2)
            ((d (+ 1 (* y x)) x) . y)
            ((d (pow x 3) x) . (* x (* x 3)))
            ((d (- (pow x 3) (* 7 (pow x 2))) x)
             . (* x (+ (* x 3) -14)))
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
            ;; This next one doesn't work- the reason is the derivative of pow involves a constant division by zero
            ;; The division by zero could be eliminated by simplify, but the simplify quits early due to an unsound union
            #;((subst (/ (* 2 (pow x 2)) (pow x 2)) x 0)
             . 2)
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

  #;(for ([rule (*differentiation-rules*)])
    (println (format "~a     ~a" (rule-input rule) (rule-output rule))))

  (for ([pair derivatives-in-repr])
       (check-equal? (differentiate-expr (car pair)) (cdr pair))))