#lang racket

(require rackunit)
(require "../syntax/rules.rkt")
(require "../common.rtk")
(require "../programs.rkt")
(require "../points.rkt")

(provide (all-defined-out))

(define rules-tests
  (test-suite "Test rewrite rules for soundness"
   (for ([rule (*rules*)])
     (let ([name (rule-name rule)] [p1 (rule-input rule)] [p2 (rule-output rule)])
       (test-case (~a (rule-name rule))
         (let*-values ([(fv) (free-variables p1)]
                       [(pts exs1) (prepare-points `(λ ,fv ,p1)
                                                   (for/list ([v fv]) `(,v . ,sample-default)))]
                       [(exs2) (make-exacts `(λ ,fv ,p2) pts)])
           (for ([pt pts] [ex1 exs1] [ex2 exs2])
             (with-check-info (['point pt] ['prog1 p1] ['prog2 p2])
               (when (and (ordinary-float? ex1) (ordinary-float? ex2))
                 (check-= ex1 ex2 0))))))))))

