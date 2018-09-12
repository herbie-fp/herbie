#lang racket

(require "mainloop.rkt" "core/alt-table.rkt" "alternative.rkt" "programs.rkt" "core/simplify.rkt"
         "syntax/rules.rkt" "common.rkt" "points.rkt")

(run-improve '(λ (x) (-.p16 (sqrt.p16 (+.p16 x (real->posit16 1.0))) (sqrt.p16 x)))
             4
             #:precondition 'TRUE
             #:precision 'posit16)
