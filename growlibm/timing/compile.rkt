#lang racket

(require "../../src/syntax/load-platform.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/reports/common.rkt")

(activate-platform! "growlibm")

(define test (read (open-input-string (vector-ref (current-command-line-arguments) 0))))

(displayln (core->c test "foo"))