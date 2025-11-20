#lang racket
(require "../src/core/egg-herbie.rkt"
         "../src/core/batch.rkt"
         "../src/syntax/types.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/platform.rkt"
         "../src/core/programs.rkt")

;; Activate default platform (likely binary64)
(activate-platform! "c")

;; The input expression from timeline
(module+ main
  (command-line
   #:args (input-str)
   (define input-expr (read (open-input-string input-str)))
   (define vars (free-variables input-expr))
   (define ctx (context vars <binary64> (map (const <binary64>) vars)))
   (define-values (batch brfs) (progs->batch (list input-expr)))
   (for ([iter (in-range 0 3)])
     (egraph-analyze-rewrite-impact batch brfs ctx iter))))
