#lang racket
(require
  "./src/api/sandbox.rkt"
  "./src/syntax/platform.rkt"
  "src/syntax/syntax.rkt"
  "src/syntax/types.rkt"
  "src/core/points.rkt"
  "src/core/programs.rkt"
  "src/utils/errors.rkt"
  "src/core/rules.rkt"
  "src/config.rkt"
  "src/core/batch.rkt"
  "src/core/egg-herbie.rkt"
  "src/core/sampling.rkt"
  "src/syntax/read.rkt"
  "src/syntax/sugar.rkt"
    "src/syntax/load-platform.rkt")

(activate-platform! "herbie20")

(define (get-error-fpcore fpcore) 
    (define test (car (load-port (open-input-string fpcore))))
    (define ctx (test-context test))
    (*num-points* 8000) 
    (*context* ctx)
    (define pcon (get-sample test))
    (define error (errors (test-spec test) pcon ctx))
    (define err-score (errors-score error))
    err-score)

(define (get-errors file-path)
  (with-input-from-file file-path
    (lambda ()
      (for ([line (in-lines)])
             (define split-line (regexp-split #px"," line))
             (define fpcore (car split-line))

          (define error (with-handlers ([exn? (λ (e) (printf "Skipping due to error: ~a\n" (exn-message e)))])

          (define error (get-error-fpcore fpcore))
          error))
        
        (printf "~a , ~a \n" (string-replace line "\n" "") error)
        ))))
(get-errors (vector-ref (current-command-line-arguments) 0))
