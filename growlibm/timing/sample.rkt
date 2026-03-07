#lang racket

(require "../../src/core/points.rkt"
         "../../src/core/sampling.rkt"
         "../../src/syntax/batch.rkt"
         "../../src/syntax/load-platform.rkt"
         "../../src/syntax/platform.rkt"
         "../../src/syntax/sugar.rkt"
         "../../src/syntax/types.rkt"
         "../../src/utils/common.rkt")

(activate-platform! "growlibm")

(define args (current-command-line-arguments))
(define core (read (open-input-string (vector-ref args 0))))

(when (> (vector-length args) 1)
  (define seed (string->number (vector-ref args 1)))
  (when seed
    (set-seed! seed)))

(define (parse-fpcore core)
  (match core
    [(list 'FPCore _ (list vars ...) props ... body) (values vars props body)]
    [(list 'FPCore (list vars ...) props ... body) (values vars props body)]
    [_ (error 'sample.rkt "invalid FPCore: ~a" core)]))

(define (parse-context vars props)
  (define prop-dict (props->dict props))
  (define default-prec (dict-ref prop-dict ':precision (*default-precision*)))
  (define-values (var-names var-precs)
    (for/lists (var-names var-precs)
               ([var (in-list vars)])
      (match var
        [(list '! var-props ... name)
         (define var-prop-dict (props->dict var-props))
         (values name (dict-ref var-prop-dict ':precision default-prec))]
        [(? symbol? name) (values name default-prec)]
        [_ (error 'sample.rkt "invalid variable declaration: ~a" var)])))
  (define output-repr (get-representation default-prec))
  (define var-reprs (map get-representation var-precs))
  (values (context var-names output-repr var-reprs) prop-dict))

(define (fpcore-expr->spec expr ctx)
  (prog->spec (fpcore->prog expr ctx)))

(define (sample-core core)
  (define-values (vars props body) (parse-fpcore core))
  (define-values (ctx prop-dict) (parse-context vars props))
  (*context* ctx)

  (define specification (fpcore-expr->spec (dict-ref prop-dict ':spec body) ctx))
  (define precondition (fpcore-expr->spec (dict-ref prop-dict ':pre 'TRUE) ctx))
  (define-values (batch brfs) (progs->batch (list specification)))
  (define sample
    (parameterize ([*num-points* (+ (*num-points*) (*reeval-pts*))])
      (sample-points precondition batch brfs (list ctx))))
  (pcontext-points (apply mk-pcontext sample)))

(define points (sample-core core))

(for ([point (in-vector points)])
  (for ([value (in-vector point)])
    (display (format "~a " value)))
  (displayln ""))
