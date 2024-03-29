#lang racket

(require "syntax/rules.rkt"
         "syntax/sugar.rkt"
         "syntax/syntax.rkt"
         "syntax/types.rkt"
         "core/egg-herbie.rkt"
         "core/matcher.rkt"
         "core/simplify.rkt"
         "core/taylor.rkt"
         "config.rkt"
         "accelerator.rkt"
         "alternative.rkt"
         "common.rkt"
         "conversions.rkt"
         "errors.rkt"
         "platform.rkt"
         "programs.rkt"
         "timeline.rkt")

(provide
  (contract-out
   [patch-table-has-expr? (-> expr? boolean?)]
   [patch-table-run (-> (listof expr?) (listof expr?) (listof alt?))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Patch table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resetter *patch-table*
  (λ () (make-hash))
  (λ () (make-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))] [exp-x (λ (x) `(exp ,x))] [log-x (λ (x) `(log ,x))]
	[ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity)
      (inf ,invert-x ,invert-x)
      (-inf ,ninvert-x ,ninvert-x)
      #;(exp ,exp-x ,log-x)
      #;(log ,log-x ,exp-x))))

(define (taylor-alt altn)
  (define expr (expand-accelerators (*rules*) (alt-expr altn)))
  (reap [sow]
    (for* ([var (free-variables expr)] [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
      (define genexpr (approximate expr var #:transform (cons f finv)))
      (for ([_ (in-range (*taylor-order-limit*))])
        (sow (alt (genexpr) `(taylor () ,name ,var) (list altn) '())))
      (timeline-stop!))))

(define (run-taylor altns)
  (reap [sow]
    (for ([altn (in-list altns)])
      (for ([approximation (taylor-alt altn)])
        (sow approximation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-literals expr)
  (match expr
    [(list op args ...) (cons op (map remove-literals args))]
    [(? literal?) (literal-value expr)]
    [_ expr]))

(define (run-rr altns)
  (define exprs (map alt-expr altns))
  (define real-exprs
    (reap [sow]
      (for ([expr (in-list exprs)])
        (match expr
          [(list 'if cond ift iff) (sow expr)]
          [(list op _ ...)
           (when (eq? (operator-info op 'otype) 'real)
             (sow expr))]
          [_ (sow expr)]))))

  ; rewrite using real rules
  (define real-rules (filter-not (compose representation? rule-otype) (*rules*)))
  (define changelistss (rewrite-expressions real-exprs (*context*) #:rules real-rules))

  ; apply changelists
  (define rewritten
    (reap [sow]
      (for ([changelists changelistss] [altn altns])
        (for ([cl changelists])
          (match-define (list subexpr input) cl)
          (sow (alt (remove-literals subexpr) (list 'rr '() input #f #f) (list altn) '()))))))

  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lowering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-lowering altns)
  (define exprs (map alt-expr altns))
  
  (list))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (patch-table-has-expr? expr)
  (hash-has-key? (*patch-table*) expr))

(define (patch-table-run hi-err-exprs lo-err-exprs)
  (define exprs (append hi-err-exprs lo-err-exprs)) ; who cares any more
  (define uncached
    (for/list ([expr (in-list exprs)] #:unless (patch-table-has-expr? expr))
      (alt (prog->spec expr) '(patch) '() '())))
  ;; Core
  (define approximations (run-taylor uncached))
  (define rewritten (run-rr uncached))
  (define new-specs (append approximations rewritten))
  (define progs (run-lowering new-specs))
  (printf "~a\n" progs)

  (list))
