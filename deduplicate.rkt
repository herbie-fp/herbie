#lang racket

(require
  "./src/syntax/load-plugin.rkt"
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
  "src/syntax/sugar.rkt")

(load-herbie-builtins)
(load-herbie-plugins)
(activate-platform! 'default)

;;; (define (extract-vars expr)
;;;   (remove-duplicates
;;;    (let loop ([expr expr])
;;;      (match expr
;;;        [(? symbol? var) (list var)] 
;;;        [(list _ args ...) (append-map loop args)]
;;;        [_ '()]))))

;;; (define (get-float-type expr)
;;;   (cond
;;;     [(and (pair? expr) (symbol? (car expr)) (regexp-match? #rx".f64$" (symbol->string (car expr))))
;;;      'binary64]

;;;     [(and (pair? expr) (symbol? (car expr)) (regexp-match? #rx".f32$" (symbol->string (car expr))))
;;;      'binary32]

;;;     [(pair? expr)
;;;      (let ([subtypes (map get-float-type (cdr expr))])
;;;        (if (member 'binary64 subtypes) 'binary64 'binary32))]

;;;     [else 'binary32]))

;;; (define (get-float-type expr)
;;;     (if (regexp-match? #rx".f64$" (symbol->string (car expr))) 'binary64 'binary32))

(define cost-proc
  (platform-cost-proc (*active-platform*)))


;;; (define (get-ctx prog)
;;;     (define float-type (get-float-type prog)) 
;;;     (define vars (extract-vars prog)) 
;;;     (define ctx
;;;         (context
;;;         vars
;;;         (get-representation float-type)
;;;         (map (λ (_) (get-representation float-type)) vars)))
;;;     ctx)

(define (get-simplified-expr test)
  (define rules (*rules*))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))
  (define ctx (test-context test))
  (define expr (test-input test))

  (*context* ctx)

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
      (,rules . ((node . ,(*node-limit*))))
      (,lowering-rules . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch (list expr)))

  (define runner (make-egraph batch (batch-roots batch) (list (context-repr ctx)) schedule))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  (debatchref (first (first batchrefss))))

(define expr-hash (make-hash))

(define (get-error-spec spec ctx) 
    (*num-points* 8000) 
    (*context* ctx)
    (define pcon (get-sample-spec spec))
    (define error (errors spec pcon ctx))
    (define err-score (errors-score error))
    err-score)

(define (process-file file-path)
  (with-input-from-file file-path
    (lambda ()
      (for ([line (in-lines)])

        (define split-line (regexp-split #px"," line))
        (define test (car (load-port (open-input-string (car split-line)))))
        (define count (read (open-input-string (car (cdr split-line)))))


    (with-handlers ([exn? (λ (e) (printf "Skipping due to error: ~a\n" (exn-message e)))])
            (define simple-expr (get-simplified-expr test))
            (hash-update! expr-hash simple-expr
            (lambda (current-val) (+ current-val count))
            count)))))
            
        (for ([(key value) (in-hash expr-hash)])
        (printf "~a, ~a\n" key value)
    ))

;;; (with-handlers ([exn:fail:user:herbie:sampling? (λ (e) (printf "Skipping due to error: ~a\n" (exn-message e)))])
;;;         (define sample (sample-points precon (list expr) (list ctx)))
;;;           (define pcon (mk-pcontext sample))
;;;           (define errs (errors expr (cdr pcon) ctx))
;;;           (printf "~a , ~a , ~a , ~a , ~a , ~a\n" 
;;;                   (errors-score errs) 
;;;                   expr 
;;;                   spec 
;;;                   cost 
;;;                   count 
;;;                   (length vars))
;;;                   )
(define file-path (vector-ref (current-command-line-arguments) 0))
(process-file file-path)

;;; (for ([(key value) expr-hash])
;;;   (printf "~a,  ~a\n" (prog->spec key) value))



    
;;; (get-error "(FPCore (z0 z1 z2) (* (log (sqrt (+ (* z0 z0) (* z1 z1)))) z2))")


;;; (displayln (get-error "(FPCore (z0 z1 ) (/.f64 (neg.f64 z0) z1))"))

;;; (count-errors "top1k_deduped.fpcore")