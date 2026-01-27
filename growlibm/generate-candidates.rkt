#lang racket

(require
  "../src/api/sandbox.rkt"
  "../src/core/points.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/core/points.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/utils/common.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/types.rkt")

;;; ------------------------- SETUP ---------------------------------
(activate-platform! "no-accelerators")
(*node-limit* 50000)

;;; ------------------------- HELPERS ---------------------------------
(define (get-holey-subexpressions expr)
  (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (and (symbol? op) (member op comparison-bases)))
  
  (define hole 'hole)

  (reap [sow]
    (let loop ([expr expr])
      (match expr
        ;; 1. CONTROL FLOW
        [(or `(if ,test ,t ,f)
             `(if.f32 ,test ,t ,f)
             `(if.f64 ,test ,t ,f))
         (loop test)
         (loop t)
         (loop f)
         (list expr hole)]

        ;; 2. COMPARISONS
        [(list (? comparison-op? op) lhs rhs)
         (loop lhs)
         (loop rhs)
         (list expr)]

        ;; 3. GENERIC OPS (Linear Expansion)
        [(list op args ...)
         ;; Recurse to get variants for all children
         (define arg-variants-list (map loop args))
         
         (define current-variants
           (append
            ;; A. Always include the original expression
            (list expr)
            
            ;; B. Generate One-Hot Variants:
            ;; Iterate through each child position 'i'
            (for*/list ([i (in-range (length args))]
                        [variant (list-ref arg-variants-list i)]
                        ;; Optimization: Don't reconstruct if the variant is just the original child
                        #:unless (equal? variant (list-ref args i)))
              
              ;; Construct: (op arg0 ... variant ... argN)
              ;; We take the original args, but swap the i-th one with the variant
              (append `(,op) 
                      (take args i) 
                      (list variant) 
                      (drop args (add1 i))))))
         
         ;; Sow valid variants
         (for ([v current-variants]) (sow v))
         
         ;; Return 'hole + linear variants to parent
         (cons hole current-variants)]

        ;; 4. LEAVES
        [_ 
         (sow expr)
         (list expr)]))))

(define cost-proc (platform-cost-proc (*active-platform*)))

(define (remove-approxes expr)
  (match expr
    [(approx _ impl) (remove-approxes impl)]
    [(list op args ...) (cons op (map remove-approxes args))]
    [_ expr]))

(define (get-cost expr)
  (cost-proc expr (get-representation 'binary64)))

(define (get-error expr)
  (with-handlers ([exn? (lambda (exn) 0)])
    (define ctx (get-ctx expr))
    (define spec (prog->spec expr))
    (*num-points* 8000)
    (*context* ctx)
    (define pcon (get-spec-sample spec))
    (define error (errors expr pcon ctx))
    (define err-score (errors-score error))
    err-score))

(define (run-egg exprs)
  (define ctxs (map get-ctx exprs))
  (*context* (ctx-union ctxs))
  (define schedule '(lift rewrite lower))

  (define-values (batch brfs)
    (progs->batch exprs))

  (define runner (make-egraph batch brfs (map context-repr ctxs) schedule (ctx-union ctxs)))
  ;;;   (define egglog-runner (make-egglog-runner batch brfs (map context-repr ctxs) schedule (ctx-union ctxs)))

  (define batchrefss (egraph-best runner batch))
  ;;;   (define batchrefss (run-egglog egglog-runner batch #:extract 1000000))
  (map (compose batch-pull first) batchrefss))

(define (alpha-rename impl)
  (define free-vars (sort (free-variables impl) symbol<?))
  (define varDict
    (for/hash ([v free-vars]
               [i (in-naturals)])
      (values v (string->symbol (format "z~a" i)))))
  (define impl* (replace-vars varDict impl))
  impl*)

(define (ctx-union ctxs)
  (define vars '())
  (define var-reprs '())
  (for ([ctx ctxs])
    (for ([var (context-vars ctx)]
          [repr (context-var-reprs ctx)])
      (unless (member var vars)
        (set! vars (append vars (list var)))
        (set! var-reprs (append var-reprs (list repr))))))
  (context vars (get-representation 'binary64) var-reprs))

(define (get-ctx expr)
  (define free-vars (free-variables expr))
  (context free-vars (get-representation 'binary64)
           (make-list (length free-vars) (get-representation 'binary64))))

(define (to-fpcore-str pair)
  (define expr (car pair))
  (define vars (sort (free-variables expr) symbol<?))
  (define ctx (get-ctx expr))
  (format "(FPCore ~a ~a)" vars (prog->fpcore expr ctx)))

(define (print-info name number)
  (with-output-to-file (string-append report-dir "/info.txt")
    (lambda () (display (format "~a, ~a\n" name number)))
    #:exists 'append))

;;; ------------------------- MAIN PIPELINE ---------------------------------
(define report-dir (vector-ref (current-command-line-arguments) 0))

(define roots (file->list (string-append report-dir "/expr_dump.txt")))
(print-info "roots" (length roots))

(define alpha-renamed-roots (map alpha-rename roots))
(define canonical-roots (run-egg alpha-renamed-roots))

(define subexprs (append* (map get-holey-subexpressions canonical-roots)))
(print-info "subexprs" (length subexprs))

(define filtered-subexprs
  (filter (lambda (n)
            (and (not (or (symbol? n) (literal? n) (number? n)))
                 (> (length (free-variables n)) 0)
                 (< (length (free-variables n)) 4)))
          subexprs))

(define alpha-renamed-subexprs (map alpha-rename filtered-subexprs))
(print-info "filtered subexprs" (length alpha-renamed-subexprs))

(define canonical-candidates (run-egg alpha-renamed-subexprs))

(define counts (make-hash))
(for ([c canonical-candidates])
  (hash-update! counts c add1 0))

(define cand-count-pairs (hash->list counts))
(print-info "deduped candidates" (length cand-count-pairs))

(define sorted-cand-count-pairs (sort cand-count-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))

(define top-candidates (take sorted-cand-count-pairs (min (length sorted-cand-count-pairs) 2000)))

(define non-exact-candidates
  (filter (lambda (p) (< 0.1 (get-error (car p)))) top-candidates))

(print-info "non-exact candidates" (length non-exact-candidates))
(define non-exact-out (map (lambda (c) (format "~a, ~a\n" (prog->fpcore (car c) (get-ctx (car c))) (cdr c))) non-exact-candidates))

(with-output-to-file (string-append report-dir "/full-candidates.txt")
  (lambda () (display non-exact-out))
  #:exists 'replace)

;; Output
(define final-output (take non-exact-candidates (min (length non-exact-candidates) 500)))
(define fpcores-out (map to-fpcore-str final-output))
(define counts-out (map (lambda (p) (cons (prog->fpcore (car p) (get-ctx (car p))) (cdr p))) 
                        final-output))

(define costs-out (map (lambda (p) (cons (prog->fpcore (car p) (get-ctx (car p))) (get-cost (car p))))
                        final-output))

(with-output-to-file (string-append report-dir "/counts.rkt")
  (lambda () (display counts-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/costs.rkt")
  (lambda () (display costs-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/candidates.txt")
  (lambda () (for-each displayln fpcores-out))
  #:exists 'replace)