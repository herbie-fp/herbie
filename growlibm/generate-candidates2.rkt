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
(define (get-subexpressions expr)
  (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (and (symbol? op)
         (member op comparison-bases)))
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (match expr
              [(or `(if ,test ,t ,f)
                   `(if.f32 ,test ,t ,f)
                   `(if.f64 ,test ,t ,f))
               (loop test)
               (loop t)
               (loop f)]
              [(approx _ impl)
               (loop impl)]
              [(list (? comparison-op?) lhs rhs)
               (loop lhs)
               (loop rhs)]
              [_
               (sow expr)
               (match expr
                 [(? number?) (void)]
                 [(? literal?) (void)]
                 [(? symbol?) (void)]
                 [(list _ args ...)
                  (for ([arg args])
                    (loop arg))]
                 [_ (void)])]))))
    subexprs)

(define (get-subexpressions2 expr)
  (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (and (symbol? op)
         (member op comparison-bases)))
  
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (match expr
              [(or `(if ,test ,t ,f)
                   `(if.f32 ,test ,t ,f)
                   `(if.f64 ,test ,t ,f))
               (loop test)
               (loop t)
               (loop f)]
              [(approx _ impl)
               (loop impl)]
              [(list (? comparison-op?) lhs rhs)
               (loop lhs)
               (loop rhs)]
              [_
               (sow expr)
               (match expr
                 [(? number?) (void)]
                 [(? literal?) (void)]
                 [(? symbol?) (void)]
                 [(list op args ...)
                  ;; --- UPDATED LOGIC FOR ALL COMBINATIONS ---
                  
                  ;; 1. Get a list of indices: (0 1 2 ...)
                  (define idxs (range (length args)))
                  
                  ;; 2. Get all subsets of indices to replace (excluding empty set)
                  (define subsets (combinations idxs))
                  
                  (for ([subset subsets])
                    (unless (null? subset) ;; Skip the case where nothing is replaced
                      (define new-args
                        (for/list ([arg args]
                                   [i (in-naturals)])
                          (if (member i subset)
                              ;; If this index is in the subset, replace with hole
                              (string->symbol (format "hole~a" i))
                              ;; Otherwise keep the original arg
                              arg)))
                      (sow (cons op new-args))))
                  
                  ;; -------------------------------------------

                  (for ([arg args])
                    (loop arg))]
                 [_ (void)])]))))
  subexprs)

(define (get-subexpressions3 expr)
  (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (and (symbol? op)
         (member op comparison-bases)))
  
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (match expr
              ;; Skip control flow and comparisons
              [(or `(if ,test ,t ,f)
                   `(if.f32 ,test ,t ,f)
                   `(if.f64 ,test ,t ,f))
               (loop test)
               (loop t)
               (loop f)]
              [(approx _ impl)
               (loop impl)]
              [(list (? comparison-op?) lhs rhs)
               (loop lhs)
               (loop rhs)]
              
              ;; Process standard math expressions
              [_
               (sow expr)
               (match expr
                 [(? number?) (void)]
                 [(? literal?) (void)]
                 [(? symbol?) (void)]
                 [(list op args ...)
                  
                  ;; Setup for Unified Holes (Content-based)
                  (define content-map (make-hash)) 
                  (define next-id 0)
                  (define (get-content-id arg)
                    (if (hash-has-key? content-map arg)
                        (hash-ref content-map arg)
                        (begin
                          (hash-set! content-map arg next-id)
                          (set! next-id (+ next-id 1))
                          (sub1 next-id))))

                  ;; Generate all combinations of children to replace
                  (define idxs (range (length args)))
                  (define subsets (combinations idxs))
                  
                  (for ([subset subsets])
                    (unless (null? subset)
                      
                      ;; VARIANT A: Distinct Holes (Positional)
                      ;; Always produces (+ hole0 hole1) regardless of content
                      (define distinct-args
                        (for/list ([arg args]
                                   [i (in-naturals)])
                          (if (member i subset)
                              (string->symbol (format "hole~a" i))
                              arg)))
                      (sow (cons op distinct-args))

                      ;; VARIANT B: Unified Holes (Content-based)
                      ;; Produces (+ hole0 hole0) if content matches
                      (define unified-args
                        (for/list ([arg args]
                                   [i (in-naturals)])
                          (if (member i subset)
                              (string->symbol (format "hole~a" (get-content-id arg)))
                              arg)))
                      
                      ;; Only sow Variant B if it is different from Variant A
                      ;; (Avoids duplicates when inputs are already distinct)
                      (unless (equal? distinct-args unified-args)
                        (sow (cons op unified-args)))))
                  
                  ;; Recurse
                  (for ([arg args])
                    (loop arg))]
                 [_ (void)])]))))
  subexprs)
(define (remove-approxes expr)
  (match expr
    [(approx _ impl) (remove-approxes impl)]
    [(list op args ...) (cons op (map remove-approxes args))]
    [_ expr]))

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

(define subexprs (append* (map get-subexpressions3 canonical-roots)))
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

(define high-error-candidates
  (filter (lambda (p) (< 0.1 (get-error (car p)))) top-candidates))

(print-info "high-error candidates" (length high-error-candidates))

;; Output
(define final-output (take high-error-candidates (min (length high-error-candidates) 500)))
(define fpcores-out (map to-fpcore-str final-output))
(define counts-out (map (lambda (p) (cons (prog->fpcore (car p) (get-ctx (car p))) (cdr p))) final-output))

(with-output-to-file (string-append report-dir "/counts.rkt")
  (lambda () (display counts-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/candidates.txt")
  (lambda () (for-each displayln fpcores-out))
  #:exists 'replace)