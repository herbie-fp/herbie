#lang racket
(require json)
(require
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "growlibm-common.rkt"
  "../src/api/sandbox.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/syntax/batch.rkt"
  "../src/core/mainloop.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/types.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/reports/common.rkt"
  "../src/syntax/platform-language.rkt")

;;; ------------------------- SETUP ---------------------------------
(activate-platform! "grow")
(struct candidate (name spec score cost error))

(define implication-threshold 0.5)
(define top-k 5)

;;; ------------------------- HELPERS ---------------------------------
(define (normalize-name name)
  (cond
    [(symbol? name) (symbol->string name)]
    [else (~a name)]))

(define (candidate-name->string cand)
  (normalize-name (candidate-name cand)))

(define (candidate-name->symbol cand)
  (string->symbol (candidate-name->string cand)))

(define (register-op! platform fpcore name cost)
  (parameterize ([*active-platform* platform])
    (define impl (fpcore->prog fpcore (get-ctx fpcore)))
    (define spec (prog->spec impl))
    (define ctx (get-ctx spec))
    (define vars (context-vars ctx))

    (define op-impl
      (create-operator-impl!
       name
       ctx
       #:spec spec
       #:impl (from-rival)
       #:fpcore `(! :precision binary64 (,name ,@vars))
       #:cost cost))
    (platform-register-implementation! platform op-impl)
    (void)))

(define (expr->test expr
                    #:name [name "scratch"]
                    #:precision [precision (*default-precision*)])
  (define vars (sort (free-variables expr) symbol<?))
  (define default-repr (get-representation precision))
  (define default-ctx
    (context vars default-repr (make-list (length vars) default-repr)))
  (define impl-expr
    (cond
      [(impl-prog? expr) expr]
      [(spec-prog? expr) (fpcore->prog expr default-ctx)]
      [else (raise-arguments-error 'expr->test "not a Herbie expression" "expr" expr)]))
  (define out-repr (repr-of impl-expr default-ctx))
  (define out-repr-name (representation-name out-repr))
  (define var-repr-names
    (for/list ([var (in-list vars)])
      (cons var out-repr-name)))
  (define spec impl-expr)
  (test name
        #f
        vars
        impl-expr
        '()
        #t
        spec
        '(TRUE)
        out-repr-name
        var-repr-names))

(define (run-herbie-expr expr platform
                         #:seed [seed #f]
                         #:name [name "scratch"]
                         #:precision [precision (*default-precision*)])
  (parameterize ([*active-platform* platform])
    (define test (expr->test expr #:name name #:precision precision))
    (define result (run-herbie 'improve test #:seed seed))
    (disable-flag! 'generate 'taylor)
    (match (job-result-status result)
      ['success
       (define backend (job-result-backend result))
       (define end (improve-result-end backend))
       (define end-best (first end))
       (define final-error (errors-score (alt-analysis-errors end-best)))
       final-error]
      [_
       (raise-arguments-error 'run-herbie-expr "Herbie run failed" "expr" expr)])))

;;; (define (improve-error-bits expr platform)
;;;     (reset!)
;;;     (parameterize ([*active-platform* platform])
;;;       (define test (expr->test expr))
;;;       (*context* (test-context test))
;;;       (define spec (prog->spec (or (test-spec test) (test-input test))))
;;;       (define pcontext (get-spec-sample spec))
;;;       (define alternatives (run-improve! (test-input test) (test-spec test) (*context*) pcontext))
;;;       (cond
;;;         [(null? alternatives) +inf.0]
;;;         [else
;;;          (define sorted-alts (sort-alts alternatives))
;;;          (define best-errs (cdr (first sorted-alts)))
;;;          (errors-score best-errs)])))

;;; ------------------------- MAIN PIPELINE ---------------------------------
(define filename (vector-ref (current-command-line-arguments) 0))
(define count-list (call-with-input-file "reports/counts.rkt" read))
(define cost-list (call-with-input-file "reports/costs.rkt" read))

(define json (string->jsexpr (first (file->lines filename))))
(define tests (hash-ref json 'tests))
(define scored-pairs
  (for/list ([t tests])
    (define input-str (hash-ref t 'input))
    (define name (hash-ref t 'link))
    (define end-val (hash-ref t 'end))
    (define spec (with-input-from-string input-str read))

    (define found-count (assoc spec count-list))
    (define found-cost (assoc spec cost-list))

    (define count (if found-count
                      (cdr found-count)
                      (begin
                        (displayln (format "~a not found" input-str))
                        0)))

    (define cost (if found-cost
                     (cdr found-cost)
                     (begin
                       (displayln (format "~a not found" input-str))
                       0)))

    (define score (if (number? end-val)
                      (/ (* end-val count) cost)
                      0))
    (candidate name spec score cost end-val)))

(define sorted-cands (sort scored-pairs > #:key candidate-score))

(define accelerators-path "reports/accelerators.json")
(define existing-accelerators
  (if (file-exists? accelerators-path)
      (let ([data (call-with-input-file accelerators-path read-json)])
        (cond
          [(vector? data) (vector->list data)]
          [(list? data) data]
          [else '()]))
      '()))

(define existing-name-set (make-hash))
(for ([entry (in-list existing-accelerators)])
  (define entry-name (hash-ref entry 'name #f))
  (when entry-name
    (hash-set! existing-name-set (normalize-name entry-name) #t)))

(define filtered-cands
  (filter (lambda (cand)
            (not (hash-has-key? existing-name-set (candidate-name->string cand))))
          sorted-cands))

(when (null? filtered-cands)
  (displayln "No accelerators discovered in this iteration.")
  (exit 0))

(define top-cands (take filtered-cands (min (length filtered-cands) top-k)))

(define base-platform (platform-copy (*active-platform*)))
(define implied-by (make-hash))

(for-each (lambda (cand)
            (displayln (format "~a: ~a"
                               (candidate-name->string cand)
                               (candidate-spec cand))))
          top-cands)

(when (> (length top-cands) 1)
  (for ([cand-a (in-list top-cands)])
    (displayln "")
    (displayln (format "considering implication from ~a: ~a" (candidate-name->string cand-a) (candidate-spec cand-a)))
    (define platform-a (platform-copy base-platform))
    (define name-a (candidate-name->string cand-a))
    (define spec-a (candidate-spec cand-a))
    (define fake-cost-a 0)
    (register-op! platform-a spec-a (candidate-name->symbol cand-a) fake-cost-a)
    (for ([cand-b (in-list top-cands)]
          #:unless (equal? (candidate-name->string cand-b) name-a))

      (define err (run-herbie-expr (candidate-spec cand-b) platform-a))
      (define diff (- (candidate-error cand-b) err))
      (define original-err (candidate-error cand-b))
      (define improvement-ratio
        (if (<= original-err 0)
            0.0
            (/ diff original-err)))
      (displayln  (format "     ~a: ~a, ~a ~a ~a" (candidate-name->string cand-b) (candidate-spec cand-b) err diff improvement-ratio))

      (when (> improvement-ratio implication-threshold)

        (displayln (format "     -> IMPLICATION DETECTED: ~a implies ~a" name-a (candidate-name->string cand-b)))
        (hash-set! implied-by (candidate-name->string cand-b) name-a)))))

(define chosen-cand
  (or (for/first ([cand (in-list top-cands)]
                  #:unless (hash-has-key? implied-by (candidate-name->string cand)))
        cand)
      (first top-cands)))

(define fpcore (candidate-spec chosen-cand))
(define name (candidate-name->string chosen-cand))
(define cost (candidate-cost chosen-cand))
(define fake-cost (floor (/ cost 5)))

(when (ormap (lambda (entry) (equal? (hash-ref entry 'name #f) name)) existing-accelerators)
  (displayln (format "accelerator ~a already present; skipping" name))
  (exit 0))

(define ctx (context (free-variables fpcore)
                     (get-representation 'binary64)
                     (make-list (length (free-variables fpcore))
                                (get-representation 'binary64))))

(define prog (fpcore->prog fpcore ctx))
(define spec (prog->spec prog))
(define (render-var-f64 var) (format "[~a <binary64>]" var))
(define (render-var-f32 var) (format "[~a <binary32>]" var))

(define operator-strf64 (format "(define-operation (~a.f64 ~a) <binary64> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary64 (~a ~a)) #:cost ~a)"
                                name
                                (string-join (map render-var-f64 (free-variables spec)))
                                spec
                                name
                                (string-join (map symbol->string (free-variables spec)))
                                fake-cost))

(define operator-strf32 (format "(define-operation (~a.f32 ~a) <binary32> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary32 (~a ~a)) #:cost ~a)"
                                name
                                (string-join (map render-var-f32 (free-variables spec)))
                                spec
                                name
                                (string-join (map symbol->string (free-variables spec)))
                                fake-cost))

(with-output-to-file "growlibm/grow.rkt"
  (lambda ()
    (displayln operator-strf64)
    (displayln operator-strf32))
  #:exists 'append)

(define new-entry (hash 'name name 'spec (format "~a" spec)))

(define updated-accelerators
  (append existing-accelerators (list new-entry)))

(call-with-output-file accelerators-path
  (lambda (out)
    (write-json updated-accelerators out))
  #:exists 'truncate)

(displayln (format "adding accelerator ~a, with spec: ~a" name spec))
