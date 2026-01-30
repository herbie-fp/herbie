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
  "../src/core/batch.rkt"
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
(activate-platform! "no-accelerators")
(struct candidate (name spec score cost))


;;; ------------------------- HELPERS ---------------------------------
(define (register-op spec name)
  (define ctx (get-ctx spec))
  (define impl
    (create-operator-impl!
     name
     ctx
     #:spec spec
     #:impl (from-rival)
     #:fpcore '(! :precision binary64 (name z0 z1))
     #:cost 0))
  (platform-register-implementation! (*active-platform*) impl)
  (void))

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
    (candidate name spec score cost)))

(define sorted-cands (sort scored-pairs > #:key candidate-score))

(when (null? sorted-cands)
  (displayln "No accelerators discovered in this iteration.")
  (exit 0))

(define chosen-cand (first sorted-cands))
(define fpcore (candidate-spec chosen-cand))
(define name (candidate-name chosen-cand))
(define cost (candidate-cost chosen-cand))
(define fake-cost (floor (/ cost 5)))

(define accelerators-path "reports/accelerators.json")
(define existing-accelerators
  (if (file-exists? accelerators-path)
      (let ([data (call-with-input-file accelerators-path read-json)])
        (cond
          [(vector? data) (vector->list data)]
          [(list? data) data]
          [else '()]))
      '()))

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
