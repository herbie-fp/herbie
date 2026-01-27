#lang racket
(require json)
(require
  "../src/syntax/load-platform.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/platform-language.rkt")

(activate-platform! "grow")

(define filename (vector-ref (current-command-line-arguments) 0))

(define count-list (call-with-input-file "reports/counts.rkt" read))
(define cost-list (call-with-input-file "reports/costs.rkt" read))

(define json (string->jsexpr (first (file->lines filename))))
(define tests (hash-ref json 'tests))
(define scored-pairs
  (for/list ([t tests])
    (define input-str (hash-ref t 'input))
    (define link (hash-ref t 'link))
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
    (list input-str link end-val score cost)))

(define sorted-pairs (sort scored-pairs > #:key fourth))

(when (null? sorted-pairs)
  (displayln "No accelerators discovered in this iteration.")
  (exit 0))

(define chosen-pair (first sorted-pairs))
(define fpcore (with-input-from-string (first chosen-pair) read))
(define link (second chosen-pair))
(define cost (last chosen-pair))
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

(when (ormap (lambda (entry) (equal? (hash-ref entry 'name #f) link)) existing-accelerators)
  (displayln (format "accelerator ~a already present; skipping" link))
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
                                link
                                (string-join (map render-var-f64 (free-variables spec)))
                                spec
                                link
                                (string-join (map symbol->string (free-variables spec)))
                                fake-cost))

(define operator-strf32 (format "(define-operation (~a.f32 ~a) <binary32> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary32 (~a ~a)) #:cost ~a)"
                                link
                                (string-join (map render-var-f32 (free-variables spec)))
                                spec
                                link
                                (string-join (map symbol->string (free-variables spec)))
                                fake-cost))


(with-output-to-file "growlibm/grow.rkt"
  (lambda ()
    (displayln operator-strf64)
    (displayln operator-strf32))
  #:exists 'append)

(define new-entry (hash 'name link 'spec (format "~a" spec)))

(define updated-accelerators
  (append existing-accelerators (list new-entry)))

(call-with-output-file accelerators-path
  (lambda (out)
    (write-json updated-accelerators out))
  #:exists 'truncate)

(displayln (format "adding accelerator ~a, with spec: ~a" link spec))
