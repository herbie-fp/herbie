#lang racket
(require json)
(require
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "growlibm-common.rkt"
  "../src/api/sandbox.rkt"
  "../src/core/points.rkt"
  "../src/config.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/platform-language.rkt")

;;; ------------------------- SETUP ---------------------------------
(activate-platform! "grow")
(struct candidate (name spec score cost error))
(define accelerators-path "reports/accelerators.json")
(define grow-platform-path "growlibm/grow.rkt")

(define implication-threshold 0.5)
(define top-k 10)

;;; ------------------------- HELPERS ---------------------------------
(define (register-op! platform fpcore name cost)
  (parameterize ([*active-platform* platform])
    (define impl (fpcore->prog fpcore (get-ctx fpcore)))
    (define spec (prog->spec impl))
    (define ctx (get-ctx spec))
    (define vars (context-vars ctx))
    (define name* (string->symbol name))

    (define op-impl
      (create-operator-impl!
       name*
       ctx
       #:spec spec
       #:impl (from-rival)
       #:fpcore `(! :precision binary64 (,name* ,@vars))
       #:cost cost))
    (platform-register-implementation! platform op-impl)
    (void)))

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

(define (add-accelerator cand)
  (define name (candidate-name cand))
  (define spec (candidate-spec cand))
  (define cost (candidate-cost cand))
  (define fake-cost (floor (/ cost 5)))

  (define ctx (context (free-variables spec)
                       (get-representation 'binary64)
                       (make-list (length (free-variables spec))
                                  (get-representation 'binary64))))

  (define prog (fpcore->prog spec ctx))
  (define spec* (prog->spec prog))
  (define free-vars (free-variables spec*))

  (define (render-var-f64 var) (format "[~a <binary64>]" var))
  (define (render-var-f32 var) (format "[~a <binary32>]" var))

  (define operator-strf64 (format "(define-operation (~a.f64 ~a) <binary64> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary64 (~a ~a)) #:cost ~a)"
                                  name
                                  (string-join (map render-var-f64 free-vars))
                                  spec*
                                  name
                                  (string-join (map symbol->string free-vars))
                                  fake-cost))

  (define operator-strf32 (format "(define-operation (~a.f32 ~a) <binary32> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary32 (~a ~a)) #:cost ~a)"
                                  name
                                  (string-join (map render-var-f32 free-vars))
                                  spec*
                                  name
                                  (string-join (map symbol->string free-vars))
                                  fake-cost))

  (with-output-to-file grow-platform-path
    (lambda ()
      (displayln operator-strf64)
      (displayln operator-strf32))
    #:exists 'append)

  (displayln (format "adding accelerator ~a, with spec: ~a" name spec)))

(define (can-reach? start-name target-name implied-by)
  (let loop ([curr start-name] [visited (set)])
    (cond [(equal? curr target-name) #t]
          [(set-member? visited curr) #f]
          [else (for/or ([neighbor (hash-ref implied-by curr (set))])
                  (loop neighbor (set-add visited curr)))])))

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
    (hash-set! existing-name-set  entry-name #t)))

(define filtered-cands
  (filter (lambda (cand)
            (not (hash-has-key? existing-name-set (candidate-name cand))))
          sorted-cands))

(when (null? filtered-cands)
  (displayln "No accelerators discovered in this iteration.")
  (exit 0))

(define top-cands (take filtered-cands (min (length filtered-cands) top-k)))

(define base-platform (platform-copy (*active-platform*)))
(define implied-by (make-hash))

(for-each (lambda (cand)
            (displayln (format "~a: ~a"
                               (candidate-name cand)
                               (candidate-spec cand))))
          top-cands)

(when (> (length top-cands) 1)
  (for ([cand-a (in-list top-cands)])
    (displayln "")
    (displayln (format "considering implication from ~a: ~a" (candidate-name cand-a) (candidate-spec cand-a)))
    (define platform-a (platform-copy base-platform))
    (define name-a (candidate-name cand-a))
    (define spec-a (candidate-spec cand-a))
    (define fake-cost-a 0)
    (register-op! platform-a spec-a (candidate-name cand-a) fake-cost-a)
    (for ([cand-b (in-list top-cands)]
          #:unless (equal? (candidate-name cand-b) name-a))

      (define err (run-herbie-expr (candidate-spec cand-b) platform-a))
      (define diff (- (candidate-error cand-b) err))
      (define original-err (candidate-error cand-b))
      (define improvement-ratio
        (if (<= original-err 0)
            0.0
            (/ diff original-err)))
      (displayln  (format "     ~a: ~a, ~a ~a ~a" (candidate-name cand-b) (candidate-spec cand-b) err diff improvement-ratio))
      (when (> improvement-ratio implication-threshold)
        (displayln (format "     -> IMPLICATION DETECTED: ~a implies ~a" name-a (candidate-name cand-b)))
        (define name-b (candidate-name cand-b))
        (hash-set! implied-by name-b (set-add (hash-ref implied-by name-b (set)) name-a))))))

(define (get-final-candidate-structs top-cands implied-by)
  (define name->struct
    (make-hash (map (lambda (c) (cons (candidate-name c) c)) top-cands)))
  (define all-names (hash-keys name->struct))
  (define reaches? (lambda (u v) (can-reach? v u implied-by)))
  (define is-source? (lambda (name)
                       (for/and ([other all-names])
                         (if (and (reaches? other name) (not (reaches? name other)))
                             #f #t))))
  (define source-names (filter is-source? all-names))
  (define unique-names (remove-duplicates source-names (lambda (a b) (reaches? a b))))

  (map (lambda (name) (hash-ref name->struct name)) unique-names))

(define to-add (get-final-candidate-structs top-cands implied-by))

(define new-json-entries
  (for/list ([cand (in-list to-add)])
    (add-accelerator cand) 
    (hash 'name (candidate-name cand) 
          'spec (format "~a" (candidate-spec cand)))))

(define current-file-content
  (if (file-exists? accelerators-path)
      (let ([data (call-with-input-file accelerators-path read-json)])
        (cond [(vector? data) (vector->list data)]
              [(list? data) data]
              [else '()]))
      '()))

(define final-list (append current-file-content new-json-entries))

(call-with-output-file accelerators-path
  (lambda (out)
    (write-json final-list out))
  #:exists 'truncate)