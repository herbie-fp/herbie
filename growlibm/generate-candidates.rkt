#lang racket

(require
  "../src/api/sandbox.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/syntax/platform-language.rkt")

(activate-platform! "no-accelerators")

(define (strip-approx expr)
  (match expr
    [(? approx?) (strip-approx (approx-impl expr))]
    [(? hole?) (strip-approx (hole-spec expr))]
    [`(if ,c ,t ,f) `(if ,(strip-approx c)
                         ,(strip-approx t)
                         ,(strip-approx f))]
    [(list op args ...) (cons op (map strip-approx args))]
    [_ expr]))

(define (comparison-symbol? sym)
  (define name (symbol->string sym))
  (or (string-contains? name "=")
      (string-contains? name "<")
      (string-contains? name ">")))

(define (contains-comparison? expr)
  (match expr
    [(? symbol?) #f]
    [(? number?) #f]
    [(? literal?) #f]
    [`(if ,c ,t ,f) (or (contains-comparison? c)
                        (contains-comparison? t)
                        (contains-comparison? f))]
    [(list (? symbol? op) args ...)
     (or (comparison-symbol? op)
         (ormap contains-comparison? args))]
    [(list args ...)
     (ormap contains-comparison? args)]
    [_ #f]))

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

(define (best-exprs exprs ctxs)
  (*context* (max-ctx ctxs))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
    (define schedule '(lift rewrite unsound lower))

  ; run egg
  (define-values (batch brfs)
    (progs->batch exprs))

  (define runner (make-egraph batch brfs (map context-repr ctxs) schedule (max-ctx ctxs)))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  batchrefss)

(define (rename-vars impl)
  (define free-vars (sort (free-variables impl) symbol<?))
  (define varDict
    (for/hash ([v free-vars]
               [i (in-naturals)])
      (values v (string->symbol (format "z~a" i)))))
  (define impl* (replace-vars varDict impl))
  impl*)

(define (count-frequencies xs)
  (define ht (make-hash))
  (for ([x xs])
    (hash-update! ht x add1 0))
  ht)

(define (max-ctx ctxs)
  (foldl (lambda (a b) (if (> (length (context-vars a)) (length (context-vars b))) a b))
         (context (list) (get-representation 'binary64) (list))
         ctxs))

(define (get-ctx expr)
  (define free-vars (free-variables expr))
  (context free-vars (get-representation 'binary64)
           (make-list (length free-vars) (get-representation 'binary64))))

(define (deduplicate pairs)
  (define exprs (map car pairs))
  (define counts (map cdr pairs))
  (define ctxs (map get-ctx exprs))
  (define ht (make-hash))
  (define best (best-exprs exprs ctxs))
  (for ([b best]
        [c counts])
    (hash-update! ht (batch-pull (first b)) (lambda (n) (+ n c)) 0))

  ht)

(define (to-fpcore-str pair)
  (define expr (car pair))
  (define vars (sort (free-variables expr) symbol<?))
  (define ctx (get-ctx expr))
  (format "(FPCore ~a ~a)" vars (prog->fpcore expr ctx)))

(define (to-count-print p)
  (define expr (car p))
  (define count (cdr p))
  (define ctx (get-ctx expr))
  (cons (prog->fpcore expr ctx) count))

(define report-dir (vector-ref (current-command-line-arguments) 0))

(define lines (file->list (string-append report-dir "/expr_dump.txt")))
(define unflattened-subexprs  (map all-subexpressions lines))

(define subexprs (map strip-approx (apply append unflattened-subexprs)))
(define filtered-subexprs
  (filter (lambda (n)
            (not (or (symbol? n)
                     (literal? n)
                     (number? n)
                     (contains-comparison? n))))
          subexprs))
(define filtered-again (filter (lambda (n)
                                 (and (> (length (free-variables n)) 0)
                                      (< (length (free-variables n)) 4))) filtered-subexprs))

(define renamed-subexprs (map rename-vars filtered-again))
(define pairs (hash->list (count-frequencies renamed-subexprs)))

(define deduplicated-pairs (hash->list (deduplicate pairs)))

(define sorted-pairs (sort deduplicated-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define first-2000 (take sorted-pairs (min (length sorted-pairs) 2000)))

(define filtered (filter (lambda (p) (< 0.1 (get-error (car p)))) first-2000))
;;; (define filtered first-2000)
(define first-500 (take filtered (min (length filtered) 500)))
(define fpcores-out (map to-fpcore-str first-500))
(define counts-out (map to-count-print first-500))

(with-output-to-file (string-append report-dir "/counts.rkt")
  (lambda ()
    (display counts-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/candidates.txt")
  (lambda ()
    (for-each displayln fpcores-out))
  #:exists 'replace)

(module+ test
  (require rackunit)
  (check-equal? (rename-vars '(+ x y)) '(+ z0 z1)))
