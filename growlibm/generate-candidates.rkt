#lang racket

(require
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
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/reports/common.rkt"
  "../src/syntax/platform-language.rkt")

(activate-platform! "no-accelerators")

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
  (define rules (*rules*))
  (*context* (max-ctx ctxs))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,100000)))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define-values (batch brfs)
    (progs->batch exprs))

  (define runner (make-egraph batch brfs (map context-repr ctxs) schedule (max-ctx ctxs)))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  batchrefss)

(define (rename-vars impl)
  (define free-vars (free-variables impl))
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

(define (has-approx expr)
  (define str (format "~v" expr))
  (or (string-contains? str "approx")
      (string-contains? str "=")
      (string-contains? str ">")
      (string-contains? str "<")))

(define (to-fpcore-str pair)
  (define expr (car pair))
  (define vars (free-variables expr))
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

(define subexprs (apply append unflattened-subexprs))
(define filtered-subexprs (filter (lambda (n) 
                                    (not (or (symbol? n) (literal? n) (approx? n) (has-approx n)))) subexprs))
(define filtered-again (filter (lambda (n) 
                                 (and (> (length (free-variables n)) 0)
                                      (< (length (free-variables n)) 4))) filtered-subexprs))

(define renamed-subexprs (map rename-vars filtered-again))
(define pairs (hash->list (count-frequencies renamed-subexprs)))

(with-output-to-file (string-append report-dir "/report_info.csv")
  (lambda ()
    (printf "og length, ~a\n"(length pairs)))
  #:exists 'replace)

(define deduplicated-pairs (hash->list (deduplicate pairs)))

(with-output-to-file (string-append report-dir "/report_info.csv")
  (lambda ()
    (printf "deduped length, ~a\n"(length deduplicated-pairs)))
  #:exists 'append)

(define sorted-pairs (sort deduplicated-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define first-1000 (take sorted-pairs (min (length sorted-pairs) 1000)))

(define filtered (filter (lambda (p) (< 0.1 (get-error (car p)))) first-1000))
;;; (define filtered first-1000)
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

;;; ;;; (print-lines sorted-triples)
;;; ;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; ;;; (define quads (map (lambda (p1) (append p1 (list (cost-proc (first p1) (get-representation 'binary64))))) sorted-triples))
;;; ;;; (print-lines quads)

;;; ;;; (define expr '(-.f64 #s(literal 1 binary64) (sqrt.f64 z0)))
;;; ;;; (define spec (prog->spec expr))
;;; ;;; (define ctx (context (free-variables expr) (get-representation 'binary64) (make-list (length (free-variables expr)) (get-representation 'binary64))))
;;; ;;; (define error (get-spec-error expr spec ctx))
;;; ;;; (displayln error)

;;; (module+ test
;;;   (require rackunit)
;;;   (check-equal? (rename-vars '(+ x y)) '(+ z0 z1)))


;;; (define expr '(+ x0 x0))
;;;   (displayln (best-exprs (list (get-ctx expr)) (list expr)))
