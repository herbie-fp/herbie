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

(define (get-spec-error expr spec ctx)
  (with-handlers ([exn? (lambda (exn) 0)])
    (*num-points* 8000)
    (*context* ctx)
    (define pcon (get-spec-sample spec))
    (define error (errors expr pcon ctx))
    (define err-score (errors-score error))
    err-score))

(define (get-simplified-expr ctx expr)
  (define rules (*rules*))
  ;;; (displayln ctx)
  ;;; (displayln expr)
  (*context* ctx)

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    (list `(lift . ((iteration . 1) (scheduler . simple)))
          `(,rules . ((node . ,(*node-limit*))))
          `(lower . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define batch (progs->batch (list expr)))

  (define runner (make-egraph batch (list (context-repr ctx)) schedule ctx))
  ; batchrefss is a (listof (listof batchref))
  (define batchrefss (egraph-best runner batch))
  (debatchref (first (first batchrefss))))

(define (rename-vars impl)
  (define free-vars (free-variables impl))
  (define varDict
    (for/hash ([v free-vars]
               [i (in-naturals)])
      (values v (string->symbol (format "z~a" i)))))
  (define replacedImpl (replace-vars varDict impl))
  replacedImpl)

(define (print-lines l)
  (for-each (lambda (element)
              (displayln element))
            l))

(define (count-frequencies xs)
  (define ht (make-hash))
  (for ([x xs])
    (hash-update! ht x add1 0))
  ht)

(define (deduplicate pairs)
  (define ht (make-hash))
  (for ([pair pairs])
    (define expr (car pair))
    (define count (cdr pair))
    (define ctx (context (free-variables expr) (get-representation 'binary64) (make-list (length (free-variables expr)) (get-representation 'binary64))))
    (define deduped (get-simplified-expr ctx expr))
    (hash-update! ht deduped (lambda (n) (+ n count)) 0))
  ht)

(define (has-approx expr)
  (define str (format "~v" expr))
  (or (string-contains? str "approx")
      (string-contains? str "=")
      (string-contains? str ">")
      (string-contains? str "<"))
  )

(define (get-error expr)
  (define vars (free-variables expr))
  (define ctx (context vars
                       (get-representation 'binary64)
                       (make-list (length vars)
                                  (get-representation 'binary64))))
  (define spec (prog->spec expr))
  (get-spec-error expr spec ctx)
  )

(define (print-fpcore expr)
  (define vars (free-variables expr))
  (define ctx (context (free-variables expr)
                       (get-representation 'binary64)
                       (make-list (length vars)
                                  (get-representation 'binary64))))
  (displayln (format "(FPCore ~a ~a)" vars (prog->fpcore expr ctx))))

(define (to-count-print p)
  (define expr (car p))
  (define count (cdr p))
  (define ctx (context (free-variables expr)
                       (get-representation 'binary64)
                       (make-list (length (free-variables expr))
                                  (get-representation 'binary64))))
  (cons (prog->fpcore expr ctx) count))

(define filename (vector-ref (current-command-line-arguments) 0))

(define lines (file->list filename))
(define unflattened-subexprs  (map all-subexpressions lines))

(define subexprs (apply append unflattened-subexprs))
(define filtered-subexprs (filter (lambda (n) (not (or (symbol? n) (literal? n) (approx? n) (has-approx n)))) subexprs))
(define filtered-again (filter (lambda (n) (> (length (free-variables n)) 0)) filtered-subexprs))
(define renamed-subexprs (map rename-vars filtered-again))

(define freqs (count-frequencies renamed-subexprs))
(define pairs (hash->list freqs))
;;; (displayln (format "og length ~a" (length pairs)))
(define deduplicated-freqs (deduplicate pairs))
(define deduplicated-pairs (hash->list deduplicated-freqs))
(define sorted-pairs (sort deduplicated-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define first-1000 (take sorted-pairs (min (length sorted-pairs) 1000)))

;;; (displayln (format "deduped length ~a" (length deduplicated-pairs)))
(define filtered (filter (lambda (p) (< 0 (get-error (car p)))) first-1000))

;;; (define sorted-triples (sort filtered-triples (lambda (p1 p2) (< (second p1) (second p2)))))
;;; (define first-500 (take sorted-triples (min (length sorted-triples) 500)))
;;; (displayln (format "filtered length ~a" (length filtered-triples)))
(define first-500 (take filtered (min (length filtered) 500)))
(for-each (lambda (p) (print-fpcore (car p))) first-500)

(define counts-to-print (map to-count-print first-500))
(define counts-file "reports/counts.rkt")
(with-output-to-file counts-file
  (lambda ()
    (display counts-to-print)))

;;; (print-lines sorted-triples)
;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; (define quads (map (lambda (p1) (append p1 (list (cost-proc (first p1) (get-representation 'binary64))))) sorted-triples))
;;; (print-lines quads)

;;; (define expr '(-.f64 #s(literal 1 binary64) (sqrt.f64 z0)))
;;; (define spec (prog->spec expr))
;;; (define ctx (context (free-variables expr) (get-representation 'binary64) (make-list (length (free-variables expr)) (get-representation 'binary64))))
;;; (define error (get-spec-error expr spec ctx))
;;; (displayln error)

(module+ test
  (require rackunit)
  (check-equal? (rename-vars '(+ x y)) '(+ z0 z1)))