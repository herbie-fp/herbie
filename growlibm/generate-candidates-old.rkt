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

(activate-platform! "no-accelerators")
(*node-limit* 50000)
(define (all-subexpressions* expr)
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
  (remove-duplicates subexprs))

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

(define (remove-approxes expr)
  (match expr
    [(approx _ impl) (remove-approxes impl)]
    [(list op args ...) (cons op (map remove-approxes args))]
    [_ expr]))

;;; (define (get-error expr)
;;;   (with-handlers ([exn? (lambda (exn) 0)])
;;;     (define ctx (get-ctx expr))
;;;     (define spec (prog->spec expr))
;;;     (*num-points* 8000)
;;;     (*context* ctx)
;;;     (define pcon (get-spec-sample spec))
;;;     (define error (errors expr pcon ctx))
;;;     (define err-score (errors-score error))
;;;     err-score))

(define (best-exprs exprs ctxs)
  (*context* (max-ctx ctxs))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
    (define schedule '(lift rewrite lower))

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
(define unflattened-subexprs (map all-subexpressions* (map remove-approxes lines)))

(define subexprs (apply append unflattened-subexprs))

(define filtered-subexprs
  (filter (lambda (n)
            (not (or (symbol? n)
                     (literal? n)
                     (number? n))))
          subexprs))

(define filtered-again (filter (lambda (n)
                                 (and (> (length (free-variables n)) 0)
                                      (< (length (free-variables n)) 4))) filtered-subexprs))

(define renamed-subexprs (map rename-vars filtered-again))
(define pairs (hash->list (count-frequencies renamed-subexprs)))

(define deduplicated-pairs (hash->list (deduplicate pairs)))

(define sorted-pairs (sort deduplicated-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define first-2000 (take sorted-pairs (min (length sorted-pairs) 2000)))

;;; (define filtered (filter (lambda (p) (< 0.1 (get-error (car p)))) first-2000))

;;; (define filtered first-2000)

(define first-500 (take first-2000 (min (length first-2000) 500)))
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
