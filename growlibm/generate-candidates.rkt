#lang racket

(require
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/utils/common.rkt"
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
  "../src/utils/common.rkt"
  "../src/reports/common.rkt")

;;; ------------------------- SETUP ---------------------------------
(activate-platform! "no-accelerators")
(*node-limit* 50000)
(*num-points* 1000)
(define report-dir (vector-ref (current-command-line-arguments) 0))
;;; ------------------------- HELPERS ---------------------------------
(define (get-cost expr)
  (cost-proc expr (get-representation 'binary64)))

(define cost-proc (platform-cost-proc (*active-platform*)))

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

(define (to-fpcore-str pair)
  (define expr (car pair))
  (define vars (sort (free-variables expr) symbol<?))
  (define ctx (get-ctx expr))
  (format "(FPCore ~a ~a)" vars (prog->fpcore expr ctx)))

(define (log-info name number report-dir)
  (with-output-to-file (string-append report-dir "/info.txt")
    (lambda () (display (format "~a, ~a\n" name number)))
    #:exists 'append))

;;; ------------------------- MAIN PIPELINE ---------------------------------
(define roots (file->list (string-append report-dir "/expr_dump.txt")))
(log-info "roots" (length roots) report-dir)

(define alpha-renamed-roots (map alpha-rename roots))
(define canonical-roots (run-egg alpha-renamed-roots))

(define subexprs (append* (map get-subexpressions canonical-roots)))
(log-info "subexprs" (length subexprs) report-dir)

(define filtered-subexprs
  (filter (lambda (n)
            (and (not (or (symbol? n) (literal? n) (number? n)))
                 (> (length (free-variables n)) 0)
                 (< (length (free-variables n)) 4)))
          subexprs))

(define alpha-renamed-subexprs (map alpha-rename filtered-subexprs))
(log-info "filtered subexprs" (length alpha-renamed-subexprs) report-dir)

(define canonical-candidates (run-egg alpha-renamed-subexprs))

(define counts (make-hash))
(for ([c canonical-candidates])
  (hash-update! counts c add1 0))

(define cand-count-pairs (hash->list counts))
(log-info "deduped candidates" (length cand-count-pairs) report-dir)

(define sorted-cand-count-pairs (sort cand-count-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))

(define top-candidates (take sorted-cand-count-pairs (min (length sorted-cand-count-pairs) 2000)))

(define non-exact-candidates
  (filter (lambda (p) (< 0.1 (get-error (car p)))) top-candidates))

(log-info "non-exact candidates" (length non-exact-candidates) report-dir)
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