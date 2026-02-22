#lang racket

(require
  "../src/syntax/load-platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "growlibm-common.rkt"
  "../src/api/sandbox.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/syntax/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/platform.rkt"
  "../src/utils/common.rkt"
  "../src/utils/errors.rkt")

;;; ------------------------- SETUP ---------------------------------
(activate-platform! "grow")
(*node-limit* 50000)
(define report-dir (vector-ref (current-command-line-arguments) 0))
(define candidate-num (string->number (vector-ref (current-command-line-arguments) 1)))
(define err-threshold 0.1)

;;; ------------------------- HELPERS ---------------------------------
(define cost-proc (platform-cost-proc (*active-platform*)))

(define (get-cost expr)
  (cost-proc expr (get-representation 'binary64)))

(define (eliminate-ifs expr)
  (define comparison-bases
    '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64
            <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op) (member op comparison-bases))

  (define (pure-math? e)
    (let check ([e e])
      (match e
        [(or `(if.f32 ,_ ,_ ,_)
             `(if.f64 ,_ ,_ ,_)) #f]
        [(list (? comparison-op?) _ _) #f]
        [(list _ args ...)
         (andmap check args)]
        [_ #t])))

  (reap [sow]
        (let loop ([expr expr])
          (match expr
            [(or `(if ,test ,t ,f)
                 `(if.f32 ,test ,t ,f)
                 `(if.f64 ,test ,t ,f))
             (loop test) (loop t) (loop f)]

            [(list (? comparison-op?) lhs rhs)
             (loop lhs) (loop rhs)]

            [(list op args ...)
             (if (pure-math? expr)
                 (sow expr)
                 (for ([arg args])
                   (loop arg)))]

            [_ (void)]))))

(define (get-subexpressions expr)
  (define subexprs
    (reap [sow]
          (let loop ([expr expr])
            (match expr
              [_
               (match expr
                 [(? number?) (void)]
                 [(? literal?) (void)]
                 [(? symbol?) (void)]
                 [(list op args ...)
                  (sow expr)
                  (for ([arg args]
                        [i (in-naturals)])
                    (define args-with-hole (list-set args i 'HOLE))
                    (define cut-expr (cons op args-with-hole))
                    (sow cut-expr)
                    (loop arg)
                    )]
                 [_ (void)])]))))
  subexprs)

(define (get-error expr)
  (with-handlers ([exn:fail:user:herbie:sampling? (lambda (exn) (displayln (format "Error getting error for expr ~a: ~a" expr exn)) 0)])
    (*num-points* 100)
    (define ctx (get-ctx expr))
    (*context* ctx)
    (define pcon (get-sample (expr->test expr #:precision 'binary64)))
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
  (define batchrefss (egraph-best runner batch))
  (define batch-pull (batch-exprs batch))
  (map (compose batch-pull first) batchrefss))

(define (alpha-rename impl)
  (define free-vars(free-variables impl))
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
  (define vars (free-variables expr))
  (define ctx (get-ctx expr))
  (format "(FPCore ~a ~a)" vars (prog->fpcore expr ctx)))

(define (candidate-expr? n)
  (and (not (or (symbol? n) (literal? n) (number? n)))
       (> (length (free-variables n)) 0)
       (< (length (free-variables n)) 4)))

(define (log-info name number report-dir)
  (with-output-to-file (string-append report-dir "/info.txt")
    (lambda () (display (format "~a, ~a\n" name number)))
    #:exists 'append))

;;; ------------------------- MAIN PIPELINE ---------------------------------
(define root-count-hash (make-hash))
(define candidate-count-hash (make-hash))
(define canonical-candidate-count-hash (make-hash))
(define subexpr-count 0)

(define roots (file->list (string-append report-dir "/expr_dump.txt")))
(define roots-no-conditionals (append* (map eliminate-ifs roots)))
(define canonical-roots (map alpha-rename (run-egg (map alpha-rename roots-no-conditionals))))

(for ([c canonical-roots])
  (hash-update! root-count-hash c add1 0))

(for ([(root-expr root-count) (in-hash root-count-hash)])
  (define subexprs (alpha-rename (get-subexpressions root-expr)))
  (set! subexpr-count (+ subexpr-count (length subexprs)))
  (for ([c subexprs])
    (hash-update! candidate-count-hash c (lambda (old) (+ old root-count)) 0)))

(define canonical-candidates (map alpha-rename (run-egg (hash-keys candidate-count-hash))))

(for ([candidate canonical-candidates]
      [cand-count (in-hash-values candidate-count-hash)])
  (hash-update! canonical-candidate-count-hash candidate (lambda (old) (+ old cand-count)) 0))

(define pairs-raw (hash->list canonical-candidate-count-hash))
(define filtered-pairs (filter (lambda (x) (candidate-expr? (car x))) pairs-raw))
(define filtered-pairs* (filter (lambda (x) (> (cdr x) 1)) filtered-pairs))
(define sorted-pairs (sort filtered-pairs* (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define top-pairs (take sorted-pairs (min (length sorted-pairs) (* 2 candidate-num))))
(define final-pairs (filter (lambda (x) (> (get-error (car x)) err-threshold)) top-pairs))

;; Output
(log-info "roots" (length roots) report-dir)
(log-info "roots no conditionals" (length roots-no-conditionals) report-dir)
(log-info "canonical roots" (hash-count root-count-hash) report-dir)
(log-info "subexpressions" subexpr-count report-dir)
(log-info "canonical subexpressions" (hash-count canonical-candidate-count-hash) report-dir)

(define final-output (take final-pairs (min (length final-pairs) candidate-num)))
(define fpcores-out (map to-fpcore-str final-output))
(define counts-out (map (lambda (p) (cons (prog->fpcore (car p) (get-ctx (car p))) (cdr p)))
                        final-output))

(define costs-out (map (lambda (p) (cons (prog->fpcore (car p) (get-ctx (car p))) (get-cost (car p))))
                       final-output))

(define full-cands-out (map (lambda (c) (format "~a, ~a\n" (prog->fpcore (car c) (get-ctx (car c))) (cdr c))) sorted-pairs))

(with-output-to-file (string-append report-dir "/full-candidates.txt")
  (lambda () (for-each display full-cands-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/counts.rkt")
  (lambda () (display counts-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/costs.rkt")
  (lambda () (display costs-out))
  #:exists 'replace)

(with-output-to-file (string-append report-dir "/candidates.txt")
  (lambda () (for-each displayln fpcores-out))
  #:exists 'replace)
