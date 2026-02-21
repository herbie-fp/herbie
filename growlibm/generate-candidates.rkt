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
  "../src/syntax/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/types.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
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

;;; (define (get-subexpressions expr)
;;;   (define comparison-bases '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
;;;   (define (comparison-op? op)
;;;     (and (symbol? op)
;;;          (member op comparison-bases)))
;;;   (define subexprs
;;;     (reap [sow]
;;;           (let loop ([expr expr])
;;;             (match expr
;;;               [(or `(if ,test ,t ,f)
;;;                    `(if.f32 ,test ,t ,f)
;;;                    `(if.f64 ,test ,t ,f))
;;;                (loop test)
;;;                (loop t)
;;;                (loop f)]
;;;               [(approx _ impl)
;;;                (loop impl)]
;;;               [(list (? comparison-op?) lhs rhs)
;;;                (loop lhs)
;;;                (loop rhs)]
;;;               [_
;;;                (sow expr)
;;;                (match expr
;;;                  [(? number?) (void)]
;;;                  [(? literal?) (void)]
;;;                  [(? symbol?) (void)]
;;;                  [(list _ args ...)
;;;                   (for ([arg args])
;;;                     (loop arg))]
;;;                  [_ (void)])]))))
;;;   subexprs)

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

(define (push-holes expr)
  (reap [sow]
    (let loop ([current-expr expr]
               [context (lambda (hole) hole)]) 

      (match current-expr
        [(list op args ...)
         (define candidate (context 'hole))
         
         (cond
           [(and (not (eq? candidate 'hole))
                 (> (get-error candidate) err-threshold))
            (sow candidate)

            (when (> (get-error current-expr) err-threshold)
              (for ([arg args] [i (in-naturals)])
                (loop arg (lambda (h) 
                            (cons op (list-set args i h))))))]
           [else
            (for ([arg args] [i (in-naturals)])
              (loop arg (lambda (h) 
                          (context (cons op (list-set args i h))))))])]

        [_ (void)]))))

(define (push-holes-exhaustive expr)
  (reap [sow]
    (let loop ([current-expr expr]
               [context (lambda (hole) hole)]) 
      (match current-expr
        [(list op args ...)
         (define candidate (context 'hole))
         
         ;; 1. Check current level
         (when (and (not (eq? candidate 'hole))
                    (> (get-error candidate) err-threshold))
           (sow candidate))

         ;; 2. ALWAYS recurse (No 'else', no stopping)
         (for ([arg args] [i (in-naturals)])
           (loop arg (lambda (h) 
                       (context (cons op (list-set args i h))))))]
        [_ (void)]))))

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
  ;;;   (define egglog-runner (make-egglog-runner batch brfs (map context-repr ctxs) schedule (ctx-union ctxs)))
  (define batchrefss (egraph-best runner batch))
  ;;;   (define batchrefss (run-egglog egglog-runner batch #:extract 1000000))
  (define batch-pull (batch-exprs batch))
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
(define root-counts (make-hash))
(define candidate-counts (make-hash))

(define roots (file->list (string-append report-dir "/expr_dump.txt")))
(log-info "roots" (length roots) report-dir)

(define pure-math (append* (map eliminate-ifs roots)))

(log-info "no-ifs" (length pure-math) report-dir)

(define alpha-renamed (map alpha-rename pure-math))
(define canonical (run-egg alpha-renamed))

(for ([c canonical])
  (hash-update! root-counts c add1 0))

(log-info "canonical roots" (hash-count root-counts) report-dir)

(define subexpr-count 0)

(for ([(root-expr mult) (in-hash root-counts)])
  (define subexprs (get-subexpressions root-expr))
  (set! subexpr-count (+ subexpr-count (length subexprs)))
  (define filtered-subexprs
    (filter (lambda (n)
              (and (not (or (symbol? n) (literal? n) (number? n)))
                   (> (length (free-variables n)) 0)
                   (< (length (free-variables n)) 4)))
            subexprs))
  (define alpha-renamed-subexprs (map alpha-rename filtered-subexprs))
  (define canonical-subexprs (run-egg alpha-renamed-subexprs))
  (define alpha-renamed-canonical-subexprs (map alpha-rename canonical-subexprs))
  (for ([c alpha-renamed-canonical-subexprs])
    (hash-update! candidate-counts c (lambda (old) (+ old mult)) 0)))

(log-info "total subexprs" subexpr-count report-dir)

(log-info "canonical subexprs" (hash-count candidate-counts) report-dir)

(define cand-count-pairs (hash->list candidate-counts))

(define sorted-cand-count-pairs (sort cand-count-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))

(define full-cands (map (lambda (c) (format "~a, ~a\n" (prog->fpcore (car c) (get-ctx (car c))) (cdr c))) sorted-cand-count-pairs))

(with-output-to-file (string-append report-dir "/full-candidates.txt")
  (lambda () (for-each display full-cands))
  #:exists 'replace)

(define top-pairs (take sorted-cand-count-pairs (min (length sorted-cand-count-pairs) (* 2 candidate-num))))

(define filtered-pairs (filter (lambda (x) (> (get-error (car x)) 0.1)) top-pairs))

(log-info "filtered pairs" (length filtered-pairs) report-dir)

;; Output
(define final-output (take filtered-pairs (min (length filtered-pairs) candidate-num)))
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