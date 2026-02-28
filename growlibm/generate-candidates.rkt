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
(define fixed-cut-depths '(2 3 4))
(define cut-hole-symbol 'HOLE)
(define egg-batch-size 5000)
(define interesting-ops '(fabs.f32 sin.f32 cos.f32 tan.f32 sinh.f32 cosh.f32 tanh.f32 asin.f32 acos.f32
                                   atan.f32 asinh.f32 atanh.f32 acosh.f32 atan2.f32 exp.f32 exp2.f32 log.f32 log10.f32
                                   log2.f32 logb.f32 ceil.f32 floor.f32 sqrt.f32 cbrt.f32 pow.f32 fmax.f32 fmin.f32 fmod.f32
                                   fabs.f64 sin.f64 cos.f64 tan.f64 sinh.f64 cosh.f64 tanh.f64 asin.f64 acos.f64
                                   atan.f64 asinh.f64 atanh.f64 acosh.f64 atan2.f64 exp.f64 exp2.f64 log.f64 log10.f64
                                   log2.f64 logb.f64 ceil.f64 floor.f64 sqrt.f64 cbrt.f64 pow.f64 fmax.f64 fmin.f64 fmod.f64))
(define max-vars 3)

;;; ------------------------- HELPERS ---------------------------------
(define cost-proc (platform-cost-proc (*active-platform*)))

(define (get-cost expr)
  (cost-proc expr (get-representation 'binary64)))

(define (cut-expr-to-depth expr depth)
  (match expr
    [(list op args ...)
     (if (<= depth 0)
         cut-hole-symbol
         (cons op (map (lambda (arg) (cut-expr-to-depth arg (sub1 depth))) args)))]
    [_ expr]))

(define (get-fixed-depth-cuts expr)
  (match expr
    [(list _ _ ...)
     (define cuts
       (for/list ([depth fixed-cut-depths])
         (cut-expr-to-depth expr depth)))
     (remove-duplicates (filter (lambda (cut) (not (equal? cut expr))) cuts))]
    [_ '()]))

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
                  (for ([arg args])
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
  (for/list ([orig-expr (in-list exprs)]
             [refs (in-list batchrefss)])
    (if (empty? refs)
        orig-expr                        
        (batch-pull (first refs)))))     

(define (run-egg-batched batch-size old-hash new-hash)
  (for ([batch (in-slice batch-size (in-hash-pairs old-hash))])
    (define canonical-exprs (run-egg (map car batch)))
    (for ([canonical-expr (in-list canonical-exprs)]
          [pair (in-list batch)])
      (define expr* (alpha-rename canonical-expr))
      (define count (cdr pair))
      (hash-update! new-hash expr* (lambda (old) (+ old count)) 0))))

(define (alpha-rename impl)
  (define free-vars (free-variables impl))
  (define varDict
    (for/hash ([v free-vars]
               [i (in-naturals)])
      (values v (string->symbol (format "z~a" i)))))
  (define impl* (replace-vars varDict impl))
  impl*)

(define (alpha-rename-all impl)
  (define free-vars (free-variables impl))
  (define n (length free-vars))

  (define new-vars
    (for/list ([i (in-range n)])
      (string->symbol (format "z~a" i))))

  (define perms (permutations new-vars))
  (for/list ([perm (in-list perms)])
    (define varDict
      (for/hash ([v (in-list free-vars)]
                 [z (in-list perm)])
        (values v z)))
    (replace-vars varDict impl)))

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

(define (candidate-expr? expr)
  (and (not (or (symbol? expr) (literal? expr) (number? expr)))
       (has-some-free-vars? expr)
       (has-not-too-many-free-vars? expr)))

(define (has-some-free-vars? expr)
  (> (length (free-variables expr)) 0))

(define (has-not-too-many-free-vars? expr )
  (<= (length (free-variables expr)) max-vars))

(define (contains-interesting-op? expr)
  (let loop ([expr expr])
    (match expr
      [(list op args ...)
       (or (member op interesting-ops)
           (ormap loop args))]
      [_ #f])))

(define (log-info name number report-dir)
  (with-output-to-file (string-append report-dir "/info.txt")
    (lambda () (display (format "~a, ~a\n" name number)))
    #:exists 'append))

;;; ------------------------- MAIN PIPELINE ---------------------------------
(define root-hash (make-hash))
(define canonical-root-hash (make-hash))
(define candidate-hash (make-hash))
(define canonical-candidate-hash (make-hash))

(define roots (file->list (string-append report-dir "/expr_dump.txt")))

(for* ([root (in-list roots)]
       [expr (in-list (eliminate-ifs root))])
  (hash-update! root-hash (alpha-rename expr) add1 0))

(run-egg-batched egg-batch-size root-hash canonical-root-hash)

(for* ([(root-expr root-count) (in-hash canonical-root-hash)]
       [subexpr (in-list (get-subexpressions root-expr))]
       #:when (candidate-expr? subexpr)
       [c (in-list (alpha-rename-all subexpr))])
  (hash-update! candidate-hash c (lambda (old) (+ old root-count)) 0))

(run-egg-batched egg-batch-size candidate-hash canonical-candidate-hash)

(define pairs-raw (hash->list canonical-candidate-hash))
(define filtered-pairs (filter (lambda (x) (and (> (cdr x) 1)
                                                (contains-interesting-op? (car x))
                                                (candidate-expr? (car x))))
                               pairs-raw))

(define sorted-pairs (sort filtered-pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define top-pairs (take sorted-pairs (min (length sorted-pairs) (* 2 candidate-num))))
(define final-pairs (filter (lambda (x) (> (get-error (car x)) err-threshold)) top-pairs))

;; Output
(log-info "roots" (length roots) report-dir)
(log-info "canonical roots" (hash-count canonical-root-hash) report-dir)
(log-info "candidates" (hash-count candidate-hash) report-dir)
(log-info "canonical candidates" (hash-count canonical-candidate-hash) report-dir)
(log-info "filtered candidates" (length filtered-pairs) report-dir)

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
