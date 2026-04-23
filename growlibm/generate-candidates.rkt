#lang racket

(require "../src/syntax/load-platform.rkt"
         "../src/syntax/sugar.rkt"
         "../src/core/programs.rkt"
         "../src/syntax/syntax.rkt"
         "growlibm-common.rkt"
         "../src/api/sandbox.rkt"
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
(define cut-hole 'cut_hole)
(define egg-batch-size 5000)
(define interesting-ops
  '(fabs.f32 sin.f32
             cos.f32
             tan.f32
             sinh.f32
             cosh.f32
             tanh.f32
             asin.f32
             acos.f32
             atan.f32
             asinh.f32
             atanh.f32
             acosh.f32
             atan2.f32
             exp.f32
             exp2.f32
             log.f32
             log10.f32
             log2.f32
             logb.f32
             ceil.f32
             floor.f32
             sqrt.f32
             cbrt.f32
             pow.f32
             fmax.f32
             fmin.f32
             fmod.f32
             fabs.f64
             sin.f64
             cos.f64
             tan.f64
             sinh.f64
             cosh.f64
             tanh.f64
             asin.f64
             acos.f64
             atan.f64
             asinh.f64
             atanh.f64
             acosh.f64
             atan2.f64
             exp.f64
             exp2.f64
             log.f64
             log10.f64
             log2.f64
             logb.f64
             ceil.f64
             floor.f64
             sqrt.f64
             cbrt.f64
             pow.f64
             fmax.f64
             fmin.f64
             fmod.f64))
(define max-vars 3)
(struct candidate (spec cost count ctx))
;;; ------------------------- HELPERS ---------------------------------
(define cost-proc (platform-cost-proc (*active-platform*)))

(define (get-cost expr)
  (cost-proc expr (get-representation 'binary64)))

(define (operator-expr? e)
  (match e
    [(list _ _ ...) #t]
    [_ #f]))

(define (replace-child expr target replacement)
  (match expr
    [(list op args ...)
     (cons op
           (for/list ([arg (in-list args)]
                      [i (in-naturals)])
             (if (= i target) replacement arg)))]))

(define (direct-cuts expr)
  (match expr
    [(list _ args ...)
     (for/list ([arg (in-list args)]
                [i (in-naturals)]
                #:when (operator-expr? arg))
       (replace-child expr i cut-hole))]
    [_ '()]))

(define (get-subexpressions expr)
  (reap [sow]
        (let loop ([expr expr])
          (match expr
            [(or (? number?) (? literal?) (? symbol?)) (void)]
            [(list _ args ...)
             (sow expr)
             (for ([arg (in-list args)]
                   [i (in-naturals)])
               (when (operator-expr? arg)
                 (for ([cut (in-list (direct-cuts arg))])
                   (sow (replace-child expr i cut))))
               (loop arg))]
            [_ (void)]))))

(define (eliminate-ifs expr)
  (define comparison-bases
    '(<.f64 <=.f64 >.f64 >=.f64 ==.f64 !=.f64 <.f32 <=.f32 >.f32 >=.f32 ==.f32 !=.f32))
  (define (comparison-op? op)
    (member op comparison-bases))

  (define (pure-math? e)
    (let check ([e e])
      (match e
        [(or `(if.f32 ,_ ,_ ,_) `(if.f64 ,_ ,_ ,_)) #f]
        [(list (? comparison-op?) _ _) #f]
        [(list _ args ...) (andmap check args)]
        [_ #t])))

  (reap [sow]
        (let loop ([expr expr])
          (match expr
            [(or `(if ,test ,t ,f) `(if.f32 ,test ,t ,f) `(if.f64 ,test ,t ,f))
             (loop test)
             (loop t)
             (loop f)]

            [(list (? comparison-op?) lhs rhs)
             (loop lhs)
             (loop rhs)]

            [(list op args ...)
             (if (pure-math? expr)
                 (sow expr)
                 (for ([arg args])
                   (loop arg)))]

            [_ (void)]))))

(define (get-error expr)
  (with-handlers ([exn:fail:user:herbie:sampling?
                   (lambda (exn)
                     (displayln (format "Error getting error for expr ~a: ~a" expr exn))
                     0)])
    (*num-points* 100)
    (define test (expr->test expr #:precision 'binary64))
    (match-define (job-result 'sample _ 'success _ _ _ _ pcon) (run-herbie 'sample test))
    (match-define (job-result 'errors _ 'success _ _ _ _ point-errors)
      (run-herbie 'errors test #:pcontext pcon))
    (/ (for/sum ([entry (in-list point-errors)]) (cdr entry)) (length point-errors))))

(define (run-egg exprs)
  (define ctxs (map get-ctx exprs))
  (deduplicate-exprs exprs ctxs))

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

(define (to-fpcore-str cand)
  (define expr (candidate-spec cand))
  (define vars (free-variables expr))
  (format "(FPCore ~a ~a)" vars (prog->fpcore expr (candidate-ctx cand))))

(define (candidate-expr? expr)
  (and (not (or (symbol? expr) (literal? expr) (number? expr)))
       (has-some-free-vars? expr)
       (has-not-too-many-free-vars? expr)))

(define (has-some-free-vars? expr)
  (> (length (free-variables expr)) 0))

(define (has-not-too-many-free-vars? expr)
  (<= (length (free-variables expr)) max-vars))

(define (contains-interesting-op? expr)
  (let loop ([expr expr])
    (match expr
      [(list op args ...) (or (member op interesting-ops) (ormap loop args))]
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

(define dump-dir "dump-intermediates")
(define dump-files
  (sort (for/list ([name (in-list (directory-list dump-dir))]
                   #:when (regexp-match? #rx"[.]rktd$" (path->string name)))
          (build-path dump-dir name))
        string<?
        #:key path->string))
(define roots
  (for*/list ([dump-file (in-list dump-files)]
              [root (in-list (file->list dump-file))])
    root))

(for* ([root (in-list roots)]
       [expr (in-list (eliminate-ifs root))])
  (hash-update! root-hash (alpha-rename expr) add1 0))

(run-egg-batched egg-batch-size root-hash canonical-root-hash)

(for* ([(root-expr root-count) (in-hash canonical-root-hash)]
       [subexpr (in-list (get-subexpressions root-expr))]
       #:when (candidate-expr? subexpr))
  (define renamed-subexprs (alpha-rename subexpr))
  (for ([c (in-list renamed-subexprs)])
    (hash-update! candidate-hash c (lambda (old) (+ old root-count)) 0)))

(run-egg-batched egg-batch-size candidate-hash canonical-candidate-hash)

(define pairs-raw (hash->list canonical-candidate-hash))
(define candidates
  (map (lambda (p) (candidate (car p) (get-cost (car p)) (cdr p) (get-ctx (car p)))) pairs-raw))

(define filtered-candidates
  (filter (lambda (c)
            (and (> (candidate-count c) 1)
                 (contains-interesting-op? (candidate-spec c))
                 (candidate-expr? (candidate-spec c))))
          candidates))

(define sorted-candidates
  (sort filtered-candidates
        (lambda (c1 c2)
          (> (/ (candidate-count c1) (candidate-cost c1))
             (/ (candidate-count c2) (candidate-cost c2))))))

(define top-candidates (take sorted-candidates (min (length sorted-candidates) (* 2 candidate-num))))
(define final-candidates
  (filter (lambda (x) (> (get-error (candidate-spec x)) err-threshold)) top-candidates))

;; Output
(log-info "roots" (length roots) report-dir)
(log-info "canonical roots" (hash-count canonical-root-hash) report-dir)
(log-info "candidates" (hash-count candidate-hash) report-dir)
(log-info "canonical candidates" (hash-count canonical-candidate-hash) report-dir)
(log-info "filtered candidates" (length filtered-candidates) report-dir)

(define final-output (take final-candidates (min (length final-candidates) candidate-num)))
(define fpcores-out (map to-fpcore-str final-output))
(define counts-out
  (map (lambda (c) (cons (prog->fpcore (candidate-spec c) (candidate-ctx c)) (candidate-count c)))
       final-output))

(define costs-out
  (map (lambda (c) (cons (prog->fpcore (candidate-spec c) (candidate-ctx c)) (candidate-cost c)))
       final-output))

(define full-cands-out
  (map (lambda (c)
         (format "~a, ~a\n" (prog->fpcore (candidate-spec c) (candidate-ctx c)) (candidate-count c)))
       sorted-candidates))

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
