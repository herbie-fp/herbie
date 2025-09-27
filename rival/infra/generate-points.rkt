#lang racket
(require (only-in fpbench interval range-table-ref condition->range-table [expr? fpcore-expr?]))
(require math/base
         math/flonum)

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr) (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

(define (unfold-let expr)
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define bindings (map cons vars (map unfold-let vals)))
     (replace-vars bindings (unfold-let body))]
    [`(let* () ,body) (unfold-let body)]
    [`(let* ([,var ,val]
             ,rest ...)
        ,body)
     (replace-vars (list (cons var (unfold-let val))) (unfold-let `(let* ,rest ,body)))]
    [`(,head ,args ...) (cons head (map unfold-let args))]
    [x x]))

(define (expand-associativity expr)
  (match expr
    [(list (and (or '+ '- '* '/) op) a ..2 b)
     (list op (expand-associativity (cons op a)) (expand-associativity b))]
    [(list (or '+ '*) a) (expand-associativity a)]
    [(list '- a) (list 'neg (expand-associativity a))]
    [(list '/ a) (list '/ 1 (expand-associativity a))]
    [(list (or '+ '-)) 0]
    [(list (or '* '/)) 1]
    [(list op a ...) (cons op (map expand-associativity a))]
    [_ expr]))

(define (desugar expr)
  (expand-associativity (unfold-let expr)))

(define (parse-test stx)
  (match-define (list 'FPCore (? symbol?) ... (list args ...) props ... body) (syntax->datum stx))

  (define prop-dict
    (let loop ([props props])
      (match props
        ['() '()]
        [(list prop val rest ...) (cons (cons prop val) (loop rest))])))

  (list (dict-ref prop-dict ':pre 'TRUE) `(λ ,args ,(desugar body))))

(define (load-file file)
  (call-with-input-file file
                        (λ (port)
                          (for/list ([test (in-port (curry read-syntax file) port)])
                            (define name
                              (path->string (file-name-from-path (path-replace-extension file ""))))
                            (cons name (parse-test test))))))

(define (load-directory dir)
  (define-values (_u basename _un) (split-path dir))
  (define suite-name (path->string basename))
  (apply append
         (for/list ([fname (in-directory dir)]
                    #:when (file-exists? fname)
                    #:when (equal? (filename-extension fname) #"fpcore"))
           (for/list ([result (load-file fname)])
             (cons suite-name (rest result))))))

(define (load-tests path)
  (define path*
    (if (string? path)
        (string->path path)
        path))
  (apply append
         (for/list ([fname (directory-list path* #:build? true)]
                    #:when (or (directory-exists? fname)
                               (equal? (filename-extension fname) #"fpcore")))
           (cond
             [(directory-exists? fname) (load-directory fname)]
             [else (load-file fname)]))))

(define (program-variables prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  vars)

(define (sample-points variables precondition number)
  (define range-table (condition->range-table precondition))
  (for/list ([i (in-range number)])
    (for/list ([var variables])
      (match-define (interval lo hi lo? hi?) (first (range-table-ref range-table var)))
      (ordinal->flonum (random-integer (flonum->ordinal (real->double-flonum lo))
                                       (+ 1 (flonum->ordinal (real->double-flonum hi))))))))

(define (make-points tests output-file)
  (for ([test tests])
    (match-define (list benchname precondition program) test)
    (define points (sample-points (program-variables program) precondition 256))
    (for ([point points])
      (writeln (list benchname program point) output-file))))

(module+ main
  (command-line #:program "generate-points"
                #:args (benchmarks-dir output-file)
                (make-points (load-tests benchmarks-dir)
                             (open-output-file output-file #:exists 'replace))))
