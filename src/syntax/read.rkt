#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "platform.rkt"
         "platform-state.rkt"
         "sugar.rkt"
         "syntax-check.rkt"
         "syntax.rkt"
         "type-check.rkt"
         "types.rkt")

(provide (struct-out test)
         test-context
         test-output-repr
         test-var-reprs
         load-test
         load-tests
         parse-test)

(define (free-variables prog)
  (match prog
    [(? literal?) '()]
    [(? number?) '()]
    [(? symbol?) (list prog)]
    [(approx _ impl) (free-variables impl)]
    [(list _ args ...) (remove-duplicates (append-map free-variables args))]))

(define *register-named-fpcore-operators?* (make-parameter #t))

(struct test (name identifier vars input output expected spec pre output-repr-name var-repr-names)
  #:prefab)

(define (test-output-repr test)
  (get-representation (test-output-repr-name test)))

(define (test-var-reprs test)
  (map get-representation (map cdr (test-var-repr-names test))))

(define (test-context test)
  (define output-repr (get-representation (test-output-repr-name test)))
  (define vars (test-vars test))
  (define var-reprs
    (for/list ([var vars])
      (get-representation (dict-ref (test-var-repr-names test) var))))
  (context (test-vars test) output-repr var-reprs))

;; Unfortunately copied from `src/syntax/sugar.rkt`
(define (expand stx)
  (match stx
    ; expand let statements
    [#`(let* ([#,vars #,vals] ...) #,body)
     (datum->syntax #f
                    (list 'let*
                          (for/list ([var (in-list vars)]
                                     [val (in-list vals)])
                            (list var (expand val)))
                          (expand body))
                    stx)]
    [#`(let ([#,vars #,vals] ...) #,body)
     (datum->syntax #f
                    (list 'let
                          (for/list ([var (in-list vars)]
                                     [val (in-list vals)])
                            (list var (expand val)))
                          (expand body))
                    stx)]
    ; special nullary operators
    [#`(,(or 'and 'or)) (datum->syntax #f 'TRUE stx)]
    [#`(+)
     (warn 'nullary-operator "+ is deprecated as a nullary operator")
     (datum->syntax #f 0 stx)]
    [#`(*)
     (warn 'nullary-operator "* is deprecated as a nullary operator")
     (datum->syntax #f 1 stx)]
    ; special unary operators
    [#`(,(or 'and 'or) #,a) (expand a)]
    ; deprecated unary operators
    [#`(,(and (or '+ '*) op) #,a)
     (warn 'unary-operator "~a is deprecated as a unary operator" op)
     (expand a)]
    [#`(/ #,a)
     (warn 'unary-operator "/ is deprecated as a unary operator")
     (datum->syntax #f (list '/ 1 (expand a)) stx)]
    ; binary operators
    [#`(,(and (or '+ '- '* '/ 'or) op) #,arg1 #,arg2)
     (datum->syntax #f (list op (expand arg1) (expand arg2)) stx)]
    ; variary operators
    [#`(,(and (or '+ '- '* '/ 'or) op) #,arg1 #,arg2 #,rest ...)
     (unless (null? rest)
       (warn 'variary-operator "~a is deprecated as a variary operator" op))
     (define prev (datum->syntax #f (list op (expand arg1) (expand arg2)) stx))
     (let loop ([prev prev]
                [rest rest])
       (match rest
         [(list) prev]
         [(list next rest ...)
          (define prev* (datum->syntax #f (list op prev (expand next)) next))
          (loop prev* rest)]))]
    [#`(,(and (or '< '<= '> '>= '=) op) #,args ...)
     (define args* (map expand args))
     (define out
       (for/fold ([out #f])
                 ([term args*]
                  [next (cdr args*)])
         (datum->syntax #f
                        (if out
                            (list 'and out (list op term next))
                            (list op term next))
                        term)))
     (or out (datum->syntax #f 'TRUE stx))]
    [#`(!= #,args ...)
     (define args* (map expand args))
     (define out
       (for/fold ([out #f])
                 ([term args*]
                  [i (in-naturals)]
                  #:when #t
                  [term2 args*]
                  [j (in-naturals)]
                  #:when (< i j))
         (datum->syntax #f
                        (if out
                            (list 'and out (list '!= term term2))
                            (list '!= term term2))
                        stx)))
     (or out (datum->syntax #f 'TRUE stx))]
    ; other operators
    [#`(#,op #,args ...) (datum->syntax #f (cons op (map expand args)) stx)]
    ; numbers, variables
    [_ stx]))

(define (expand-core stx)
  (match stx
    [#`(FPCore #,name (#,vars ...) #,props ... #,body)
     (datum->syntax #f (append (list 'FPCore name vars) props (list (expand body))) stx)]
    [#`(FPCore (#,vars ...) #,props ... #,body)
     (datum->syntax #f (append (list 'FPCore vars) props (list (expand body))) stx)]))

(define (parse-platform-name ann)
  (match ann
    [(list '! props ... body)
     (define dict (props->dict props))
     (dict-ref dict ':herbie-platform #f)]
    [_ #f]))

(define (parse-test stx)
  (assert-program! stx)
  (define stx* (expand-core stx))
  (define-values (output-repr ctx) (assert-program-typed! stx*))
  (define-values (func-name args props body)
    (match (syntax->datum stx*)
      [(list 'FPCore name (list args ...) props ... body) (values name args props body)]
      [(list 'FPCore (list args ...) props ... body) (values #f args props body)]))

  ;; NOTE: We intentionally do not use (define prop-dict (props->dict props)) here
  ;; despite its apparent efficiency. props->dict could be considered, if :alt was a unique key
  ;; in our property. When there are multiple entries with the same key, props->dict would collapse
  ;; them, and prevent mutliple Developer Targets from being represented correctly.
  ;; This less efficient implementation preserves all entries, and maintains dict-ref functionality.
  (define prop-dict
    (let loop ([props props])
      (match props
        ['() '()]
        [(list prop val rest ...) (cons (cons prop val) (loop rest))])))

  (define default-prec (dict-ref prop-dict ':precision (*default-precision*)))
  (define var-names (context-vars ctx))
  (define var-reprs (context-var-reprs ctx))

  ;; Try props first, then identifier, else the expression itself
  (define name (or (dict-ref prop-dict ':name #f) func-name body))

  ;; inline and desugar
  (define body* (fpcore->prog body ctx))
  (define pre* (fpcore->spec (dict-ref prop-dict ':pre 'TRUE)))

  (define targets
    (for/list ([(key val) (in-dict prop-dict)]
               #:when (eq? key ':alt))
      (match (parse-platform-name val) ; plat-name is symbol or #f
        ; If plat-name extracted, check if name matches
        [(? symbol? plat-name) (cons val (equal? (~a plat-name) (*platform-name*)))]
        ; try to lower
        [#f
         (with-handlers ([exn:fail:user:herbie:missing? (lambda (e) (cons val #f))])
           ; Testing if error thrown
           (fpcore->prog val ctx)
           (cons val #t))])))

  (define spec (fpcore->prog (dict-ref prop-dict ':spec body) ctx))

  ;; Named fpcores become platform operators
  (when (and func-name (*register-named-fpcore-operators?*))
    (register-fpcore-operator! func-name (struct-copy context ctx [repr output-repr]) body* spec))
  (check-unused-variables var-names body* pre*)
  (check-weird-variables var-names)

  (test (~a name)
        func-name
        var-names
        body*
        targets
        (dict-ref prop-dict ':herbie-expected #t)
        (prog->spec spec)
        pre*
        (representation-name output-repr)
        (for/list ([var (in-list var-names)]
                   [repr (in-list var-reprs)])
          (cons var (representation-name repr)))))

(define (check-unused-variables vars precondition expr)
  ;; Fun story: you might want variables in the precondition that
  ;; don't appear in the `expr`, because that can allow you to do
  ;; non-uniform sampling. For example, if you have the precondition
  ;; `(< x y)`, where `y` is otherwise unused, then `x` is sampled
  ;; non-uniformly (biased toward small values).
  (define used (set-union (free-variables expr) (free-variables precondition)))
  (unless (set=? vars used)
    (define unused (set-subtract vars used))
    (warn 'unused-variable
          #:url "faq.html#unused-variable"
          "unused ~a ~a"
          (if (equal? (set-count unused) 1) "variable" "variables")
          (string-join (map ~a unused) ", "))))

(define (check-weird-variables vars)
  (for ([var (in-list vars)])
    (define const-name (string->symbol (string-upcase (symbol->string var))))
    (when (operator-exists? const-name)
      (warn 'strange-variable
            #:url "faq.html#strange-variable"
            "unusual variable ~a; did you mean ~a?"
            var
            const-name))))

(define (our-read-syntax port name)
  (parameterize ([read-decimal-as-inexact false])
    (read-syntax port name)))

(define (load-port port)
  (port-count-lines! port)
  (for/list ([test (in-port (curry our-read-syntax "stdin") port)])
    (parse-test test)))

(define (load-file file)
  (call-with-input-file file
                        (λ (port)
                          (port-count-lines! port)
                          (for/list ([test (in-port (curry our-read-syntax file) port)])
                            (parse-test test)))))

(define (load-directory dir)
  (apply append
         (for/list ([fname (in-directory dir)]
                    #:when (file-exists? fname)
                    #:when (equal? (filename-extension fname) #"fpcore"))
           (load-file fname))))

(define (load-tests path)
  (define path*
    (if (string? path)
        (string->path path)
        path))
  (define out
    (cond
      [(port? path) (load-port path)]
      [(equal? path "-") (load-port (current-input-port))]
      [(directory-exists? path*) (load-directory path*)]
      [else (load-file path*)]))
  (define duplicates (find-duplicates (map test-name out)))
  (unless (null? duplicates)
    (warn 'duplicate-names
          "Duplicate ~a ~a used for multiple cores"
          (if (equal? (length duplicates) 1) "name" "names")
          (string-join (map (curry format "\"~a\"") duplicates) ", ")))
  out)

(define (load-test path)
  (parameterize ([*register-named-fpcore-operators?* #f])
    (last (load-tests path))))

(module+ test
  (require rackunit
           "../syntax/float.rkt"
           "../syntax/load-platform.rkt")

  (activate-platform! (*platform-name*))
  (define precision 'binary64)
  (define ctx (context '(x y z a) <binary64> (make-list 4 <binary64>)))

  ;; named FPCore operators

  ;; Test classic quadp and quadm examples
  (define discr-ctx (context '(a b c) <binary64> (make-list 3 <binary64>)))
  (define discr-body `(sqrt (- (* b b) (* a c))))
  (define discr-prog (fpcore->prog discr-body discr-ctx))
  (register-fpcore-operator! 'discr discr-ctx discr-prog discr-prog)
  (define quadp `(/ (+ (- y) (discr x y z)) x))
  (define quadm `(/ (- (- y) (discr x y z)) x))
  (check-equal? (fpcore->prog quadp ctx) '(/.f64 (+.f64 (neg.f64 y) (discr x y z)) x))
  (check-equal? (fpcore->prog quadm ctx) '(/.f64 (-.f64 (neg.f64 y) (discr x y z)) x))

  ;; x^5 = x^3 * x^2
  (define sqr-ctx (context '(x) <binary64> (list <binary64>)))
  (define sqr-prog (fpcore->prog '(* x x) sqr-ctx))
  (register-fpcore-operator! 'sqr sqr-ctx sqr-prog sqr-prog)
  (define cube-ctx (context '(x) <binary64> (list <binary64>)))
  (define cube-prog (fpcore->prog '(* x x x) cube-ctx))
  (register-fpcore-operator! 'cube cube-ctx cube-prog cube-prog)
  (define fifth '(* (cube a) (sqr a)))
  (check-equal? (fpcore->prog fifth ctx) '(*.f64 (cube a) (sqr a)))

  ;; array arguments
  (define vec2 (make-array-representation #:elem <binary64> #:len 2))
  (define sum2-ctx (context '(v) <binary64> (list vec2)))
  (define sum2-prog (fpcore->prog '(+ (ref v 0) (ref v 1)) sum2-ctx))
  (register-fpcore-operator! 'sum2 sum2-ctx sum2-prog sum2-prog)
  (define vec-ctx (context '(a) <binary64> (list vec2)))
  (check-equal? (fpcore->prog '(sum2 a) vec-ctx) '(sum2 a))
  (check-equal? ((impl-info 'sum2 'fl) #(1.0 2.0)) 3.0)

  ;; array return values
  (define vec-out-ctx (context '(x y) vec2 (list <binary64> <binary64>)))
  (define vec-out-prog (fpcore->prog '(array x y) vec-out-ctx))
  (register-fpcore-operator! 'mkvec vec-out-ctx vec-out-prog vec-out-prog)
  (check-equal? (prog->spec vec-out-prog) '(array x y))
  (check-equal? ((impl-info 'mkvec 'fl) 1.0 2.0) #(1.0 2.0))

  ;; serialized platform state can restore named operators on a fresh platform copy
  (define state (platform-serialize))
  (activate-platform! "math")
  (check-false (impl-exists? 'mkvec))
  (activate-platform! state)
  (check-equal? ((impl-info 'mkvec 'fl) 1.0 2.0) #(1.0 2.0))

  ;; sequential reads can reference previously defined named FPCore operators
  (define port
    (open-input-string (string-append "(FPCore helper (x) (+ x 1))\n" "(FPCore (x) (helper x))\n")))
  (check-equal? (length (load-tests port)) 2)

  ;; casting edge cases
  (check-equal? (fpcore->prog `(cast x) ctx) 'x)
  (check-equal? (fpcore->prog `(cast (! :precision binary64 x)) ctx) 'x))
