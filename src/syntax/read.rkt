#lang racket

(require "../core/programs.rkt"
         "../utils/common.rkt"
         "../utils/errors.rkt"
         "platform.rkt"
         "sugar.rkt"
         "syntax-check.rkt"
         "syntax.rkt"
         "type-check.rkt"
         "types.rkt")

(provide (struct-out test)
         test-context
         test-output-repr
         test-var-reprs
         load-tests
         parse-test)

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

(define (array-of dims elem)
  (cond
    [(null? dims) elem]
    [else
     (define repr (make-array-representation #:elem elem #:len (first dims)))
     (define name (representation-name repr))
     (define existing (and (repr-exists? name) (get-representation name)))
     (array-of (rest dims) (or existing repr))]))

(define (precision->real-repr prec)
  (unless (symbol? prec)
    (raise-herbie-error "Invalid :precision ~a; expected a real representation name" prec))
  (define repr (get-representation prec))
  (unless (equal? (representation-type repr) 'real)
    (raise-herbie-error "Invalid :precision ~a; expected a real representation name" prec))
  repr)

(define (parse-test stx)
  (assert-program! stx)
  (define stx* (expand-core stx))
  (define output-repr (assert-program-typed! stx*))
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
  (define default-repr (precision->real-repr default-prec))

  (define-values (var-names var-reprs)
    (for/lists (var-names var-reprs)
               ([var (in-list args)])
               (match var
                 [(list '! props ... name dims ...)
                  (define prop-dict (props->dict props))
                  (define arg-prec (dict-ref prop-dict ':precision default-prec))
                  (define arg-repr (precision->real-repr arg-prec))
                  (values name (array-of dims arg-repr))]
                 [(list (? symbol? name) dims ...) (values name (array-of dims default-repr))]
                 [(? symbol? name) (values name default-repr)])))
  (define ctx (context var-names default-repr var-reprs))

  ;; Named fpcores need to be added to function table
  (when func-name
    (register-function! func-name args default-prec body))

  ;; Try props first, then identifier, else the expression itself
  (define name (or (dict-ref prop-dict ':name #f) func-name body))

  ;; inline and desugar
  (define body* (fpcore->prog body ctx))
  (define pre* (fpcore->prog (dict-ref prop-dict ':pre 'TRUE) ctx))

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
  (check-unused-variables var-names body* pre*)
  (check-weird-variables var-names)

  (test (~a name)
        func-name
        var-names
        body*
        targets
        (dict-ref prop-dict ':herbie-expected #t)
        spec
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

(module+ test
  (require rackunit
           "../utils/float.rkt"
           "../syntax/load-platform.rkt")

  (activate-platform! (*platform-name*))
  (define precision 'binary64)
  (define ctx (context '(x y z a) <binary64> (make-list 4 <binary64>)))

  ;; inlining

  ;; Test classic quadp and quadm examples
  (register-function! 'discr (list 'a 'b 'c) precision `(sqrt (- (* b b) (* a c))))
  (define quadp `(/ (+ (- y) (discr x y z)) x))
  (define quadm `(/ (- (- y) (discr x y z)) x))
  (check-equal? (fpcore->prog quadp ctx)
                '(/.f64 (+.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 x z)))) x))
  (check-equal? (fpcore->prog quadm ctx)
                '(/.f64 (-.f64 (neg.f64 y) (sqrt.f64 (-.f64 (*.f64 y y) (*.f64 x z)))) x))

  ;; x^5 = x^3 * x^2
  (register-function! 'sqr (list 'x) precision '(* x x))
  (register-function! 'cube (list 'x) precision '(* x x x))
  (define fifth '(* (cube a) (sqr a)))
  (check-equal? (fpcore->prog fifth ctx) '(*.f64 (*.f64 (*.f64 a a) a) (*.f64 a a)))

  ;; casting edge cases
  (check-equal? (fpcore->prog `(cast x) ctx) 'x)
  (check-equal? (fpcore->prog `(cast (! :precision binary64 x)) ctx) 'x))
