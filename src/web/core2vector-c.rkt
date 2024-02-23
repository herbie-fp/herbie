#lang racket

(require generic-flonum)
(require fpbench/src/imperative)

(provide core->vector-c)

;; TODO: There are a bunch of different ways to initialize vector constants. See https://www.agner.org/optimize/optimizing_assembly.pdf.

(define (visit-if/vector-c vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define prec (ctx-lookup-prop ctx ':precision))
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ift-ctx))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
  (define-values (ctx* name) (ctx-random-name iff-ctx prec))
  (define type (type->vector-c prec))
  (define suffix (precision->suffix prec))
  (printf "~a~a ~a = _mm256_blendv_~a(~a, ~a, ~a)\n"
          indent type name suffix
          ; we need to flip iff and ift since 0 means first argument
          iff* ift* cond*)
  (values name ctx*))

(define (visit-op_/vector-c vtor op args #:ctx ctx)
  (match (cons op args)
    [(list '- x)
     ;; TODO: Any better way to do this?
     (visit-op_/vector-c vtor '- (list 0 x) #:ctx ctx)]
    [else
     (define prec (ctx-lookup-prop ctx ':precision))
     (define indent (ctx-lookup-extra ctx 'indent))
     (define-values (name-ctx name) (ctx-random-name ctx prec))
     (define args*
       (for/list ([arg args])
         (define-values (arg* arg-ctx) (visit/ctx vtor arg ctx))
         arg*))
     (define ctx*
       (if (set-member? bool-ops op)
           (ctx-update-props name-ctx (list ':precision 'boolean))
           name-ctx))
     (printf "~a~a ~a = ~a;\n" indent (type->vector-c prec) name (operator->vector-c name op args* ctx))
     (values name ctx*)]))

(define (visit-number/vector-c vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->vector-c prec) name (constant->vector-c x ctx))
  (values name name-ctx))

(define (visit-constant/vector-c vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->vector-c prec) name (constant->vector-c x ctx))
  (values name 
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props name-ctx (list ':precision 'boolean))
              name-ctx)))

(define-expr-visitor imperative-visitor vector-c-visitor
  [visit-if visit-if/vector-c]
  [visit-op_ visit-op_/vector-c]
  [visit-call visit-op_/vector-c]
  [visit-number visit-number/vector-c]
  [visit-constant visit-constant/vector-c])

;; TODO: Remove or import
(define (binary80->string x)
  (parameterize ([gfl-exponent 15] [gfl-bits 80])
    (let ([s (gfl->string (gfl x))])
      (if (string-contains? s ".") s (string-append s ".0")))))

(define (operator->vector-c out operator arguments context)
  (define precision (ctx-lookup-prop context ':precision))
  (define type (type->vector-c precision))
  (define suffix (precision->suffix precision))
  (define assign "~a = ~a")
  (match operator
    [(or '== '!= '< '> '<= '>=)
     (define immediate 
       (match operator
         ['== "0x10"]
         ['!= "0x1C"]
         ['< "0x1"]
         ['<= "0x2"]
         ['> "0xE"]
         ['>= "0xD"]))
     (define comparisons
       (if (equal? operator '!=)
           ;; If the operator is `!=`, we need to compare each element with every
           ;; other. What we want is the Cartesian product but with unordered
           ;; instead of ordered pairs; and `(combinations <list> 2)` will give us this.
           (combinations arguments 2)
           ;; For the other operators, comparing elements stepwise (first with second,
           ;; second with third, ...) and taking the conjunction will do the right thing.
           (map list (drop-right arguments 1) (rest arguments))))
     (define arguments*
       (map
        (match-lambda [(list a b) (format "_mm256_cmp_~a(~a, ~a, ~a)" suffix a b immediate)])
        comparisons))
     (format assign out (operator-nary->binary "and" suffix arguments*))]
    ['and (format assign (operator-nary->binary "and" suffix arguments))]
    ['or (format assign (operator-nary->binary "or" suffix arguments))]
    ['not
     (format "~a = ((~a) _mm256_xor_si256((__m256i) ~a, _mm256_set1_epi64x(-1LL)))" out type (first arguments))]
    [_ 
     ;; - TODO: Missing: ;; cbrt, pow, hypot, exp, exp2, exp10, expm1, log, ...
     ;; - `round` needs attention, see `round_pd` intrinsic
     ;; - abs?
     ;; - What is fdim? 
     (define name
       (match operator
         ['+ "add"]
         ['- "sub"]
         ['* "mul"]
         ['/ "div"]
         ['sqrt "sqrt"]
         ['ceil "ceil"]
         ['floor "floor"]
         ['fmax "max"]
         ['fmin "min"]
         ['fmax "min"]))
     (format "_mm256_~a_~a(~a)"
             name (precision->suffix precision)
             (string-join arguments ", "))]))

(define (constant->vector-c variable context)
  (define precision (ctx-lookup-prop context ':precision))
  (define c-type (type->c precision))
  (format
   "_mm256_set1_~a(~a)"
   (precision->suffix precision)
   (match variable
     [(or 'TRUE 'FALSE (? hex?)) (~a variable)]
     [(or 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN)
      (format "((~a) ~a)" c-type variable)]
     [(? symbol?) (format "((~a) M_~a")]
     [(? number?)
      (define c-type-suffix (c-type->suffix c-type))
      (match c-type
        ["int64_t" (~a (inexact->exact variable))]
        ["long double" (format "~a~a" (binary80->string variable) c-type-suffix)]
        [_ (format "~a~a" (real->double-flonum variable) (c-type->suffix c-type))])])))

(define (operator-nary->binary name suffix arguments)
  (for/fold ([l (car arguments)]) ([r (cdr arguments)])
    (format "_mm256_~a_~a(~a, ~a)" suffix l r)))

(define/match (c-type->suffix type)
  [("int64_t") ""]
  [("double") ""]
  [("float") "f"]
  [("long double") "l"])

(define/match (type->c type)
  [('binary64) "double"]
  [('binary32) "float"]
  [('binary80) "long double"]
  [('boolean) "int"]
  [('integer) "int64_t"])

(define (precision->suffix type)
  (match type
    ['binary64 "pd"]
    ['binary32 "ps"]))
  
(define (type->vector-c type)
  (match type
    ['binary64 "__m256d"]
    ['binary32 "__m256"]))

(define (round->vector-c variable context)
  (define type (type->vector-c (ctx-lookup-prop context ':precision)))
  (format "((~a) ~a)" type variable))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 3]
    [('binary32) 2]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->vector-c operator argument argument-context context)
  (define precision (ctx-lookup-prop context ':precision))
  (define argument-precision (ctx-lookup-prop argument-context ':precision))
  (if (and (set-member? '(+ - * /) operator)
           (> (cmp-prec precision argument-precision) 0))
      (round->vector-c argument context)
      argument))

(define (round-mode->vector-c mode context)
  (define indent (ctx-lookup-extra context 'indent))
  (format "~afesetround(~a);\n" indent
    (match mode
     ['nearestEven  "FE_TONEAREST"]
     ['toPositive   "FE_UPWARD"]
     ['toNegative   "FE_DOWNWARD"]
     ['toZero       "FE_TOWARDZERO"]
     [_             (error 'round-mode->c (format "Unsupported rounding mode ~a" mode))])))

(define (parameters->vector-c arguments argument-contexts)
  (string-join
    (for/list ([argument (in-list arguments)] [context (in-list argument-contexts)])
      (let ([type (type->vector-c (ctx-lookup-prop context ':precision))])
        (format "~a ~a" type argument)))
    ", "))

(define (program->vector-c name arguments argument-contexts
                           body return-value context bound-variables)
  ;; TODO: Handle different rounding modes
  (define type (type->vector-c (ctx-lookup-prop context ':precision)))
  (define round-mode (ctx-lookup-prop context ':round))
  (match round-mode
    ['nearestEven
     (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
             type name (parameters->vector-c arguments argument-contexts)
             body (trim-infix-parens return-value))]
    [_
     (define-values (_ return-value) (ctx-random-name context))
     (format "~a ~a(~a) {\n~a~a~a ~a = ~a;\n~a\treturn ~a;\n}\n"
             type name (parameters->vector-c arguments argument-contexts)
             (round-mode->vector-c round-mode context) body
             type return-value (trim-infix-parens return-value)
             (round-mode->vector-c 'nearestEven context) return-value)]))

;; List of reserved variable names
(define vector-c-reserved
  '(auto break case char const continue default
         do double else enum extern float for goto if
         inline int long register restrict return
         short signed sizeof static struct switch
         typedef union unsigned void volatile while))

(define vector-c-header
  (const "#include <immintrin.h>"))

(define core->vector-c
  (make-imperative-compiler "vector-c"
    #:operator operator->vector-c
    #:infix-ops null
    #:constant constant->vector-c
    #:type type->vector-c
    #:round round->vector-c
    #:implicit-round implicit-round->vector-c
    #:round-mode round-mode->vector-c
    #:program program->vector-c
    #:visitor vector-c-visitor
    #:reserved vector-c-reserved))
