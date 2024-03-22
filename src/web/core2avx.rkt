#lang racket

(require generic-flonum)
(require fpbench/src/imperative)

(provide core->avx)

;; TODO: There are a bunch of different ways to initialize vector constants. See https://www.agner.org/optimize/optimizing_assembly.pdf.

(define (visit-if/avx vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define prec (ctx-lookup-prop ctx ':precision))
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ift-ctx))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
  (define-values (ctx* name) (ctx-random-name iff-ctx prec))
  (define type (type->avx prec))
  (define suffix (precision->suffix prec))
  (printf "~a~a ~a = _mm256_blendv_~a(~a, ~a, ~a);\n"
          indent type name suffix
          ; we need to flip iff and ift since 0 means first argument
          iff* ift* cond*)
  (values name ctx*))

(define (visit-op_/avx vtor op args #:ctx ctx)
  (match (cons op args)
    [(list '- x)
     ;; TODO: Any better way to do this?
     (visit-op_/avx vtor '- (list 0 x) #:ctx ctx)]
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
     (printf "~a~a ~a = ~a;\n" indent (type->avx prec) name (operator->avx name op args* ctx))
     (values name ctx*)]))

(define (visit-number/avx vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->avx prec) name (constant->avx x ctx))
  (values name name-ctx))

(define (visit-constant/avx vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->avx prec) name (constant->avx x ctx))
  (values name 
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props name-ctx (list ':precision 'boolean))
              name-ctx)))

(define-expr-visitor imperative-visitor avx-visitor
  [visit-if visit-if/avx]
  [visit-op_ visit-op_/avx]
  [visit-call visit-op_/avx]
  [visit-number visit-number/avx]
  [visit-constant visit-constant/avx])

;; TODO: Remove or import
(define (binary80->string x)
  (parameterize ([gfl-exponent 15] [gfl-bits 80])
    (let ([s (gfl->string (gfl x))])
      (if (string-contains? s ".") s (string-append s ".0")))))

(define (operator->avx out operator arguments context)
  (define precision (ctx-lookup-prop context ':precision))
  (define type (type->avx precision))
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
    ['round
     (format "_mm256_round_~a(~a, ~a)"
             suffix
             (first arguments)
             (round-mode->avx* (ctx-lookup-prop context ':round)))]
    ['fabs
     (format "_mm256_andnot_~a(_mm256_set1_~a(-0.0), ~a)" suffix suffix (first arguments))]
    [_ 
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
         ['fmax "min"]
         ['fma "fmadd"]
         ['fmsub "fmsub"]
         ['fnmadd "fnmadd"]
         ['fnmsub "fnmsub"]))
     (format "_mm256_~a_~a(~a)" name suffix (string-join arguments ", "))]))

(define (constant->avx variable context)
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
  
(define (type->avx type)
  (match type
    ['binary64 "__m256d"]
    ['binary32 "__m256"]))

(define (round->avx variable context)
  (define type (type->avx (ctx-lookup-prop context ':precision)))
  (format "((~a) ~a)" type variable))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 3]
    [('binary32) 2]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->avx operator argument argument-context context)
  (define precision (ctx-lookup-prop context ':precision))
  (define argument-precision (ctx-lookup-prop argument-context ':precision))
  (if (and (set-member? '(+ - * /) operator)
           (> (cmp-prec precision argument-precision) 0))
      (round->avx argument context)
      argument))

(define (round-mode->avx mode context)
  (define indent (ctx-lookup-extra context 'indent))
  (format "~afesetround(~a);\n" indent
    (match mode
     ['nearestEven  "FE_TONEAREST"]
     ['toPositive   "FE_UPWARD"]
     ['toNegative   "FE_DOWNWARD"]
     ['toZero       "FE_TOWARDZERO"]
     [_             (error 'round-mode->avx (format "Unsupported rounding mode ~a" mode))])))

;; TODO: Change name
(define (round-mode->avx* mode)
  (match mode
    ;; TODO: Is this right?
    ['nearestEven "(_MM_FROUND_TO_NEAREST_INT |_MM_FROUND_NO_EXC)"]
    ['toPositive "(_MM_FROUND_TO_POS_INF |_MM_FROUND_NO_EXC)"]
    ['toNegative "(_MM_FROUND_TO_NEG_INF |_MM_FROUND_NO_EXC)"]
    ['toZero "(_MM_FROUND_TO_ZERO |_MM_FROUND_NO_EXC)"]
    [_ (error 'round-mode->avx* (format "Unsupported rounding mode ~a" mode))]))

(define (parameters->avx arguments argument-contexts)
  (string-join
    (for/list ([argument (in-list arguments)] [context (in-list argument-contexts)])
      (let ([type (type->avx (ctx-lookup-prop context ':precision))])
        (format "~a ~a" type argument)))
    ", "))

(define (program->avx name arguments argument-contexts
                           body return-value context bound-variables)
  ;; TODO: Do we even need to use fesetround?
  (define type (type->avx (ctx-lookup-prop context ':precision)))
  (define round-mode (ctx-lookup-prop context ':round))
  (match round-mode
    ['nearestEven
     (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
             type name (parameters->avx arguments argument-contexts)
             body (trim-infix-parens return-value))]
    [_
     (define-values (_ return-value) (ctx-random-name context))
     (format "~a ~a(~a) {\n~a~a~a ~a = ~a;\n~a\treturn ~a;\n}\n"
             type name (parameters->avx arguments argument-contexts)
             (round-mode->avx round-mode context) body
             type return-value (trim-infix-parens return-value)
             (round-mode->avx 'nearestEven context) return-value)]))

;; List of reserved variable names
(define avx-reserved
  '(auto break case char const continue default
         do double else enum extern float for goto if
         inline int long register restrict return
         short signed sizeof static struct switch
         typedef union unsigned void volatile while))

(define avx-header
  (const "#include <immintrin.h>"))

(define core->avx
  (make-imperative-compiler "avx"
    #:operator operator->avx
    #:infix-ops null
    #:constant constant->avx
    #:type type->avx
    #:round round->avx
    #:implicit-round implicit-round->avx
    #:round-mode round-mode->avx
    #:program program->avx
    #:visitor avx-visitor
    #:reserved avx-reserved))
