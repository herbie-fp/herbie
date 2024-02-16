#lang racket

(require fpbench/src/imperative)

(provide core->c c-header)

(define c-header
  (let ([header
         (string-join
           (list "#include <fenv.h>"
                 "#include <math.h>"
                 "#include <stdint.h>"
                 "#include <immintrin.h>"
                 "#include <mkl.h>"
                 "#define TRUE 1"
                 "#define FALSE 0"
                 ""
                 "typedef union {"
                 "double d[4];"
                 "float f[8];"
                 "__m256 vs;"
                 "__m256d vd;"
                 "} v256;"
                 "")
            "\n")])
    (λ _ header)))

(define c-supported
  (supported-list
    (invert-op-proc (curry set-member? '(array dim size ref for for* tensor tensor*)))
    fpcore-consts
    (curry set-member? '(binary32 binary64))
    (invert-rnd-mode-proc (curry equal? 'nearestAway))
    #f))

(define c-reserved  ; Language-specific reserved names (avoid name collisions)
  '(auto break case char const continue default
    do double else enum extern float for goto if
    inline int long register restrict return
    short signed sizeof static struct switch
    typedef union unsigned void volatile while))

(define/match (type->suffix type)
  [("binary32") "f"]
  [(_) ""])

(define/match (type->c type)
  [('binary64) "v256"]
  [('binary32) "v256"]
  [('boolean) "v256"])

(define ((mkl-formatter fn-name) prec args out)
  (define arg-format
    (match prec
      ['binary64 "~a.d"]
      ['binary32 "~a.f"]
      [_ (error 'mkl-formatter "unimplemented ~a" prec)]))
  (format "v~a~a(~a, ~a, ~a)"
          (match prec ['binary64 "d"] ['binary32 "s"])
          fn-name
          (match prec ['binary64 4] ['binary32 8])
          (string-join (map (curry format arg-format) args) ", ")
          (format arg-format out)))

(define (comparator->mkl op args ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define instr
    (match prec
      ['binary64 "_mm256_cmp_pd"]
      ['binary32 "_mm256_cmp_ps"]
      [_ (error 'comparator->mkl "unsupported comparator type ~a" prec)]))
  (define cmp-imm
    (match op
      ['== "0x10"]
      ['!= "0x1C"]
      ['< "0x1"]
      ['<= "0x2"]
      ['> "0xE"]
      ['>= "0xD"]))
  (match (cons op args)
    [(list (or '== '< '> '<= '>=) arg args ...)
     (logical->mkl 'and
                   (for/list ([a (cons arg args)] [b args])
                     (format "~a(~a, ~a, ~a)" instr a b cmp-imm))
                   ctx)]
    [(list '!= args ...)
     (logical->mkl 'and
                   (let loop ([args args])
                     (cond
                       [(null? args) '()]
                       [else (append
                               (for/list ([b (cdr args)])
                                 (format "~a(~a, ~a, ~a)" instr (car args) b cmp-imm))
                               (loop (cdr args)))]))
                   ctx)]))

(define (logical->mkl op args ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match (cons op args)
    [(list 'not a)
     (define formatter
       (match prec
         ['binary64 "((__m256d) _mm256_xor_si256((__m256i) ~a, _mm256_set1_epi64x(-1LL)))"]
         ['binary32 "((__m256) _mm256_xor_si256((__m256i) ~a, _mm256_set1_epi64x(-1LL)))"]
         [_ (error 'logical->mkl "unsupported type ~a" prec)]))
     (format formatter a)]
    [(list (or 'or 'and) a)
     a]
    [(list 'and arg args ...)
     (define instr
       (match prec
         ['binary64 "_mm256_and_pd"]
         ['binary32 "_mm256_and_ps"]
         [_ (error 'logical->mkl "unsupported type ~a" prec)]))
     (for/fold ([lhs arg]) ([rhs args])
       (format "~a(~a, ~a)" instr lhs rhs))]
    [(list 'or arg args ...)
     (define instr
       (match prec
         ['binary64 "_mm256_or_pd"]
         ['binary32 "_mm256_or_ps"]
         [_ (error 'logical->mkl "unsupported type ~a" prec)]))
     (for/fold ([lhs arg]) ([rhs args])
       (format "~a(~a, ~a)" instr lhs rhs))]))

(define (type->vec-accessor prec)
  (match prec
    ['binary64 "~a.vd"]
    ['binary32 "~a.vs"]
    [_ (error 'vec-accessor "unsupported ~a" prec)]))

(define (operator->mkl out op args ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match (cons op args)
    [(list (or '== '!= '< '> '<= '>=) _ ...)
     (define arg-format (type->vec-accessor prec))
     (define args* (map (curry format arg-format) args))
     (define expr (comparator->mkl op args* ctx))
     (format "~a = (~a) ~a" out (type->c prec) expr)]
    [(list (or 'not 'and 'or) _ ...)
     (define arg-format (type->vec-accessor prec))
     (define args* (map (curry format arg-format) args))
     (define expr (logical->mkl op args* ctx))
     (format "~a = (~a) ~a" out (type->c prec) expr)]
    [(list '- a)
     (define zero (format "((~a) ~a)" (type->c prec) (constant->c 0 ctx)))
     (define formatter (mkl-formatter "Sub"))
     (formatter prec (list zero a) out)]
    [_
     (define formatter
       (mkl-formatter
         (match op
           ['recip "Inv"]
           ['rsqrt "InvSqrt"]
           ['+ "Add"]
           ['- "Sub"]
           ['* "Mul"]
           ['/ "Div"]
           ['sqrt "Sqrt"]
           ['cbrt "Cbrt"]
           ['pow "Pow"]
           ['hypot "Hypot"]
           ['exp "Exp"]
           ['exp2 "Exp2"]
           ['exp10 "Exp10"]
           ['expm1 "Expm1"]
           ['log "Ln"]
           ['log2 "Log2"]
           ['log10 "Log10"]
           ['log1p "Log1p"]
           ['cos "Cos"]
           ['sin "Sin"]
           ['cos "Cos"]
           ['tan "Tan"]
           ['asin "Asin"]
           ['acos "Acos"]
           ['atan "Atan"]
           ['atan2 "Atan2"]
           ['sinh "Sinh"]
           ['cosh "Cosh"]
           ['tanh "Tanh"]
           ['asinh "Asinh"]
           ['acosh "Acosh"]
           ['atanh "Atanh"])))
     (formatter prec args out)]))

(define (operator->c op args ctx)
  (error 'operator->c "bad ~a ~a ~a" op args ctx))

(define (type->scalar-type prec)
  (match prec
    ['binary64 "double"]
    ['binary32 "float"]
    [_ (error 'type->scalar-type "unimplemented ~a" prec)]))

(define (constant->c x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match x
    [(or 'TRUE 'FALSE)
     (unless (member prec '(binary64 binary32))
       (error 'constant->c "unsupported context for boolean constant ~a" prec))
     (format "((~a) ((__m256) ~a))"
             (type->c prec)
             (match x
               ['TRUE "_mm256_set1_epi64x(-1LL)"]
               ['FALSE "_mm256_set1_epi64x(0LL)"]))]
    [_
     (define scalar
       (match x
         [(or (? hex?) 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN)
          (format "((~a) ~a)" (type->scalar-type prec) x)]
         [(? number?)
          (format "~a~a" (real->double-flonum x) (type->suffix prec))]
         [(? symbol?)
          (format "((~a) M_~a)" (type->scalar-type prec) x)]))
      (match prec
        ['binary32
         (format "{ ~a }" (string-join
                            (for/list ([_ (in-range 8)]) scalar)
                            ", "))]
        ['binary64
         (format "{ ~a }" (string-join
                            (for/list ([_ (in-range 4)]) scalar)
                            ", "))]
        [_
         (error 'constant->c "unimplemented ~a" prec)])]))
  
(define (round->c x ctx)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (format "((~a) ~a)" type x))

(define (cmp-prec prec1 prec2)
  (define/match (prec->num prec)
    [('binary64) 3]
    [('binary32) 2]
    [('boolean)  0])
  (- (prec->num prec1) (prec->num prec2)))

(define (implicit-round->c op arg arg-ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
  (if (set-member? '(+ - * /) op)
      (if (> (cmp-prec prec arg-prec) 0)
          (round->c arg ctx)
          arg)  ; TODO: warn unfaithful
      arg))

(define (round-mode->c mode ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (format "~afesetround(~a);\n" indent
    (match mode
     ['nearestEven  "FE_TONEAREST"]
     ['toPositive   "FE_UPWARD"]
     ['toNegative   "FE_DOWNWARD"]
     ['toZero       "FE_TOWARDZERO"]
     [_             (error 'round-mode->c "Unsupported rounding mode ~a" mode)])))

(define (params->c args arg-ctxs)
  (string-join
    (for/list ([arg (in-list args)] [ctx (in-list arg-ctxs)])
      (let ([type (type->c (ctx-lookup-prop ctx ':precision))])
        (format "~a ~a" type arg)))
    ", "))

(define (program->c name args arg-ctxs body ret ctx used-vars)
  (define type (type->c (ctx-lookup-prop ctx ':precision)))
  (define rnd-mode (ctx-lookup-prop ctx ':round))
  (match rnd-mode
   ['nearestEven
    (format "~a ~a(~a) {\n~a\treturn ~a;\n}\n"
            type name (params->c args arg-ctxs)
            body (trim-infix-parens ret))]
   [_
    (define-values (_ ret-var) (ctx-random-name ctx))
    (format "~a ~a(~a) {\n~a~a~a ~a = ~a;\n~a\treturn ~a;\n}\n"
            type name (params->c args arg-ctxs)
            (round-mode->c rnd-mode ctx) body
            type ret-var (trim-infix-parens ret) 
            (round-mode->c 'nearestEven ctx) ret-var)]))

(define (visit-op_/c vtor op args #:ctx ctx)
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
  (printf "~a~a ~a;\n" indent (type->c prec) name)
  (printf "~a~a;\n" indent (operator->mkl name op args* ctx))
  (values name ctx*))

(define (visit-number/c vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->c prec) name (constant->c x ctx))
  (values name name-ctx))

(define (visit-constant/c vtor x #:ctx ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define-values (name-ctx name) (ctx-random-name ctx prec))
  (printf "~a~a ~a = ~a;\n" indent (type->c prec) name (constant->c x ctx))
  (values name 
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props name-ctx (list ':precision 'boolean))
              name-ctx)))

(define (visit-if/c vtor cond ift iff #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define prec (ctx-lookup-prop ctx ':precision))
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ift-ctx))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond ctx))
  (define-values (ctx* name) (ctx-random-name iff-ctx prec))
  (define type (type->c prec))
  (define arg-format (type->vec-accessor prec))
  (printf "~a~a ~a = (~a) ~a(~a, ~a, ~a);\n"
          indent type name type
          (match prec
            ['binary64 "_mm256_blendv_pd"]
            ['binary32 "_mm256_blendv_ps"]
            [_ (error 'visit-if/c
                      "unimplemented if" 
                      (list 'if cond ift iff))])
          (format arg-format iff*) ; we need to flip these since 0 means first argument
          (format arg-format ift*)
          (format arg-format cond*))
  (values name ctx*))

(define-expr-visitor imperative-visitor mkl-visitor
  [visit-if visit-if/c]
  [visit-op_ visit-op_/c]
  [visit-call visit-op_/c]
  [visit-number visit-number/c]
  [visit-constant visit-constant/c])

(define core->c
  (make-imperative-compiler "c"
    #:operator operator->c
    #:constant constant->c
    #:type type->c
    #:round round->c
    #:implicit-round implicit-round->c
    #:round-mode round-mode->c
    #:program program->c
    #:visitor mkl-visitor
    #:reserved c-reserved))

(module+ test
  (define exprs
    '((FPCore (x y) (if (== x y) 1 0))
      (FPCore (x y) (if (or (< x y) (> x y)) 1 0))
      (FPCore () E)
      (FPCore () (if TRUE 1 0))))

  (define compiled
    (map (curryr core->c "test") exprs)) 

  (for-each displayln compiled)
  (void))
