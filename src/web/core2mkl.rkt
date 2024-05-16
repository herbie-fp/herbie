#lang racket

(require fpbench/src/imperative
         fpbench/src/fpcore-visitor)

(provide core->mkl mkl-header)

;; --------------------------------------------------------
;; Utility

(define mkl-header
  (let ([header
         (string-join
           (list "#include <fenv.h>"
                 "#include <math.h>"
                 "#include <stdint.h>"
                 "#include <immintrin.h>"
                 "#include <mkl.h>"
                 "#define TRUE 1"
                 "#define FALSE 0"
                 "")
            "\n")])
    (λ _ header)))

(define mkl-reserved  ; Language-specific reserved names (avoid name collisions)
  '(auto break case char const continue default
    do double else enum extern float for goto if
    inline int long register restrict return
    short signed sizeof static struct switch
    typedef union unsigned void volatile while))

(define default-ctx
  (ctx-update-props
    (make-compiler-ctx)
    '(:precision binary64 :round nearestEven)))

(define (fix-name name)
  (unless (non-empty-string? name)
    (error 'fix-name "must be a non-empty string" name))
  (define name*
    (apply string-append
           (for/list ([char (~a name)])
             (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
                 (string char)
                 (format "_~a_" (char->integer char))))))
  ; can't have a leading number
  (if (regexp-match #rx"[0-9]" (string (string-ref name* 0)))
      (string-append "_" name*)
      name*))

(define (core-unpack core)
  (match core
    [(list 'FPCore (list args ...) props ... body)
     (values args props body)]
    [(list 'FPCore _ (list args ...) props ... body)
     (values args props body)]))

;; --------------------------------------------------------
;; MKL compiler

(define/match (type->mkl _)
  [('binary64) "double"]
  [('binary32) "float"]
  [('integer) "int"]
  [('bool) "int"])

(define/match (type->suffix _)
  [('binary64) ""]
  [('binary32) "f"]
  [('integer) ""]
  [('bool) ""])

(define (constant->mkl x ctx)
  (define prec (ctx-lookup-prop ctx ':precision))
  (match x
    [(or 'TRUE 'FALSE) x]
    [(or (? hex?) 'M_1_PI 'M_2_PI 'M_2_SQRTPI 'INFINITY 'NAN)
     (format "((~a) ~a)" (type->mkl prec) x)]
    [(? number?)
     (format "~a~a" (real->double-flonum x) (type->suffix prec))]
    [(? symbol?)
     (format "((~a) M_~a)" (type->mkl prec) x)]))

(define ((logical-formatter ctx op args) iter)
  (define args* (map (lambda (x) (format "~a[~a]" x iter)) args))
  (match (cons op args*)
    [(list 'not a) (format "!~a" a)]
    [(list (or '== '!= '< '> '<= '>=)) (constant->mkl 'TRUE ctx)]
    [(list (or '== '< '> '<= '>=) arg args ...)
     (format "(~a)"
             (string-join
               (for/list ([a (cons arg args)] [b args])
                 (format "~a ~a ~a" a op b))
               " && "))]
    [(list '!= args ...)
     (format "(~a)"
              (string-join
                (let loop ([args args])
                  (if (null? args)
                      '()
                      (append
                        (for/list ([b (cdr args)])
                          (format "~a != ~a" (car args) b))
                        (loop (cdr args)))))
                " && "))]
   [(list 'and a ...)
    (format "(~a)" (string-join (map ~a a) " && "))]
   [(list 'or a ...)
    (format "(~a)" (string-join (map ~a a) " || "))]))

(define ((op-formatter fn-name) ctx args out)
  (define prec (ctx-lookup-prop ctx ':precision))
  (define size (ctx-lookup-extra ctx 'size))
  (format "v~a~a(~a, ~a, ~a)"
          (match prec ['binary64 "d"] ['binary32 "s"])
          fn-name
          size
          (string-join args ", ")
          out))

(define (operator->mkl ctx op args out)
  (define formatter
    (op-formatter
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
        ['atanh "Atanh"]
        ['ceil "Ceil"]
        ['floor "Floor"]
        ['trunc "Trunc"]
        ['round "Round"]
        ['rint "Rint"]
        ['logb "Logb"]
        ['lgamma "LGamma"]
        ['tgamma "TGamma"]
        ['fabs "Abs"]
        ['fdim "Fdim"]
        ['fmax "Fmax"]
        ['fmin "Fmin"]
        ['erf "Erf"]
        ['erfc "Erfc"]
        ['hypot "Hypot"]
        ['fmod "Fmod"]
        ['copysign "CopySign"]
        ['remainder "Remainder"])))
  (formatter ctx args out))

(define (decl->mkl ctx [id #f])
  (define prec (ctx-lookup-prop ctx ':precision))
  (define indent (ctx-lookup-extra ctx 'indent))
  (define size (ctx-lookup-extra ctx 'size))
  (define-values (ctx* name)
    (if id
        (ctx-unique-name ctx id prec)
        (ctx-random-name ctx prec)))
  (define scalar-type (type->mkl prec))
  (printf "~a~a *~a = malloc(~a * sizeof(~a));\n"
          indent scalar-type name size scalar-type)
  (values ctx* name))

(define (copy->mkl ctx dst src-formatter)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define size (ctx-lookup-extra ctx 'size))
  (define-values (ctx* iter) (ctx-unique-name ctx 'i 'integer))
  (define iter-type (type->mkl 'integer))
  (printf "~afor(~a ~a = 0; ~a < ~a; ~a++) {\n" 
          indent iter-type iter iter size iter)
  (printf "~a\t~a[~a] = ~a;\n" indent dst iter (src-formatter iter))
  (printf "~a}\n" indent)
  ctx*)

(define (program->mkl name size-name out-name args body ret ctx)
  (define rnd-mode (ctx-lookup-prop ctx ':round))
  (unless (eq? rnd-mode 'nearestEven)
    (error 'program->mkl "unsupported rounding mode: ~a" rnd-mode))
  (define p (open-output-string))
  (parameterize ([current-output-port p])
    (printf "void ~a(~a) {\n"
            name
            (string-join
            `(,(format "const ~a ~a"
                       (type->mkl (ctx-lookup-prec ctx size-name))
                       size-name)
              ,@(for/list ([arg (in-list args)])
                  (define type (type->mkl (ctx-lookup-prec ctx arg)))
                    (format "const ~a *~a" type arg))
              ,(format "~a *~a"
                       (type->mkl (ctx-lookup-prop ctx ':precision))
                       out-name))
            ", "))
    (printf "~a" body)
    (copy->mkl ctx out-name (lambda (iter) (format "~a[~a]" ret iter)))
    (printf "}\n")
    (get-output-string p)))

;; --------------------------------------------------------
;; MKL compiler visitor

(define (visit-if/mkl vtor cond ift iff #:ctx ctx)
  (define-values (ift* ift-ctx) (visit/ctx vtor ift ctx))
  (define-values (iff* iff-ctx) (visit/ctx vtor iff ift-ctx))
  (define-values (cond* cond-ctx) (visit/ctx vtor cond iff-ctx))
  (define-values (ctx* name)
    (let ([prec (ctx-lookup-prop ctx ':precision)])
      (decl->mkl (ctx-update-props cond-ctx `(:precision ,prec)))))
  (define ctx** (copy->mkl ctx* name
                           (lambda (iter)
                             (format "(~a[~a] ? ~a[~a] : ~a[~a])"
                                     cond* iter ift* iter iff* iter))))
  (values name ctx**))

(define (visit-let_/mkl vtor let_ vars vals body #:ctx ctx)
  (define indent (ctx-lookup-extra ctx 'indent))
  (define ctx*
    (for/fold ([ctx* ctx]) ([var (in-list vars)] [val (in-list vals)])
      (define-values (val* val-ctx) (visit/ctx vtor val (match let_ ['let ctx] ['let* ctx*])))
      (define prec (ctx-lookup-prop val-ctx ':precision))
      (define-values (name-ctx name) (ctx-unique-name ctx* var prec))
      (printf (match prec
                [(or 'binary64 'binary32) "~a~a *~a = ~a;\n"]
                [_ "~a~a ~a = ~a;\n"])
              indent
              (type->mkl prec)
              name
              val*)
      name-ctx))
  (visit/ctx vtor body ctx*))

(define (visit-op_/mkl vtor op args #:ctx ctx)
  (match (cons op args)
    [(list '- x)
     (visit-op_/mkl vtor '- (list 0 x) #:ctx ctx)]
    [_
     (define indent (ctx-lookup-extra ctx 'indent))
     (define args*
       (for/list ([arg args])
         (define-values (arg* _) (visit/ctx vtor arg ctx))
         arg*))
     (define-values (ctx* name)
       (decl->mkl (if (set-member? bool-ops op)
                      (ctx-update-props ctx '(:precision integer))
                      ctx)))
     (cond
       [(member op '(== != < > <= >= not and or))
        (define src-formatter (logical-formatter ctx op args*))
        (define ctx** (copy->mkl ctx* name src-formatter))
        (values name ctx**)]
       [else
        (printf "~a~a;\n" indent (operator->mkl ctx op args* name))
        (values name ctx*)])]))

(define (visit-digits/mkl vtor m e b #:ctx ctx)
  (visit/ctx vtor (digits->number m e b) ctx))

(define (visit-constant/mkl vtor x #:ctx ctx)
  (define-values (ctx* name)
    (decl->mkl (if (set-member? '(TRUE FALSE) x)
                   (ctx-update-props ctx '(:precision integer))
                   ctx)))
  (define ctx** (copy->mkl ctx* name (lambda (_) (constant->mkl x ctx))))
  (values name
          (if (set-member? '(TRUE FALSE) x)
              (ctx-update-props ctx** (list ':precision 'boolean))
              ctx**)))

(define (visit-symbol/mkl vtor x #:ctx ctx)
  (define name (ctx-lookup-name ctx x))
  (define var-prec (ctx-lookup-prec ctx name))
  (values name (ctx-update-props ctx `(:precision ,var-prec))))

(define-expr-visitor default-compiler-visitor mkl-visitor
  [visit-if visit-if/mkl]
  [visit-let_ visit-let_/mkl]
  [visit-op_ visit-op_/mkl]
  [visit-digits visit-digits/mkl]
  [visit-number visit-constant/mkl]
  [visit-constant visit-constant/mkl]
  [visit-symbol visit-symbol/mkl])

;; --------------------------------------------------------
;; Public API

(define (core->mkl prog name)
  (parameterize ([*gensym-used-names* (mutable-set)] 
                 [*gensym-collisions* 1]
                 [*gensym-fix-name* fix-name])
    (define-values (args props body) (core-unpack prog))
    (define ctx
      (let ([ctx0 (ctx-update-props default-ctx props)])
        (let ([ctx1 (ctx-reserve-names ctx0 mkl-reserved)])
          (ctx-set-extra ctx1 'indent "\t"))))

    ; compiled function name
    (define fname
      (let-values ([(cx fname) (ctx-unique-name ctx name)])
        (begin0 fname (set! ctx cx))))

    ; output vector & size
    (define-values (size-name out-name)
      (let-values ([(ctx* size-name) (ctx-unique-name ctx 'n 'integer)])
        (let-values ([(ctx* out-name) (ctx-unique-name ctx* 'out
                                        (ctx-lookup-prop ctx ':precision))])
          (let ([ctx* (ctx-set-extra ctx* 'output out-name)])
            (let ([ctx* (ctx-set-extra ctx* 'size size-name)])
              (set! ctx ctx*)
              (values size-name out-name))))))

    ; compiled argument names
    (define arg-names
      (for/list ([arg (in-list args)])
        (define-values (props name)
          (match arg
            [(list '! props ... name) (values props name)]
            [name (values '() name)]))
        (define arg-ctx (ctx-update-props ctx props))
        (define arg-prec (ctx-lookup-prop arg-ctx ':precision))
        (define-values (ctx* name*) (ctx-unique-name ctx name arg-prec))
        (set! ctx ctx*)
        name*))
    
    ; compile the body
    (define p (open-output-string))
    (define-values (body* ret)
      (parameterize ([current-output-port p])
        (define-values (o _) (visit/ctx mkl-visitor body ctx))
        (values (get-output-string p)
                (trim-infix-parens o))))
    
    ; compose everything
    (program->mkl fname size-name out-name arg-names body* ret ctx)))

(module+ test
  (define exprs
    '((FPCore (x y) (if (== x y) 1 0))
      (FPCore (x y) (if (or (< x y) (> x y)) 1 0))
      (FPCore () (if TRUE 1 0))
      (FPCore (x) (let ([x (+ x 1)]) (- x 1)))
      (FPCore (x) (+ x x))
      (FPCore () E)))

  (define compiled
    (map (curryr core->mkl "test") exprs)) 

  (for-each displayln compiled)
  (void))
