#lang racket

(require "../common.rkt")
(require "../programs.rkt")
(require "../reports/datafile.rkt")
(require net/uri-codec)

(provide compile-info)

(define (fix-name name)
  (string-replace (uri-encode (~a name)) #rx"[^a-zA-Z0-9]" "_"))

(define (apply-converter conv args)
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply format (list-ref conv (length args)) args)]
   [(procedure? conv) (apply conv args)]
   [else (error "Unknown syntax entry" conv)]))

(define-table operators->c
  [+  "~a + ~a"]
  [-  '(#f "-~a" "~a - ~a")]
  [*  "~a * ~a"]
  [/  '(#f "1.0/~a" "~a / ~a")]

  [sqr    (lambda (x) (format "~a * ~a" x x))]
  [cube   (lambda (x) (format "~a * (~a * ~a)" x x x))]
  [cotan  "1.0 / tan(~a)"]

  [acos       "acos(~a)"]
  [acosh      "acosh(~a)"]
  [asin       "asin(~a)"]
  [asinh      "asinh(~a)"]
  [atan       "atan(~a)"]
  [atan2      "atan2(~a, ~a)"]
  [atanh      "atanh(~a)"]
  [cbrt       "cbrt(~a)"]
  [ceil       "ceil(~a)"]
  [copysign   "copysign(~a, ~a)"]
  [cos        "cos(~a)"]
  [cosh       "cosh(~a)"]
  [erf        "erf(~a)"]
  [erfc       "erfc(~a)"]
  [exp        "exp(~a)"]
  [exp2       "exp2(~a)"]
  [expm1      "expm1(~a)"]
  [abs        "fabs(~a)"]
  [fdim       "fdim(~a, ~a)"]
  [floor      "floor(~a)"]
  [fma        "fma(~a, ~a, ~a)"]
  [fmax       "fmax(~a, ~a)"]
  [fmin       "fmin(~a, ~a)"]
  [mod        "fmod(~a, ~a)"]
  [hypot      "hypot(~a, ~a)"]
  [log        "log(~a)"]
  [log10      "log10(~a)"]
  [log1p      "log1p(~a)"]
  [log2       "log2(~a)"]
  [expt       "pow(~a, ~a)"]
  [remainder  "remainder(~a, ~a)"]
  [round      "round(~a)"]
  [sin        "sin(~a)"]
  [sinh       "sinh(~a)"]
  [sqrt       "sqrt(~a)"]
  [tan        "tan(~a)"]
  [tanh       "tanh(~a)"]
  [trunc      "trunc(~a)"]

  [if   "~a ? ~a : ~a"]
  [=    "~a == ~a"]
  [>    "~a > ~a"]
  [<    "~a < ~a"]
  [>=   "~a >= ~a"]
  [<=   "~a <= ~a"]
  ; TODO what should not be?
  [and  "~a && ~a"]
  [or   "~a || ~a"])


(define-table constants->c
  [pi    "atan2(1.0, 0.0)"]
  [e     "exp(1.0)"])

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (if? l)
  (and (list? l) (eq? (car l) 'if)))

(define (program->c prog [type "double"] [fname "f"])
  (define vars (program-variables prog))
  (define unused-vars (unused-variables prog))
  (define body (compile (program-body prog)))

  (define (value->c expr)
    (cond
     [(member expr vars) (fix-name expr)]
     [(member expr constants) (apply-converter (car (hash-ref constants->c expr)) '())]
     [(symbol? expr) expr]
     [else (real->double-flonum (->flonum expr))]))

  (define (app->c expr)
    (if (list? expr)
        (let* ([rec (list-ref (hash-ref operators->c (car expr)) 0)]
               [args (map value->c (cdr expr))])
          (apply-converter rec args))
        (value->c expr)))

  (write-string
    (let ([pdecls
            (for/list ([var vars])
                      (if (member var unused-vars)
                        (format "~a __attribute__((unused)) ~a" type (fix-name var))
                        (format "~a ~a" type (fix-name var))))])
      (printf "double ~a(~a) {\n" ; TODO shouldn't "double" here be type ?
              fname
              (string-join pdecls ", ")))

   (for/list ([assignment (cadr body)])
     (if (comparison? (cadr assignment))
         (printf "        bool ~a = ~a;\n" (car assignment)
                 (app->c (cadr assignment)))
         (printf "        ~a ~a = ~a;\n" type (car assignment)
                 (app->c (cadr assignment)))))

   (printf "        return ~a;\n" (value->c (caddr body)))
   (printf "}\n\n")))

(define-table operators->mpfr
  [+  "mpfr_add(~a, ~a, ~a, MPFR_RNDN)"]
  [-  '(#f
        #f
        "mpfr_neg(~a, ~a, MPFR_RNDN)"
        "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)")]
  [*  "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)"]
  [/  '(#f
        #f
        "mpfr_ui_div(~a, 1, ~a, MPFR_RNDN)"
        "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]

  [sqr      "mpfr_sqr(~a, ~a, MPFR_RNDN)"]
  [cotan    "mpfr_cot(~a, ~a, MPFR_RNDN)"]
  [cube     (λ (x y) (string-append
              (format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN); "
                      x y y)
              (format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)"
                      x x y)))]

  [acos       "mpfr_acos(~a, ~a, MPFR_RNDN)"]
  [acosh      "mpfr_acosh(~a, ~a, MPFR_RNDN)"]
  [asin       "mpfr_asin(~a, ~a, MPFR_RNDN)"]
  [asinh      "mpfr_asinh(~a, ~a, MPFR_RNDN)"]
  [atan       "mpfr_atan(~a, ~a, MPFR_RNDN)"]
  [atan2      "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)"]
  [atanh      "mpfr_atanh(~a, ~a, MPFR_RNDN)"]
  [cbrt       "mpfr_cbrt(~a, ~a, MPFR_RNDN)"]
  [ceil       "mpfr_ceil(~a, ~a, MPFR_RNDN)"]
  [copysign   "mpfr_copysign(~a, ~a, ~a, MPFR_RNDN)"]
  [cos        "mpfr_cos(~a, ~a, MPFR_RNDN)"]
  [cosh       "mpfr_cosh(~a, ~a, MPFR_RNDN)"]
  [erf        "mpfr_erf(~a, ~a, MPFR_RNDN)"]
  [erfc       "mpfr_erfc(~a, ~a, MPFR_RNDN)"]
  [exp        "mpfr_exp(~a, ~a, MPFR_RNDN)"]
  [exp2       "mpfr_exp2(~a, ~a, MPFR_RNDN)"]
  [expm1      "mpfr_expm1(~a, ~a, MPFR_RNDN)"]
  [abs        "mpfr_abs(~a, ~a, MPFR_RNDN)"]
  [fdim       "mpfr_dim(~a, ~a, ~a, MPFR_RNDN)"]
  [floor      "mpfr_floor(~a, ~a, MPFR_RNDN)"]
  [fma        "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)"]
  [fmax       "mpfr_fmax(~a, ~a, ~a, MPFR_RNDN)"]
  [fmin       "mpfr_fmin(~a, ~a, ~a, MPFR_RNDN)"]
  [mod        "mpfr_fmod(~a, ~a, ~a, MPFR_RNDN)"]
  [hypot      "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)"]
  [log        "mpfr_log(~a, ~a, MPFR_RNDN)"]
  [log10      "mpfr_log10(~a, ~a, MPFR_RNDN)"]
  [log1p      "mpfr_log1p(~a, ~a, MPFR_RNDN)"]
  [log2       "mpfr_log2(~a, ~a, MPFR_RNDN)"]
  [expt       "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)"]
  [remainder  "mpfr_remainder(~a, ~a, ~a, MPFR_RNDN)"]
  [round      "mpfr_round(~a, ~a, MPFR_RNDN)"]
  [sin        "mpfr_sin(~a, ~a, MPFR_RNDN)"]
  [sinh       "mpfr_sinh(~a, ~a, MPFR_RNDN)"]
  [sqrt       "mpfr_sqrt(~a, ~a, MPFR_RNDN)"]
  [tan        "mpfr_tan(~a, ~a, MPFR_RNDN)"]
  [tanh       "mpfr_tanh(~a, ~a, MPFR_RNDN)"]
  [trunc      "mpfr_trunc(~a, ~a, MPFR_RNDN)"]



  [if       (lambda (r c a b) (string-append
               (format "if (mpfr_get_si(~a, MPFR_RNDN)) { " c)
               (format "mpfr_set(~a, ~a, MPFR_RNDN); " r a)
               "} else { "
               (format "mpfr_set(~a, ~a, MPFR_RNDN); " r b)
               "}"))]
  [>        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)"]
  [<        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)"]
  [>=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)"]
  [<=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)"]
  ; TODO what should not be?
  [and      (string-append
              "mpfr_set_si(~a, "
                "mpfr_get_si(~a, MPFR_RNDN) && "
                "mpfr_get_si(~a, MPFR_RNDN), "
              "MPFR_RNDN)")]
  [or       (string-append
              "mpfr_set_si(~a, "
                "mpfr_get_si(~a, MPFR_RNDN) || "
                "mpfr_get_si(~a, MPFR_RNDN), "
              "MPFR_RNDN)")])

(define-table constants->mpfr
  [pi    "mpfr_const_pi(~a, MPFR_RNDN)"]
  [e     (λ (x) (format "mpfr_set_si(~a, 1, MPFR_RNDN); mpfr_exp(~a, ~a, MPFR_RNDN);" x x x))])

(define (program->mpfr prog [bits 128] [fname "f"])
  (define vars (program-variables prog))
  (define unused-vars (unused-variables prog))
  (define body (compile (program-body prog)))

  (define (app->mpfr out expr)
    (cond
     [(list? expr)
      (let* ([rec (list-ref (hash-ref operators->mpfr (car expr)) 0)]
             [args (cdr expr)])
        (apply-converter rec (cons out args)))]
     [(number? expr) ""]
     [(member expr vars)
      (format "mpfr_set_d(~a, ~a, MPFR_RNDN)" out (fix-name expr))]
     [(member expr constants)
      (apply-converter (car (hash-ref constants->mpfr expr)) (list out))]
     [(symbol? expr)
      (format "mpfr_set(~a, ~a, MPFR_RNDN)" out expr)]))

  (write-string
   (printf "static mpfr_t ~a;\n\n" (string-join (map symbol->string (map car (cadr body))) ", "))

   (printf "void setup_mpfr_~a() {\n" fname)
   ; Some guard bits added, just in case
   (printf "        mpfr_set_default_prec(~a);\n" (+ bits 16))
   (for ([reg (cadr body)])
     (if (number? (cadr reg))
         (printf "        mpfr_init_set_str(~a, \"~a\", 10, MPFR_RNDN);\n" (car reg) (cadr reg))
         (printf "        mpfr_init(~a);\n" (car reg))))
   (printf "}\n\n")

   (let ([pdecls
           (for/list ([var vars])
                     (if (member var unused-vars)
                       (format "double __attribute__((unused)) ~a" (fix-name var))
                       (format "double ~a" (fix-name var))))])
     (printf "double ~a(~a) {\n"
             fname
             (string-join pdecls ", ")))

   (for ([assignment (cadr body)])
     (printf "        ~a;\n" (app->mpfr (car assignment) (cadr assignment))))


  (printf "        return mpfr_get_d(~a, MPFR_RNDN);\n" (caddr body))
  (printf "}\n\n")))

(define (compile-all name iprog fprog dprog bits)
  (printf "#include <tgmath.h>\n")
  (printf "#include <gmp.h>\n")
  (printf "#include <mpfr.h>\n")
  (printf "#include <stdio.h>\n")
  (printf "#include <stdbool.h>\n\n")
  (printf "char *name = \"~a\";\n\n" name)
  (display (program->c iprog "float" "f_if"))
  (display (program->c iprog "double" "f_id"))
  (newline)
  (display (program->c fprog "float" "f_of"))
  (display (program->c dprog "double" "f_od"))
  (printf "void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d, mpfr_rnd_t rmd) {\n")
  (printf "        mpfr_fmod(r, n, d, rmd);\n")
  (printf "        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, rmd);\n")
  (printf "}\n\n")
  (newline)
  (display (program->mpfr iprog bits "f_im"))
  (display (program->mpfr fprog bits "f_fm"))
  (display (program->mpfr dprog bits "f_dm")))

(define (compile-info base-dir single-info double-info)
  (for ([single-test (report-info-tests single-info)] [double-test (report-info-tests double-info)])
    (when (and (not (member (table-row-status single-test) '("timeout" "crash")))
               (not (member (table-row-status double-test) '("timeout" "crash"))))
      (match (cons single-test double-test)
        [(cons (table-row name single-status _ _ _ _ _ _ vars _ input single-output _ single-bits dir)
               (table-row name double-status _ _ _ _ _ _ vars _ input double-output _ double-bits dir))
         (define fname (build-path base-dir dir "compiled.c"))
         (debug #:from 'compile-info "Compiling" name "to" fname)
         (write-file fname
                     (compile-all name `(λ ,vars ,input) `(λ ,vars ,single-output)
                                  `(λ ,vars ,double-output) (max single-bits double-bits)))]
        [else
         (error "Test case order, names, inputs don't match for single and double precision results."
                single-test double-test)]))))

(module+ main
  (require racket/cmdline)
  (require "../config.rkt")

  (define dir report-output-path)

  (command-line
   #:program "compile/c.rkt"
   #:once-each
   [("-d") dir* "Report output directory"
    (set! dir dir*)]
   #:args (single-json-file double-json-file)
   (compile-info dir (read-datafile single-json-file) (read-datafile double-json-file))))
