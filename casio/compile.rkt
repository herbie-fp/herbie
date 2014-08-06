#lang racket

(require casio/common)
(require casio/programs)
(require net/uri-codec)

(define (fix-name name)
  (string-replace (uri-encode (~a name)) #rx"[^a-zA-Z0-9]" "_"))

(define (apply-converter conv args)
  (cond
   [(string? conv) (apply format conv args)]
   [(list? conv) (apply format (list-ref conv (length args)) args)]
   [(procedure? conv) (apply conv args)]
   [else (error "Unknown syntax entry" conv)]))

(define-table operators->c
  [+        "~a + ~a"]
  [-        '(#f "-~a" "~a - ~a")]
  [*        "~a * ~a"]
  [/        '(#f "1.0/~a" "~a / ~a")]
  [abs      "fabs(~a)"]
  [sqrt     "sqrt(~a)"]
  [sqr      (λ (x) (format "~a * ~a" x x))]
  [exp      "exp(~a)"]
  [expt     "pow(~a, ~a)"]
  [log      "log(~a)"]
  [sin      "sin(~a)"]
  [cos      "cos(~a)"]
  [tan      "tan(~a)"]
  [cotan    "1.0 / tan(~a)"]
  [asin     "asin(~a)"]
  [acos     "acos(~a)"]
  [atan     "atan(~a)"]
  [sinh     "sinh(~a)"]
  [cosh     "cosh(~a)"]
  [tanh     "tanh(~a)"]
  [atan2    "atan2(~a, ~a)"]
  [if       "~a ? ~a : ~a"]
  [>        "~a > ~a"]
  [<        "~a < ~a"]
  [<=       "~a <= ~a"]
  [>=       "~a >= ~a"]
  [and      "~a && ~a"]
  [or       "~a || ~a"]
  [mod      "fmod2(~a, ~a)"])

(define-table constants->c
  [pi    "atan2(1.0, 0.0)"]
  [e     "exp(1.0)"])

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (if? l)
  (and (list? l) (eq? (car l) 'if)))

(define (compile->c prog [type "double"] [fname "f"])
  (define vars (program-variables prog))
  (define body (compile (program-body prog)))

  (define (value->c expr)
    (cond
     [(member expr vars) (fix-name expr)]
     [(member expr constants) (apply-converter (car (hash-ref constants->c expr)) '())]
     [(symbol? expr) expr]
     [else (->flonum expr)]))

  (define (app->c expr)
    (if (list? expr)
        (let* ([rec (list-ref (hash-ref operators->c (car expr)) 0)]
               [args (map value->c (cdr expr))])
          (apply-converter rec args))
        (value->c expr)))

  (write-string
   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "float ~a" (fix-name var))) ", "))

   (for/list ([assignment (cadr body)])
     (if (comparison? (cadr assignment))
         (printf "        bool ~a = ~a;\n" (car assignment)
                 (app->c (cadr assignment)))
         (printf "        ~a ~a = ~a;\n" type (car assignment)
                 (app->c (cadr assignment)))))

   (printf "        return ~a;\n" (value->c (caddr body)))
   (printf "}\n\n")))

(define-table operators->mpfr
  [+        "mpfr_add(~a, ~a, ~a, MPFR_RNDN)"]
  [-        '(#f #f "mpfr_neg(~a, ~a, MPFR_RNDN)" "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)")]
  [*        "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)"]
  [/        '(#f #f "mpfr_ui_div(~a, 1, ~a, MPFR_RNDN)" "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [abs      "mpfr_abs(~a, ~a, MPFR_RNDN)"]
  [sqrt     "mpfr_sqrt(~a, ~a, MPFR_RNDN)"]
  [sqr      (λ (x y) (format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)" x y y))]
  [exp      "mpfr_exp(~a, ~a, MPFR_RNDN)"]
  [expt     "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)"]
  [log      "mpfr_log(~a, ~a, MPFR_RNDN)"]
  [sin      "mpfr_sin(~a, ~a, MPFR_RNDN)"]
  [cos      "mpfr_cos(~a, ~a, MPFR_RNDN)"]
  [tan      "mpfr_tan(~a, ~a, MPFR_RNDN)"]
  [cotan    "mpfr_cot(~a, ~a, MPFR_RNDN)"]
  [asin     "mpfr_asin(~a, ~a, MPFR_RNDN)"]
  [acos     "mpfr_acos(~a, ~a, MPFR_RNDN)"]
  [atan     "mpfr_atan(~a, ~a, MPFR_RNDN)"]
  [sinh     "mpfr_sinh(~a, ~a, MPFR_RNDN)"]
  [cosh     "mpfr_cosh(~a, ~a, MPFR_RNDN)"]
  [tanh     "mpfr_tanh(~a, ~a, MPFR_RNDN)"]
  [if       "if (~a) { ~a; } else { ~a; }"]
  [>        "mpfr_cmp(~a, ~a) > 0"]
  [<        "mpfr_cmp(~a, ~a) < 0"]
  [<=       "mpfr_cmp(~a, ~a) <= 0"]
  [>=       "mpfr_cmp(~a, ~a) >= 0"]
  [and      "~a && ~a"]
  [or       "~a || ~a"]
  [atan2    "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)"]
  [mod      "mpfr_fmod2(~a, ~a, ~a)"])

(define-table constants->mpfr
  [pi    "mpfr_const_pi(~a, MPFR_RNDN)"]
  [e     "ERROR(\"E unsupported\")"])

(define (compile->mpfr prog [bits 128] [fname "f"])
  (define vars (program-variables prog))
  (define body (compile (program-body prog)))

  (define (app->mpfr out expr)
    (cond
     [(list? expr)
      (let* ([rec (list-ref (hash-ref operators->mpfr (car expr)) 0)]
             [args (cdr expr)])
        (apply-converter rec (cons out args)))]
     [(number? expr)
      (let ([register (gensym "r")])
        (format "mpfr_init_set_str(~a, \"~a\", 10, MPFR_RNDN)" out expr))]
     [(member expr (program-variables prog))
      (format "mpfr_set_flt(~a, ~a, MPFR_RNDN)" out (fix-name expr))]
     [(member expr constants)
      (apply-converter (car (hash-ref constants->mpfr expr)) (list out))]
     [(symbol? expr)
      (format "mpfr_set(~a, ~a, MPFR_RNDN)" out expr)]))

  (write-string
   (printf "static mpfr_t ~a;\n\n" (string-join (map symbol->string (map car (cadr body))) ", "))

   (printf "void setup_mpfr() {\n")
   ; Some guard bits added, just in case
   (printf "        mpfr_set_default_prec(~a);\n" (+ bits 8))
   (for ([reg (map car (cadr body))])
     (printf "        mpfr_init(~a);\n" reg))
   (printf "}\n\n")

   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "float ~a" (fix-name var))) ", "))

   (for ([assignment (cadr body)])
     (printf "        ~a;\n" (app->mpfr (car assignment) (cadr assignment))))


  (printf "        return mpfr_get_d(~a, MPFR_RNDN);\n" (caddr body))
  (printf "}\n\n")))

(define (compile->all name iprog oprog bits)
  (printf "#include <tgmath.h>\n")
  (printf "#include <gmp.h>\n")
  (printf "#include <mpfr.h>\n")
  (printf "#include <stdio.h>\n")
  (printf "#include <stdbool.h>\n\n")
  (printf "const char name[] = \"~a\";\n\n" name)
  (display (compile->c iprog "float" "f_if"))
  (display (compile->c iprog "double" "f_id"))
  (display (compile->c iprog "long double" "f_il"))
  (printf "long double fmod2(long double n, long double d) {\n")
  (printf "        double r = fmodl(n, d);\n")
  (printf "        return r < 0 ? r + d : r;\n")
  (printf "}\n\n")
  (newline)
  (display (compile->c oprog "float" "f_of"))
  (display (compile->c oprog "double" "f_od"))
  (display (compile->c oprog "long double" "f_ol"))
  (printf "void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {\n")
  (printf "        mpfr_fmod(r, n, d, MPFR_RNDN);\n")
  (printf "        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);\n")
  (printf "}\n\n")
  (newline)
  (display (compile->mpfr iprog bits "f_im"))
  (display (compile->mpfr oprog bits "f_om")))

(define (do-all results-file [output "/home/pavpan/casio/compile/tc~a.c"])
  (let ([fd (open-input-file results-file)])
    (let loop ([idx 0])
      (let ([line (read fd)])
        (if (eof-object? line)
            (void)
            (let* ([name (first line)]
                   [iprog (second line)]
                   [oprog (third line)]
                   [tprog (fourth line)]
                   [bits (fifth line)])
              (when oprog
                (printf "Outputing ~a to tc~a.c (~a)\n" name idx bits)
                (write-file (format output idx)
                 (compile->all name iprog oprog bits)))
              (loop (+ 1 idx))))))))
