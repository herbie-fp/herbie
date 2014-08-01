#lang racket

(require casio/common)
(require casio/programs)

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
  [abs      "abs(~a)"]
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
  [pi    "PI"]
  [e     "exp(1.0)"])

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (if? l)
  (and (list? l) (eq? (car l) 'if)))

(define (compile->c prog [fname "f"])
  (define fmod-used? #f)
  (define vars (program-variables prog))
  (define body (compile (program-body prog)))

  (define (value->c expr)
    (cond
     [(member expr vars) expr]
     [(member expr constants) (apply-converter (hash-ref constants->c expr))]
     [(symbol? expr) expr]
     [else (->flonum expr)]))

  (define (app->c expr)
    (if (list? expr)
        (let* ([rec (list-ref (hash-ref operators->c (car expr)) 0)]
               [args (map value->c (cdr expr))])
          (when (eq? (car expr) 'mod)
            (set! fmod-used? #t))
          (apply-converter rec args))
        (value->c expr)))

  (write-string
   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "double ~a" var)) ", "))

   (for/list ([assignment (cadr body)])
     (if (comparison? (cadr assignment))
         (printf "        bool ~a = ~a;\n" (car assignment)
                 (app->c (cadr assignment)))
         (printf "        double ~a = ~a;\n" (car assignment)
                 (app->c (cadr assignment)))))

   (printf "        return ~a;\n" (value->c (caddr body)))
   (printf "}\n")

   (when fmod-used?
     (printf "\ndouble fmod2(double n, double d) {\n")
     (printf "        double r = fmod(n, d);\n")
     (printf "        return r < 0 ? r + d : r;\n")
     (printf "}\n"))))

(define-table operators->mpfr
  [+        "mpfr_add(~a, ~a, ~a, MPFR_RNDN)"]
  [-        '(#f #f "mpfr_neg(~a, ~a, MPFR_RNDN)" "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)")]
  [*        "mpfr_mul(~a, ~a, MPFR_RNDN)"]
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
  [pi    "mpfr_const_pi(~a)"]
  [e     "ERROR(\"E unsupported\")"])

(define (compile->mpfr prog [bits 128] [fname "f"])
  (define fmod-used? #f)
  (define vars (program-variables prog))
  (define body (compile (program-body prog)))

  (define (value->mpfr out expr)
    (cond
     [(member expr (program-variables prog)) expr]
     [(member expr constants) ((hash-ref constants->mpfr expr) out)]
     [(symbol? expr) expr]
     [else (error "Unknown syntax for MPFR" expr)]))

  (define (app->mpfr out expr)
    (cond
     [(list? expr)
      (let* ([rec (list-ref (hash-ref operators->mpfr (car expr)) 0)]
             [args (map (curry value->mpfr out) (cdr expr))])
        (when (eq? (car expr) 'mod)
          (set! fmod-used? #t))
        (apply-converter rec (cons out args)))]
     [(number? expr)
      (let ([register (gensym "r")])
        (format "mpfr_init_set_str(~a, \"~a\", 10, MPFR_RNDN)" out expr))]
     [(member expr (program-variables prog))
      (format "mpfr_set_d(~a, ~a, MPFR_RNDN)" out expr)]
     [(member expr constants)
      (apply-converter (hash-ref constants->mpfr expr) (list out))]
     [(symbol? expr)
      (format "mpfr_set(~a, ~a, MPFR_RNDN)" out expr)]))

  (write-string
   (printf "static mpfr_t ~a;\n\n" (string-join (map symbol->string (map car (cadr body))) ", "))

   (printf "void setup_mpfr() {\n")
   (printf "        mpfr_set_default_prec(~a);\n" bits)
   (for ([reg (map car (cadr body))])
     (printf "        mpfr_init(~a);\n" reg))
   (printf "}\n\n")

   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "double ~a" var)) ", "))

   (for ([assignment (cadr body)])
     (printf "        ~a;\n" (app->mpfr (car assignment) (cadr assignment))))


  (printf "        return mpfr_get_d(~a, MPFR_RNDN);\n" (caddr body))
  (printf "}\n")

  (when fmod-used?
    (printf "\nvoid mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {\n")
    (printf "        mpfr_fmod(r, n, d, MPFR_RNDN);\n")
    (printf "        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);\n")
    (printf "}\n"))))

(define (do-compile iprog oprog bits path fname)
  (write-file (string-append path fname ".c")
   (printf "#include <math.h>\n")
   (printf "#include <gmp.h>\n")
   (printf "#include <mpfr.h>\n\n")
   (display (compile->c iprog (string-append fname "_id")))
   (newline)
   (display (compile->c oprog (string-append fname "_od")))
   (newline)
   (display (compile->mpfr iprog bits (string-append fname "_im")))))
