#lang racket

(require casio/common)
(require casio/programs)
(require reports/datafile)
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

(define (program->c prog [type "double"] [fname "f"])
  (define vars (program-variables prog))
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
   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "~a ~a" type (fix-name var))) ", "))

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
  [if       (λ (r c a b)
               (format
                "if (mpfr_get_si(~a, MPFR_RNDN)) { mpfr_set(~a, ~a, MPFR_RNDN); } else { mpfr_set(~a, ~a, MPFR_RNDN); }"
                c r a r b))]
  [>        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)"]
  [<        "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)"]
  [<=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)"]
  [>=       "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)"]
  [and      "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)"]
  [or       "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) || mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)"]
  [atan2    "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)"]
  [mod      "mpfr_fmod2(~a, ~a, ~a)"])

(define-table constants->mpfr
  [pi    "mpfr_const_pi(~a, MPFR_RNDN)"]
  [e     "ERROR(\"E unsupported\")"])

(define (program->mpfr prog [bits 128] [fname "f"])
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
   (for ([reg (map car (cadr body))])
     (printf "        mpfr_init(~a);\n" reg))
   (printf "}\n\n")

   (printf "double ~a(~a) {\n" fname
           (string-join (for/list ([var vars]) (format "double ~a" (fix-name var))) ", "))

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
  (printf "void mpfr_fmod2(mpfr_t r, mpfr_t n, mpfr_t d) {\n")
  (printf "        mpfr_fmod(r, n, d, MPFR_RNDN);\n")
  (printf "        if (mpfr_cmp_ui(r, 0) < 0) mpfr_add(r, r, d, MPFR_RNDN);\n")
  (printf "}\n\n")
  (newline)
  (display (program->mpfr iprog bits "f_im"))
  (display (program->mpfr fprog bits "f_fm"))
  (display (program->mpfr dprog bits "f_dm")))

(define (compile-c-files float-results-file double-results-file [output-file "tc~a.c"])
  (for ([id (in-naturals)] [fline (read-datafile float-results-file)] [dline (read-datafile double-results-file)])
    (match (cons fline dline)
      [`((,name ,input ,foutput ,target ,fbits ,ftime) .
         (,name ,input ,doutput ,target ,dbits ,dtime))
       (debug #:from 'compile-datafile "Compiling" name "to" (format output-file id))
       (write-file (format output-file id) (compile-all name input foutput doutput dbits))])))

(define (compile-mpfr-bits float-results-file double-results-file dir)
  (write-file (build-path dir "mpfr-bits.csv")
              (for ([line (read-datafile double-results-file)])
                (match line
                  [`(,name ,input ,output ,target ,bits ,time)
                   (printf "~a\n" bits)]))))

(define (compile-casio-runtime float-results-file double-results-file dir)
  (write-file (build-path dir "casio-runtime.csv")
              (for ([line (read-datafile double-results-file)])
                (match line
                  [`(,name ,input ,output ,target ,bits ,time)
                   (printf "~a\n" (/ time 1000))]))))

(define (compile-datafiles float-results-file double-results-file [dir "."] [output-file "tc~a.c"])
  (compile-c-files float-results-file double-results-file (string-append dir "/" output-file))
  (compile-mpfr-bits float-results-file double-results-file dir)
  (compile-casio-runtime float-results-file double-results-file dir))

(define *format* "tc~a.c")
(define *dir* ".")

(command-line
 #:program "compile"
 #:once-each
 [("-d") dir "Directory into which to place compiled files"
  (set! *dir* dir)]
 [("-f") fmt "Format of output file names; use a single ~a for an index"
  (set! *format* fmt)]
 #:args (float-results-file double-results-file)
 (compile-datafiles float-results-file double-results-file *dir* *format*))
