#lang racket

(require net/uri-codec)
(require "../common.rkt")
(require "../programs.rkt")
(require "datafile.rkt")

(provide compile-info program->c)

(define (fix-name name)
  (string-replace (uri-encode (~a name)) #rx"[^a-zA-Z0-9]" "_"))

(define (comparison? l)
  (and (list? l) (member (car l) '(< > <= >= and or))))

(define (program->c prog [type "double"] [fname "f"])
  (define vars (program-variables prog))
  (define unused-vars (unused-variables prog))
  (define body (compile (program-body prog)))

  (define/contract (value->c expr)
    (-> expr? string?)
    (cond
     [(member expr vars) (fix-name expr)]
     [(number? expr) (~a expr)]
     [(constant? expr) (constant-info expr '->c/double)]
     [(symbol? expr) (~a expr)] ; intermediate variable
     [else
      (define val (real->double-flonum (->flonum expr)))
      (if (equal? type "float") (format "~af" val) (~a val))]))

  (define/contract (app->c expr)
    (-> expr? string?)
    (if (list? expr)
        (apply (operator-info (car expr) '->c/double) (map value->c (cdr expr)))
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

(define (program->mpfr prog [bits 128] [fname "f"])
  (define vars (program-variables prog))
  (define unused-vars (unused-variables prog))
  (define body (compile (program-body prog)))

  (define/contract (app->mpfr out expr)
    (-> string? expr? string?)
    (cond
     [(list? expr)
      (apply (operator-info (car expr) '->c/mpfr) out (map ~a (cdr expr)))]
     [(number? expr) ""]
     [(member expr vars)
      (format "mpfr_set_d(~a, ~a, MPFR_RNDN)" out (fix-name expr))]
     [(constant? expr)
      ((constant-info expr '->c/mpfr) out)]
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
     (printf "        ~a;\n" (app->mpfr (~a (car assignment)) (cadr assignment))))


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
    (when (and (not (member (table-row-status single-test) '("timeout" "error" "crash")))
               (not (member (table-row-status double-test) '("timeout" "error" "crash"))))
      (match (cons single-test double-test)
        [(cons (table-row name single-status _ _ _ _ _ _ _ vars input single-output _ single-bits dir)
               (table-row name double-status _ _ _ _ _ _ _ vars input double-output _ double-bits dir))
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

  (command-line
   #:program "compile/c.rkt"
   #:args (single-json-file double-json-file dir)
   (compile-info dir (read-datafile single-json-file) (read-datafile double-json-file))))
