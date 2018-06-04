#lang racket

(require "fpcore-common.rkt" "fpcore.rkt")
(provide compile-program headers)

(define (fix-name name)
  (string-join
    (for/list ([char (~a name)])
      (if (regexp-match #rx"[a-zA-Z0-9_]" (string char))
          (string char)
          (format "_~a_" (char->integer char))))
    ""))

(define/match (constant->js c)
  [('E) "math.E"]
  [('LOG2E) "math.LOG2E"]
  [('LOG10E) "math.LOG10E"]
  [('LN2) "math.LN2"]
  [('LN10) "math.LN10"]
  [('PI) "math.pi"]
  [('PI_2) "(math.pi/2)"]
  [('PI_4) "(math.pi/4)"]
  [('1_PI) "(1/math.pi)"]
  [('2_PI) "(2/math.pi)"]
  [('2_SQRTPI) "(2/math.sqrt(math.pi))"]
  [('SQRT2) "math.SQRT2"]
  [('SQRT1_2) "(1/math.SQRT2)"]
  [('MAXFLOAT) "Number.MAX_VALUE"]
  [('TRUE) "true"]
  [('FALSE) "false"]
  [('INFINITY) "Infinity"]
  [('NAN) "math.NaN"]
  [(_) (error 'constant->js "Unsupported constant ~a" c)])

(define/match (operator->js op)
  [((or '== '+ '- '* '/  '< '> '<= '>=)) (format "(~a ~a ~a)" "~a" op "~a")]
  [('and) "~a && ~a"]
  [('or) "~a || ~a"]
  [('not) "!~a"]
  [('fabs) "math.abs(~a)"]
  [('exp) "math.exp(~a)"]
  [('exp2) "math.pow(2, ~a)"]
  [('expm1) "math.expm1(~a)"]
  [('log) "math.log(~a)"]
  [('log10) "math.log10(~a)"]
  [('log2) "math.log2(~a)"]
  [('log1p) "math.log1p(~a)"]
  [('logb) "math.floor(math.log2(math.abs(~a)))"]
  [('pow) "math.pow(~a, ~a)"]
  [('sqrt) "math.sqrt(~a)"]
  [('cbrt) "math.cbrt(~a)"]
  [('hypot) "math.hypot(~a, ~a)"]
  [('sin) "math.sin(~a)"]
  [('cos) "math.cos(~a)"]
  [('tan) "math.tan(~a)"]
  [('asin) "math.asin(~a)"]
  [('acos) "math.acos(~a)"]
  [('atan) "math.atan(~a)"]
  [('atan2) "math.atan2(~a, ~a)"]
  [('sinh) "math.sinh(~a)"]
  [('cosh) "math.cosh(~a)"]
  [('tanh) "math.tanh(~a)"]
  [('asinh) "math.asinh(~a)"]
  [('acosh) "math.acosh(~a)"]
  [('atanh) "math.atanh(~a)"]
  [('erf) "math.erf(~a)"]
  [('erfc) "1 - math.erf(~a)"] ;; TODO: This implementation has large error for large inputs
  [('tgamma) "math.gamma(~a)"]
  [('lgamma) "math.log(math.gamma(~a))"]
  [('ceil) "math.ceil(~a)"]
  [('floor) "math.floor(~a)"]
  [('remainder) "math.mod(~a, ~a)"]
  [('fmax) "math.max(~a, ~a)"]
  [('fmin) "math.min(~a, ~a)"]
  [('fdim) "math.max(0, ~a - ~a)"]
  [('copysign) "math.abs(~a) * math.sign(~a)"]
  [('trunc) "math.fix(~a)"]
  [('round) "math.round(~a)"]
  [('isinf)  "(math.abs(~a) === Number.POSITIVE_INFINITY)"]
  [('isnan) "isNaN(~a)"]
  [(_) (error 'operator->js "Unsupported operation ~a" op)])

(define (application->js type operator args)
  (if (and (eq? operator '-) (= (length args) 1))
    (format "(- ~a)" (car args))
    (apply format (operator->js operator) args)))

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)])
                         (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (expr->js expr #:names [names #hash()] #:type [type 'binary64] #:indent [indent "\t"])
  ;; Takes in an expression. Returns an expression and a new set of names
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val vals])
       (printf "~avar ~a = ~a;\n" indent (fix-name var*)
               (expr->js val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->js body #:names names* #:type type #:indent indent)]
    [`(if ,cond ,ift ,iff)
     (define test (expr->js cond #:names names #:type type #:indent indent))
     (define outvar (gensym 'temp))
     (printf "~avar ~a;\n" indent (fix-name outvar))
     (printf "~aif (~a) {\n" indent test)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->js ift #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a} else {\n" indent)
     (printf "~a\t~a = ~a;\n" indent (fix-name outvar)
             (expr->js iff #:names names #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (fix-name outvar)]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var vars] [var* vars*] [val inits])
       (printf "~avar ~a = ~a;\n" indent (fix-name var*)
               (expr->js val #:names names #:type type #:indent indent)))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (fix-name (gensym 'test)))
     (printf "~avar ~a = ~a\n" indent test-var
             (expr->js cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~awhile (~a) {\n" indent test-var)
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (printf "~a\tvar ~a = ~a;\n" indent (fix-name temp-var)
               (expr->js update #:names names* #:type type #:indent (format "~a\t" indent))))
     (for ([var* vars*] [temp-var temp-vars])
       (printf "~a\t~a = ~a;\n" indent (fix-name var*) (fix-name temp-var)))
     (printf "~a\t~a = ~a;" indent test-var
             (expr->js cond #:names names* #:type type #:indent (format "~a\t" indent)))
     (printf "~a}\n" indent)
     (expr->js retexpr #:names names* #:type type #:indent indent)]
    [(list (? operator? operator) args ...)
     (define args_c
       (map (λ (arg) (expr->js arg #:names names #:type type #:indent indent)) args))
     (application->js type operator args_c)]
    [(? constant?)
     (format "(~a)" (constant->js expr))]
    [(? symbol?)
     (fix-name (dict-ref names expr expr))]
    [(? number?)
     (format "~a" (real->double-flonum expr) )]))

(define (compile-program prog #:name name)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (_ properties) (parse-properties props))
  (define type (dict-ref properties ':precision 'binary64))

  (define arg-strings
    (for/list ([var args])
      (format "~a" (fix-name (if (list? var) (car var) var)))))
  (define func-body
    (with-output-to-string
      (λ ()
        (parameterize ([*names* (apply mutable-set args)])
          (printf "\treturn ~a;\n" (expr->js body #:type type))))))
  (format "function ~a(~a) {\n~a}\n"
          (fix-name name)
          (string-join arg-strings ", ")
          func-body))

(define headers
  (string-append
    "// Code generated by racket core2js.rkt DO NOT EDIT.\n\n"
    "var math = require('mathjs')\n\n"))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "compile.rkt"
   #:args (pkg)
   (port-count-lines! (current-input-port))
   (printf headers)
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (printf "~a\n" (compile-program expr #:name (format "Ex~a" n))))))
