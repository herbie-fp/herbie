#lang racket

; Issues at 32000 with (λ (B x) ...)

(require math/bigfloat math/flonum)

(provide run-sollya)

;(require "./interval-evaluate.rkt")
;(require "run-mpfi.rkt")

(define *precision* (make-parameter 32))

(define (program-body prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define function->sollya-format
  (make-hash
   `((pow . "~a ^ ~a")
     (+ . "~a + ~a")
     (- . "~a - ~a")
     (/ . "~a / ~a")
     (sqrt . "sqrt(~a)")
     (* . "~a * ~a")
     (fma . "(~a * ~a) + ~a")       ; no fma impl
     (hypot . "sqrt(~a^2 + ~a^2)")  ; no hypot impl
     (exp . "exp(~a)")
     (expm1 . "expm1(~a)")
     (log . "log(~a)")
     (log10 . "log10(~a)")
     (log2 . "log2(~a)")
     (log1p . "log1p(~a)")
     ;(cbrt . "(~a^(1/3))")         no cbrt
     (sin . "sin(~a)")
     (cos . "cos(~a)")
     (tan . "tan(~a)")
     (asin . "asin(~a)")
     (acos . "acos(~a)")
     (atan . "atan(~a)")
     (sinh . "sinh(~a)")
     (cosh . "cosh(~a)")
     (tanh . "tanh(~a)")
     (asinh . "asinh(~a)")
     (acosh . "acosh(~a)")
     (atanh . "atanh(~a)")
     (atan2 . "atan(~a/~a)")
     (erf . "erf(~a)")
     (erfc . "erfc(~a)")
     ;(tgamma . Gamma)
     ;(lgamma . LogGamma)
     (ceil . "ceil(~a)")
     (floor . "floor(~a)")
     (fmod . "mod(~a, ~a)")
     ;(remainder . QuotientRemainer) ; no remainder
     (fmax . "max(~a, ~a)")
     (fmin . "min(~a, ~a)")
     ;(truc . Truncate)                no truncate
     (round . "round(~a)")
     ;(if . "(if (~a ~a ~a) then ~a)") ; it is not that simple with if
     ;(< . LessThan)
     ;(> . GreaterThan)
     ;(<= . LessEqual)
     ;(>= . GreaterEqual)
     ;(== . Equal)
     ;(!= . NotEqual)
     ;(and . And)
     ;(or . Or)
     ;(not . Not)
     (neg . "- ~a")
     (fabs . "abs(~a)"))))

;; exp2, expm1, 

(define (round-sollya val)
  (match (*precision*)
    [64 (format "D(~a)" val)]
    [32 (format "SG(~a)" val)]))

(define (number->sollya num)
  #;(bigfloat->string num)
  (number->string num))

(define (expr->sollya expr)
  (match expr
    [(list (or 'let 'let*) (list vars ...) (app expr->sollya args) ...)
     (format "~a; ~a"
             (string-join (map (lambda (x)
                                 (format "var ~a; ~a := ~a" (car x) (car x) (expr->sollya (second x))))
                               vars) "; ")
             args)]
    [(list op (app expr->sollya args) ...)
     (define sollya-format
       (if (and (equal? op '-) (equal? (length args) 1))
           "- ~a"
           (hash-ref function->sollya-format op)))
     (apply (curry format sollya-format) args)]
    ['PI
     "pi"]
    ['E
     "exp(1)"]
    [(? symbol?)
     (if (equal? expr 'f)
         (error "f can not be a variable")
         (round-sollya (string-replace (symbol->string expr) "-" "")))]  ; avoid symbol '-' in variables
    [(? number?)
     (round-sollya expr)]))

(define (program->sollya prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...)
                      ':name name
                      ':precision precision
                      (or (list ':pre _ ':alt _ body)
                          (list ':alt _ ':pre _ body)
                          (list ':pre _ body)
                          (list ':alt _ body)
                          body))
    prog)
  (*precision* (string->number (string-replace (symbol->string precision) "binary" "")))
  (format "prec=~a; procedure f(~a) { ~a; };"
          (*precision*)
          (string-join (map (lambda (x)
                              (string-replace (symbol->string x) "-" "")) ; avoid symbol '-' in variables
                            vars) ", ")
          (expr->sollya body)))

(define (load-points port)
  (define points
    (for/list ([read-res (in-port read port)])
      read-res))
  (for/hash ([group (group-by second points)])
    (values (second (car group)) (map third group))))

(define sollya-path (find-executable-path "sollya"))

(define (make-sollya prog #:backup [backup #f])
  (define-values (process m-out m-in m-err)
    (subprocess #f #f #f sollya-path "--flush" "--warnonstderr"))

  (define buffer (make-bytes 65536 0))

  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup (apply fprintf backup fmt vs))
    (flush-output m-in))

  (ffprintf "~a\n" (program->sollya prog))
  (println (program->sollya prog))
  
  (let loop ([i 0])
    (define step (read-bytes-avail! buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (if (regexp-match #rx"^The precision has been set to [0-9]+ bits.\n" s)
        (eprintf "Sollya started for:\n\t~a\n" prog)
        (loop (+ i step))))

  (values process m-out m-in m-err))

(define (seconds->ms seconds)
  (* 1000 (string->number seconds)))

; Format: (list our-time-measurement sollya-time-report result status)
(define (run-sollya prog #;pts #:backup [backup #f])
  (define-values (process m-out m-in m-err)
    (make-sollya prog #:backup backup))

  (define buffer (make-bytes 65536 0))
  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup (apply fprintf backup fmt vs))
    (flush-output m-in))

  ;(define out
    ;(for/list ([pt (in-list pts)])
  (define (compiled-spec pt)
    (define start (current-inexact-milliseconds))
    (ffprintf "time(f(~a));\n"
              (string-join (map number->sollya pt) ", "))
    (let loop ([i 0])
      (define step (read-bytes-avail!* buffer m-out i))
      (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
      (cond
        [(> (- (current-inexact-milliseconds) start) 2000.0)
         (eprintf "Killing and restarting Sollya\n")
         (eprintf "~s\n" s)
         (define-values (process2 m-out2 m-in2 m-err2)
           (make-sollya prog #:backup backup))
         (subprocess-kill process true)
         (set! process process2)
         (set! m-out m-out2)
         (set! m-in m-in2)
         (set! m-err m-err2)
         (list 2000.0 2000.0 +nan.0 'exit)]
          
        [(regexp-match #rx"^Warning: the given expression is undefined or numerically unstable\n*" s)
         (let ([dt (- (current-inexact-milliseconds) start)])
           (match-define (list _ result sollya-time)
             (string-split s "\n"))
           (list dt (seconds->ms sollya-time) +nan.0 'unsamplable))]
          
        [(regexp-match #rx"^[-+.e0-9]+\n[-+.e0-9]+\n$" s)
         (let ([dt (- (current-inexact-milliseconds) start)])
           (match-define (list result sollya-time) (string-split s "\n"))
           (list dt (seconds->ms sollya-time) (list (bigfloat->flonum (bf result))) 'valid))]
        [else
         (loop (+ i step))])))
  compiled-spec)
  ;(ffprintf "quit;\n")
  ;(subprocess-wait process)
  ;(cons (subprocess-status process) out))

#;(define (parse-output s)
  (define lines (string-split s "\n" #:repeat? #t))
  (with-handlers ([exn:misc:match? (λ (e)
    (newline)
    (printf "Could not parse results:\n")
    (pretty-print lines)
    (exit))])

    (match-lines lines)))


#;(define (match-lines lines)
  (match lines
    [(list
      (regexp #rx"Warning: rounding has happened. The value displayed is a faithful rounding to [0-9]+ bits of the true result.")
      rest ...)
     (match-lines rest)]
    [(list
      "If safe computation is needed, try to increase the precision."
      rest ...)
     (match-lines rest)]
    [(list
      (regexp #rx"Warning: Rounding occurred when converting the constant \"[-+0-9e]+\" to floating-point with [0-9]+ bits.")
      rest ...)
     (match-lines rest)]
    [(list
      (regexp #rx"^[-+e0-9.]+")
      _ ...)
     (first lines)]
    ))

(define (count-results out)
  (define sampled 0)
  (define invalid 0)
  (define unsamplable 0)
  (define unknown 0)
  (define crash 0)
  (define timeout 0)
  (for ([val out])
    (match (cdr val)
      ['invalid
       (set! invalid (add1 invalid))]
      ['memory
       (set! unknown (add1 unknown))
       (set! crash (add1 crash))]
      ['timeout
       (set! unknown (add1 unknown))
       (set! timeout (add1 timeout))]
      ['unsamplable
       (set! unsamplable (add1 unsamplable))]
      ['unknown
       (set! unknown (add1 unknown))]
      [(regexp #rx"[0-9]+(\\.[0-9]+)?(`[0-9]*)?(\\*\\^-?[0-9]+)?")
       (set! sampled (add1 sampled))]
      ))
  (list sampled invalid unsamplable unknown crash timeout))

(define (add-results r1 r2)
  (map + r1 r2))

(define (print-results results)
  (match-define (list sampled invalid unsamplable unknown crash timeout) results)
  (eprintf "\nResults: ~a ok, ~a bad, ~a unsamplable, ~a unknown (~a crash, ~a timeout)\n"
           sampled invalid unsamplable unknown crash timeout))

#;(define (go points skip)
  (define results (list 0 0 0 0 0 0))
  (for ([(prog pts*) (in-hash points)])
    (call-with-output-file "mathematica.log" #:exists 'replace
      (λ (p)
        (define to-drop (min skip (length pts*)))
        (set! skip (- skip to-drop))
        (define pts (drop pts* to-drop))

        (match-define (cons status out) (run-mathematica prog pts #:backup p))
        (match status
          [0
           (set! results (add-results results (count-results out)))
           (print-results results)]
          [_
           (printf "Status: ~a\n" status)
           (pretty-print out)
           (exit status)]))))
    results)

#;(module+ main
  (define skip 0)
  (command-line
   #:program "run-mathematica"
   #:once-each
   [("--skip") n "How many points to skip"
    (set! skip (or (string->number n) skip))]
   #:args (points-file)
   (define points (call-with-input-file points-file load-points))
   (go points skip)))