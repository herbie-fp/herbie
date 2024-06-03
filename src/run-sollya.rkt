#lang racket

; Issues at 32000 with (λ (B x) ...)

(require math/bigfloat math/flonum "syntax/types.rkt")

(provide run-sollya)

;(require "./interval-evaluate.rkt")
;(require "run-mpfi.rkt")

(define *precision* (make-parameter 53))
(define timeout-ms 5.0)

(define (program-body prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define function->sollya-format
  (make-hash
   `((pow . "(~a ^ ~a)")
     (+ . "(~a + ~a)")
     (- . "(~a - ~a)")
     (/ . "(~a / ~a)")
     (sqrt . "sqrt(~a)")
     (* . "(~a * ~a)")
     (fma . "((~a * ~a) + ~a)")       ; no fma impl
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
     (neg . "(- ~a)")
     (fabs . "abs(~a)"))))


(define (round-sollya val)
  (match (*precision*)
    [53 (format "D(~a)" val)]
    [24 (format "SG(~a)" val)]))

(define (number->interval-sollya num)
  (format "[~a;~a]" (number->string num) (number->string num)))
(define (number->number-sollya num)
  (format "~a" num))

(define (expr->sollya expr)
  (match expr
    [(list (or 'let 'let*) (list vars ...) (app expr->sollya args) ...)
     (format "~a; ~a"
             (string-join (map (lambda (x)
                                 (format "var ~a; ~a := ~a" (car x) (car x) (expr->sollya (second x))))
                               vars) "; ")
             args)]
    [(or (list 'PI) '(PI) 'PI)
     "pi"]
    [(or '(E) 'E (list 'E))
     "exp(1)"]
    [(list op (app expr->sollya args) ...)
     (define sollya-format
       (if (and (equal? op '-) (equal? (length args) 1))
           "(- ~a)"
           (hash-ref function->sollya-format op #;(lambda () (raise-herbie-user-error)))))
     (apply (curry format sollya-format) args)]
    [(? symbol?)
     (if (equal? expr 'f)
         (error "f can not be a variable")
         (round-sollya (var-parse expr)))]  ; avoid symbols '.' '-' in variables
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
  (parameterize ([*precision* (string->number (string-replace (symbol->string precision) "binary" ""))])
    (format "prec=~a; procedure f(~a) { ~a; };"
            (*precision*)
            (string-join (map var-parse vars) ", ")
            (expr->sollya body))))

(define (exprs+ctxs->sollya exprs ctxs)
  (when (equal? 'binary32 (representation-name (context-repr (car ctxs))))
    (error "binary32 is not implemented in this comparison"))
  (define vars (context-vars (car ctxs)))
  (parameterize ([*precision* 53])
    (format "prec=~a; procedure f(~a) { ~a; };"
            (*precision*)
            (string-join (map var-parse vars) ", ")
            (expr->sollya exprs))))

(define (var-parse x)
   (string-replace
    (string-replace 
     (string-replace (symbol->string x) "-" "")
     "." "")
    "*" "_"))

(define sollya-path (find-executable-path "sollya"))
#;(define sollya-path (find-executable-path "/home/artemya/RA/sollya-8.0/sollya"))

(define (make-sollya prog #:backup [backup #f] #:inform [inform #f])
  (define-values (process m-out m-in m-err)
    (subprocess #f #f #f sollya-path "--flush"))

  (define buffer (make-bytes 65536 0))

  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup (apply fprintf backup fmt vs))
    (flush-output m-in))

  (with-handlers ([number? (lambda (e)
                             (subprocess-kill process #t)
                             (close-output-port m-in)
                             (close-input-port m-out)
                             (close-input-port m-err))])
    (ffprintf "~a\n" (if (equal? (length prog) 2)
                         (exprs+ctxs->sollya (car (first prog)) (second prog))
                         (program->sollya prog))))
  
  (let loop ([i 0])
    (define step (read-bytes-avail! buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (if (regexp-match #rx"^The precision has been set to [0-9]+ bits.\n" s)
        (when inform (eprintf "Sollya started for:\n\t~a\n" prog))
        (loop (+ i step))))

  (values process m-out m-in m-err))

(define (seconds->ms seconds)
  (* 1000 (string->number seconds)))

; Format: (list interal-time external-time result status)
(define (run-sollya prog #:backup [backup #f])
  (printf "\nSollya program: ~s\n" (if (equal? (length prog) 2)
                                       (exprs+ctxs->sollya (car (first prog)) (second prog))
                                       (program->sollya prog)))
  (define-values (process m-out m-in m-err)
    (make-sollya prog #:backup backup #:inform #t))

  (define buffer (make-bytes 65536 0))
  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup (apply fprintf backup fmt vs))
    (flush-output m-in))

  (define (compiled-spec pt [interval-eval #f])
    (define start (current-inexact-milliseconds))
    (define input (if interval-eval                        ; parse points to Sollya's format
                      (map number->interval-sollya pt)
                      (map number->number-sollya pt)))
    (ffprintf "time(f(~a));\n" (string-join input ", "))   ; write to Sollya

    (define out (if interval-eval                          ; parse Sollya's output
                    (parse-sollya-interval buffer m-out start)
                    (parse-sollya-number  buffer m-out start)))
    
    (when (equal? (last out) 'exit)                        ; when Sollya has timed out restart the process
      (define-values (process2 m-out2 m-in2 m-err2)
        (make-sollya prog #:backup backup))
      (kill-process)
      (set! process process2)
      (set! m-out m-out2)
      (set! m-in m-in2)
      (set! m-err m-err2))
    out)
  (define (kill-process)
    (subprocess-kill process #t)
    (close-output-port m-in)
    (close-input-port m-out)
    (close-input-port m-err))
  (values compiled-spec kill-process))

; provides too wide intervals
(define (parse-sollya-interval buffer m-out start)
  (let loop ([i 0])
    (define step (read-bytes-avail!* buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (cond
      [(regexp-match #rx"^Warning: the given expression is undefined or numerically unstable\n*" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time)
               (list (fl +nan.0) (fl +nan.0))
               'invalid))]

      [(regexp-match #rx"\\[[-+.e0-9]+;[-+.e0-9]+\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (match-define (list lower upper)
           (string-split (string-replace (string-replace result "[" "") "]" "") ";"))
         (list dt (seconds->ms sollya-time)
               (list (bigfloat->flonum (bf lower)) (bigfloat->flonum (bf upper)))
               'valid))]

      [(regexp-match #rx"[-+.e0-9]+\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time)
               (list (bigfloat->flonum (bf result)) (bigfloat->flonum (bf result)))
               'valid))]
        
      [(regexp-match #rx"\\[NaN;NaN\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time)
               (list (fl +nan.0) (fl +nan.0))
               'unsamplable))]
        
      [(regexp-match #rx"\\[-infty;infty\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time)
               (list (fl -inf.0) (fl +inf.0))
               'unsamplable))]
        
      [(regexp-match #rx"\\[[-+.e0-9]+;infty\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (match-define (list lower upper)
           (string-split (string-replace (string-replace result "[" "") "]" "") ";"))
         (list dt (seconds->ms sollya-time)
               (list (bigfloat->flonum (bf lower)) (fl +inf.0))
               'valid))]
      
      [(regexp-match #rx"\\[-infty;[-+.e0-9]+\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (match-define (list lower upper)
           (string-split (string-replace (string-replace result "[" "") "]" "") ";"))
         (list dt (seconds->ms sollya-time)
               (list (fl -inf.0) (bigfloat->flonum (bf upper)))
               'valid))]

      [(> (- (current-inexact-milliseconds) start) timeout-ms)
       (when (not (equal? s ""))
         (eprintf "\nUnprocessed output from Sollya\n")
         (eprintf "Stdout interval: ~s\n" s)
         (error "crashed"))
       (list timeout-ms timeout-ms (list (fl +nan.0) (fl +nan.0)) 'exit)]
      
      [else
       (loop (+ i step))])))


(define (parse-sollya-number buffer m-out start)
  (let loop ([i 0])
    (define step (read-bytes-avail!* buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (cond
      ; Undefined
      [(regexp-match #rx"^Warning: the given expression is undefined or numerically unstable\n*" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time) result (if (equal? result "NaN")
                                                       'invalid
                                                       'unsamplable)))]

      ; NaN
      [(regexp-match #rx"^NaN\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) (fl +nan.0) 'invalid))]
      
      [(regexp-match #rx"^\\[NaN;NaN\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) (fl +nan.0) 'invalid))]

      ; Infinity
      [(regexp-match #rx"^-?infty\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time)
               (if (string-contains? result "-") (fl -inf.0) (fl +inf.0))
               'valid))]

      [(regexp-match #rx"^[-+.e0-9]+\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) (bigfloat->flonum (bf result)) 'valid))]

      ; Timeout
      [(> (- (current-inexact-milliseconds) start) timeout-ms)
       (when (not (equal? s ""))
         (eprintf "\nUnprocessed output from Sollya\n")
         (eprintf "Stdout number: ~s\n" s)
         (error "crashed"))
       (list timeout-ms timeout-ms (fl +nan.0) 'exit)]
      [else
       (loop (+ i step))])))