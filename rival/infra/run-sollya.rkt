#lang racket
(require math/bigfloat
         math/flonum)

(define sollya-path (find-executable-path "sollya"))

(provide sollya-compile
         sollya-apply
         sollya-kill)

(struct sollya-machine
        ([process #:mutable] [out #:mutable]
                             [in #:mutable]
                             [err #:mutable]
                             buffer
                             exprs
                             vars
                             prec
                             backup))

;----------------------------------------- INPUT PARSING ---------------------------------------------
; Precision of the current program
(define *precision* (make-parameter 53))

(define function->sollya-format
  (make-hash `((pow . "(~a ^ ~a)") (+ . "(~a + ~a)")
                                   (- . "(~a - ~a)")
                                   (/ . "(~a / ~a)")
                                   (sqrt . "sqrt(~a)")
                                   (* . "(~a * ~a)")
                                   (fma . "((~a * ~a) + ~a)") ; shorthand for fma
                                   (hypot . "sqrt(~a^2 + ~a^2)") ; shorthand for hypot
                                   (exp . "exp(~a)")
                                   (expm1 . "expm1(~a)")
                                   (log . "log(~a)")
                                   (log10 . "log10(~a)")
                                   (log2 . "log2(~a)")
                                   (log1p . "log1p(~a)")
                                   ;(cbrt . "(~a^(1/3))")                           ; no cbrt
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
                                   ;(atan2 . "atan(~a/~a)")                         ; no atan2
                                   (erf . "erf(~a)")
                                   (erfc . "erfc(~a)")
                                   ;(tgamma . Gamma)                                ; no tgamma
                                   ;(lgamma . LogGamma)                             ; no lgamma
                                   (ceil . "ceil(~a)")
                                   (floor . "floor(~a)")
                                   (fmod . "mod(~a, ~a)")
                                   ;(remainder . QuotientRemainer)                  ; no remainder
                                   (fmax . "max(~a, ~a)")
                                   (fmin . "min(~a, ~a)")
                                   ;(truc . Truncate)                               ; no truncate
                                   (round . "round(~a)")
                                   (assert . "if (~a) then (~a) else nan")
                                   (if . "if (~a) then (~a) else (~a)")
                                   (TRUE . "true")
                                   (< . "((~a) < (~a))")
                                   (> . "((~a) > (~a))")
                                   (<= . "((~a) <= (~a))")
                                   (>= . "((~a) >= (~a))")
                                   (== . "((~a) == (~a))")
                                   (!= . "((~a) != (~a))")
                                   (and . "((~a) && (~a))")
                                   (or . "((~a) || (~a))")
                                   (not . "(!(~a))")
                                   (neg . "(- (~a))")
                                   (fabs . "abs(~a)"))))

(define (round-sollya val)
  (match (*precision*)
    [53 (format "D(~a)" val)]
    [24 (format "SG(~a)" val)]))

(define (number->number-sollya num)
  (format "~a" num))

(define (expr->sollya expr)
  (match expr
    ; Precondition parsing
    [(list (list 'assert precondition) (app expr->sollya args))
     (if (equal? precondition '(TRUE))
         args
         (format (hash-ref function->sollya-format 'assert) (expr->sollya precondition) args))]

    ; Constants
    [(or (list 'PI) '(PI) 'PI) "pi"]
    [(or '(E) 'E (list 'E)) "exp(1)"]

    ; Operation parsing
    [(list '- (app expr->sollya arg))
     (define sollya-format "(- ~a)")
     (format sollya-format arg)]

    [(list op (app expr->sollya args) ...)
     (define sollya-format (hash-ref function->sollya-format op))
     (apply (curry format sollya-format) args)]

    ; Variable to be rounded
    [(? symbol?) (round-sollya (var-parse expr))]

    ; Constant with arbitary precisino
    [(? number?) (format "(~a)" expr)]))

(define (prog->sollya exprs vars prec)
  (parameterize ([*precision* prec])
    (format "prec=~a; procedure f(~a) { ~a; };"
            (*precision*)
            (string-join (map var-parse vars) ", ")
            (expr->sollya exprs))))

(define (var-parse x)
  (if (equal? x 'f)
      (string-replace (symbol->string x) "f" "fvar")
      (if (equal? x 'D)
          (string-replace (symbol->string x) "D" "Dvar")
          (string-replace (string-replace (string-replace (symbol->string x) "-" "") "." "")
                          "*"
                          "_"))))

; ------------------------------------------ APPLY ---------------------------------------------------
; Output format is: (values interal-time external-time result status)
(define (sollya-apply machine pt #:timeout [timeout 20.0])
  ; Process input
  (define input (map number->number-sollya pt))
  (sollya-write machine "time(f(~a));\n" (string-join input ", "))

  ; Process output
  (define out (parse-sollya-output machine (+ timeout 5.0)))

  ; when Sollya has timed out - restart the process
  (when (equal? (last out) 'exit)
    (define machine-new
      (sollya-compile (sollya-machine-exprs machine)
                      (sollya-machine-vars machine)
                      (sollya-machine-prec machine)))
    (sollya-kill machine)
    (sollya-machine-copy machine machine-new))
  out)

; --------------------------------------- HELPER FUNCTIONS -------------------------------------------
(define (sollya-machine-copy machine1 machine2)
  (set-sollya-machine-process! machine1 (sollya-machine-process machine2))
  (set-sollya-machine-out! machine1 (sollya-machine-out machine2))
  (set-sollya-machine-in! machine1 (sollya-machine-in machine2))
  (set-sollya-machine-err! machine1 (sollya-machine-err machine2)))

(define (sollya-write machine fmt . vs)
  (apply fprintf (sollya-machine-in machine) fmt vs)
  (when (sollya-machine-backup machine)
    (apply fprintf (sollya-machine-backup machine) fmt vs))
  (flush-output (sollya-machine-in machine)))

(define (sollya-kill machine)
  (subprocess-kill (sollya-machine-process machine) #t)
  (close-output-port (sollya-machine-in machine))
  (close-input-port (sollya-machine-out machine))
  (close-input-port (sollya-machine-err machine)))

; --------------------------------------- COMPILATION ------------------------------------------------
(define (sollya-compile exprs vars prec #:backup [backup #f])
  ; Check whether parsing is available
  (prog->sollya exprs vars prec)
  #;(printf "Sollya program: ~a\n" (prog->sollya exprs vars prec))

  ; Create a process
  (define-values (process m-out m-in m-err) (subprocess #f #f #f sollya-path "--flush"))

  (define buffer (make-bytes 65536 0))

  (define machine (sollya-machine process m-out m-in m-err buffer exprs vars prec backup))

  ; Write the program to Sollya
  (sollya-write machine "~a\n" (prog->sollya exprs vars prec))

  (let loop ([i 0])
    (define step (read-bytes-avail! (sollya-machine-buffer machine) (sollya-machine-out machine) i))
    (define s (bytes->string/latin-1 (sollya-machine-buffer machine) #f 0 (+ i step)))
    (unless (regexp-match #rx"^The precision has been set to [0-9]+ bits.\n" s)
      (loop (+ i step))))
  machine)

;-------------------------------------- OUTPUT PARSING -----------------------------------------------
(define (seconds->ms seconds)
  (* (string->number seconds) 1000))

(define (parse-sollya-output machine timeout)
  (define start (current-inexact-milliseconds))
  (define <-bf bigfloat->flonum)
  (let loop ([i 0])
    (define step (read-bytes-avail!* (sollya-machine-buffer machine) (sollya-machine-out machine) i))
    (define s (bytes->string/latin-1 (sollya-machine-buffer machine) #f 0 (+ i step)))
    (cond
      ; Undefined
      [(regexp-match #rx"^Warning: the given expression is undefined or numerically unstable\n*" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (take-right (string-split s "\n") 2))
         (list dt (seconds->ms sollya-time) #f 'invalid))]

      ; NaN
      [(regexp-match #rx"^NaN\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) #f 'invalid))]
      [(regexp-match #rx"^\\[NaN;NaN\\]\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) #f 'invalid))]

      ; Infinity
      [(regexp-match #rx"^-?infty\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt
               (seconds->ms sollya-time)
               (if (string-contains? result "-")
                   (fl -inf.0)
                   (fl +inf.0))
               'valid))]

      ; Valid output
      [(regexp-match #rx"^[-+.e0-9]+\n[-+.e0-9]+\n$" s)
       (let ([dt (- (current-inexact-milliseconds) start)])
         (match-define (list result sollya-time) (string-split s "\n"))
         (list dt (seconds->ms sollya-time) (<-bf (bf result)) 'valid))]

      ; Timeout
      [(> (- (current-inexact-milliseconds) start) timeout)
       (when (not (equal? s ""))
         (eprintf "\nUnprocessed output from Sollya\n")
         (eprintf "Stdout number: ~s\n" s)
         (sollya-kill machine)
         (error "crashed"))
       (list timeout timeout #f 'exit)]

      [else (loop (+ i step))])))
