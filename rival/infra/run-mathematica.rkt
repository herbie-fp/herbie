#lang racket

; Issues at 32000 with (λ (B x) ...)

(require math/bigfloat)
(require math/bigfloat)

(require "./interval-evaluate.rkt")
(require "./run-mpfi.rkt")

(define function->wolfram
  (make-hash `((pow . Power) (+ . Plus)
                             (- . Subtract)
                             (/ . Divide)
                             (sqrt . Sqrt)
                             (* . Times)
                             (fma . FMA)
                             (hypot . Hypot)
                             (exp . Exp)
                             (log . Log)
                             (log10 . Log10)
                             (log2 . Log2)
                             (cbrt . CubeRoot)
                             (sin . Sin)
                             (cos . Cos)
                             (tan . Tan)
                             (asin . ArcSin)
                             (acos . ArcCos)
                             (atan . ArcTan)
                             (sinh . Sinh)
                             (cosh . Cosh)
                             (tanh . Tanh)
                             (asinh . ArcSinh)
                             (acosh . ArcCosh)
                             (atanh . ArcTanh)
                             (atan2 . ArcTan)
                             (erf . Erf)
                             (erfc . Erfc)
                             (tgamma . Gamma)
                             (lgamma . LogGamma)
                             (ceil . Ceiling)
                             (floor . Floor)
                             (fmod . Mod)
                             (remainder . QuotientRemainer)
                             (fmax . Max)
                             (fmin . Min)
                             (truc . Truncate)
                             (round . Round)
                             (if . If)
                             (< . LessThan)
                             (> . GreaterThan)
                             (<= . LessEqual)
                             (>= . GreaterEqual)
                             (== . Equal)
                             (!= . NotEqual)
                             (and . And)
                             (or . Or)
                             (not . Not)
                             (neg . Minus)
                             (fabs . Abs))))

;; exp2, expm1,

(define (number->wolfram num)
  (define num2 (inexact->exact num))
  (format "Divide[~a, ~a]" (numerator num2) (denominator num2)))

(define (expr->wolfram expr)
  (match expr
    [(list op (app expr->wolfram args) ...)
     (define fn (hash-ref function->wolfram op))
     (format "checkReal[~a[~a]]" fn (string-join args ", "))]
    ['PI "Pi"]
    ['E "E"]
    [(? symbol?) (regexp-replace #rx"[*._-]" (symbol->string expr) "AWeirdSymbol")]
    [(? number?) (number->wolfram expr)]))

(define (program->wolfram prog)
  (format "f[~a] := ~a\n"
          (string-join (map (compose (curry format "~a_") expr->wolfram) (program-variables prog))
                       ", ")
          (expr->wolfram (program-body prog))))

(define (load-points port)
  (define points
    (for/list ([read-res (in-port read port)])
      read-res))
  (for/hash ([group (group-by second points)])
    (values (second (car group)) (map third group))))

(define math-path (find-executable-path "math"))

(define (make-mathematica prog #:backup [backup #f])
  (define-values (process m-out m-in m-err) (subprocess #f #f #f math-path))

  (define buffer (make-bytes 65536 0))

  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup
      (apply fprintf backup fmt vs))
    (flush-output m-in))

  (ffprintf "~a\n" (call-with-input-file "headers.wls" port->string))
  (ffprintf "~a\n" (program->wolfram prog))
  (ffprintf "Print[\"Rival\" <> \"Ready\"]\n")
  (let loop ([i 0])
    (define step (read-bytes-avail! buffer m-out i))
    (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
    (if (regexp-match #rx".*RivalReady.*In\\[[0-9]+\\].*" s)
        (eprintf "Mathematica started for ~a\n" prog)
        (loop (+ i step))))

  (values process m-out m-in m-err))

(define (run-mathematica prog pts #:backup [backup #f])
  (define-values (process m-out m-in m-err) (make-mathematica prog #:backup backup))

  (define buffer (make-bytes 65536 0))
  (define (ffprintf fmt . vs)
    (apply fprintf m-in fmt vs)
    (when backup
      (apply fprintf backup fmt vs))
    (flush-output m-in))

  (define out
    (for/list ([pt (in-list pts)])
      (define start (current-inexact-milliseconds))
      (ffprintf "TimeConstrained[FullForm[N[f[~a], 20]], 1]\n"
                (string-join (map number->wolfram pt) ", "))
      (let loop ([i 0])
        (define step (read-bytes-avail!* buffer m-out i))
        (define s (bytes->string/latin-1 buffer #f 0 (+ i step)))
        (cond
          [(> (- (current-inexact-milliseconds) start) 2000.0)
           (eprintf "Killing and restarting Mathematica\n")
           (eprintf "~s\n" s)
           (define-values (process2 m-out2 m-in2 m-err2) (make-mathematica prog #:backup backup))
           (subprocess-kill process true)
           (set! process process2)
           (set! m-out m-out2)
           (set! m-in m-in2)
           (set! m-err m-err2)
           (cons 10000.0 'timeout)]
          [(string-contains? s "\nIn")
           (let ([dt (- (current-inexact-milliseconds) start)])
             (printf ".")
             (flush-output)
             (cons dt (parse-output s)))]
          [else (loop (+ i step))]))))
  (ffprintf "Exit[]\n")
  (subprocess-wait process)
  (cons (subprocess-status process) out))

(define (parse-output s)
  (define lines (string-split s "\n" #:repeat? #t))
  (with-handlers ([exn:misc:match? (λ (e)
                                     (newline)
                                     (printf "Could not parse results:\n")
                                     (pretty-print lines)
                                     (exit))])

    (match-lines lines)))

(define (match-lines lines)
  (match lines
    [(list (regexp #rx"In\\[[0-9]+\\]:= ") rest ...) (match-lines rest)]
    [(list rest ... (regexp #rx"In\\[[0-9]+\\]:= ")) (match-lines rest)]
    [(list (regexp #rx"Out\\[[0-9]+\\]= \\$Aborted")) 'timeout]
    [(list (regexp #rx"Out\\[[0-9]+\\]//FullForm= (.+)" (list _ x))) x]
    [(list (regexp #rx"Out\\[[0-9]+\\]//FullForm= ") " " (regexp #rx"> +[0-9-](.+)" (list _ x))) x]
    [(list "Throw::nocatch: Uncaught Throw[domain-error, BadValue] returned to top level." _ ...)
     'invalid]
    [(list "General::ovfl: Overflow occurred in computation." _ ...) 'unsamplable]
    [(list "General::unfl: Underflow occurred in computation." _ ...) 'unsamplable]
    [(list "General::nomem: "
           "   The current computation was aborted because there was insufficient memory"
           "    available to complete the computation."
           _ ...)
     (eprintf "Mathematica ran out of memory!\n")
     'memory]
    [(list _ ... (regexp #rx"Divide::indet: Indeterminate expression .*") _ ...) 'unsamplable]
    [(list _ ... (regexp #rx"Divide::infy: Infinite expression .*") _ ...) 'unsamplable]
    [(list "Divide::infy: Infinite expression " _ ...) 'unsamplable]
    [(list _ ... (regexp #rx"Power::infy: Infinite expression .*") _ ...) 'unsamplable]
    [(list _ ... (regexp #rx"Infinity::indet: Indeterminate expression .*") _ ...) 'unsamplable]
    [(list _ ... "Infinity::indet: " _ ... (regexp #rx"Indeterminate expression .*") _ ...)
     'unsamplable]
    [(list _ ... (regexp #rx"ArcTan::indet: Indeterminate expression .*") _ ...) 'unsamplable]
    [(list (regexp #rx"General::stop: Further output of .*")
           (regexp #rx" +will be suppressed during this calculation")
           _ ...)
     'unsamplable]
    [(list "N::meprec: Internal precision limit $MaxExtraPrecision = 3100." _ ...) 'unknown]))

(define (count-results out)
  (define sampled 0)
  (define invalid 0)
  (define unsamplable 0)
  (define unknown 0)
  (define crash 0)
  (define timeout 0)
  (for ([val out])
    (match (cdr val)
      ['invalid (set! invalid (add1 invalid))]
      ['memory
       (set! unknown (add1 unknown))
       (set! crash (add1 crash))]
      ['timeout
       (set! unknown (add1 unknown))
       (set! timeout (add1 timeout))]
      ['unsamplable (set! unsamplable (add1 unsamplable))]
      ['unknown (set! unknown (add1 unknown))]
      [(regexp #rx"[0-9]+(\\.[0-9]+)?(`[0-9]*)?(\\*\\^-?[0-9]+)?") (set! sampled (add1 sampled))]))
  (list sampled invalid unsamplable unknown crash timeout))

(define (add-results r1 r2)
  (map + r1 r2))

(define (print-results results)
  (match-define (list sampled invalid unsamplable unknown crash timeout) results)
  (eprintf "\nResults: ~a ok, ~a bad, ~a unsamplable, ~a unknown (~a crash, ~a timeout)\n"
           sampled
           invalid
           unsamplable
           unknown
           crash
           timeout))

(define (go points skip)
  (define results (list 0 0 0 0 0 0))
  (for ([(prog pts*) (in-hash points)])
    (call-with-output-file "mathematica.log"
                           #:exists 'replace
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

(module+ main
  (define skip 0)
  (command-line #:program "run-mathematica"
                #:once-each
                [("--skip") n "How many points to skip" (set! skip (or (string->number n) skip))]
                #:args (points-file)
                (define points (call-with-input-file points-file load-points))
                (go points skip)))
