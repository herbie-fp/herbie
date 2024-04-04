#lang racket

(require math/bigfloat math/flonum rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

(define (log2-approx x)
  (define exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
  (if (equal? exp -9223372036854775807)
      (- (get-slack))  ; 0.bf
      (if (or (< 1000000000 (abs exp)))
          (get-slack)  ; overflow/inf.bf/nan.bf
          (+ exp 1)))) ; +1 because mantissa is not considered

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
;; name ::= 'fl or 'ival
(define (make-progs-interpreter name vars ivec roots)
  (define rootvec (list->vector roots))
  (define rootlen (vector-length rootvec))
  (define varc (length vars))
  (define vreg-count (+ varc (vector-length ivec)))
  (define vregs (make-vector vreg-count))
  (define vprecs (make-vector (vector-length ivec)))   ; vector that stores working precisions
  (define vstart-precs (setup-vstart-precs ivec varc)) ; starting precisions for the tuning mode
  (define prec-threshold (/ (*max-mpfr-prec*) 25))     ; parameter for sampling histogram table
  (if (equal? name 'ival)
      (λ args
        (match (*use-mixed-precision*)
          [#t (define timeline-stop! (timeline-start!/unsafe 'mixsample "backward-pass"
                                                             (* (*sampling-iteration*) 1000)))
              (if (equal? (*sampling-iteration*) 0)
                  (vector-copy! vprecs 0 vstart-precs) ; clear precisions from the last args
                  (backward-pass ivec varc vregs vprecs vstart-precs rootvec)) ; back-pass
              (timeline-stop!)]
          [#f (vector-fill! vprecs (bf-precision))])
        
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)] [precision (in-vector vprecs)])
          (define timeline-stop! (timeline-start!/unsafe 'mixsample
                                                         (symbol->string (object-name (car instr)))
                                                         (- precision (remainder precision prec-threshold))))
          (parameterize ([bf-precision precision])
            (vector-set! vregs n (apply-instruction instr vregs)))
          (timeline-stop!))
        
        (for/vector #:length rootlen ([root (in-vector rootvec)])
          (vector-ref vregs root)))
   
      ; name is 'fl
      (λ args
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)])
          (vector-set! vregs n (apply-instruction instr vregs)))
        (for/vector #:length rootlen ([root (in-vector rootvec)])
          (vector-ref vregs root)))))

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a)
     (op (vector-ref regs a))]
    [(list op a b)
     (op (vector-ref regs a)
         (vector-ref regs b))]
    [(list op a b c)
     (op (vector-ref regs a)
         (vector-ref regs b)
         (vector-ref regs c))]
    [(list op args ...)
     (apply op (map (curryr vector-ref regs) args))]))

(define (get-slack)
  (match (*sampling-iteration*)
    [0 256]
    [1 512]
    [2 1024]
    [3 2048]
    [4 4096]
    [5 8192]))

; Function sets up vstart-precs vector, where all the precisions
; are equal to (+ (*tuning-final-output-prec*) (* depth (*ground-truth-extra-bits*))),
; where depth is the depth of a node in the given computational tree (ivec)
(define (setup-vstart-precs ivec varc)
  (define ivec-len (vector-length ivec))
  (define vstart-precs (make-vector ivec-len))
  (unless (vector-empty? ivec)
    (for ([instr (in-vector ivec (- ivec-len 1) -1 -1)] ; reversed over ivec
          [n (in-range (- ivec-len 1) -1 -1)])          ; reversed over indices of vstart-precs
      (define current-prec (max (vector-ref vstart-precs n) (*tuning-final-output-prec*)))
      (vector-set! vstart-precs n current-prec)
    
      (define tail-registers (rest instr))
      (for ([idx (in-list tail-registers)])
        (when (>= idx varc)          ; if tail register is not a variable
          (define idx-prec (vector-ref vstart-precs (- idx varc)))
          (set! idx-prec (max        ; sometimes an instruction can be in many tail registers
                          idx-prec   ; We wanna make sure that we do not tune a precision down
                          (+ current-prec (*ground-truth-extra-bits*))))
          (vector-set! vstart-precs (- idx varc) idx-prec)))))
  vstart-precs)

;; Translates a Herbie IR into an interpretable IR.
;; Requires some hooks to complete the translation.
(define (make-compiler name
                       #:input->value input->value
                       #:op->procedure op->proc
                       #:op->itypes op->itypes
                       #:if-procedure if-proc)
  (lambda (exprs vars)
    ;; Instruction cache
    (define icache '())
    (define exprhash
      (make-hash
       (for/list ([var vars] [i (in-naturals)])
         (cons var i))))
    ; Counts
    (define size 0)
    (define exprc 0)
    (define varc (length vars))

    ; Translates programs into an instruction sequence of operations
    (define (munge prog)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?)
           (list (const (input->value prog 'real)))]
          [(literal value (app get-representation repr))
           (list (const (input->value value repr)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge c) (munge t) (munge f))]
          [(list op args ...)
           (cons (op->proc op) (map munge args))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                           (set! exprc (+ 1 exprc))
                           (set! icache (cons expr icache))))))
    
    (define names (map munge exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (make-progs-interpreter name vars ivec names)))

;; Compiles a program of operators into a procedure
;; that evaluates the program on a single input of intervals
;; returning intervals.
(define (compile-specs specs vars)
  ; strangeness with specs: need to check for `repr-conv?` operators
  ; normally we'd call `repr-conv?` from `src/syntax/syntax.rkt`
  ; but it will check the entire table of operators every call,
  ; so greedily compute the set ahead of time
  (define repr-convs (operator-all-impls 'cast))
  (define (real-op op)
    (if (member op repr-convs)
        (impl->operator op)
        op))

  (define compile
    (make-compiler 'ival
                   #:input->value
                   (lambda (prog _)
                     (define lo (parameterize ([bf-rounding-mode 'down]) (bf prog)))
                     (define hi (parameterize ([bf-rounding-mode 'up]) (bf prog)))
                     (ival lo hi))
                   #:op->procedure (lambda (op) (operator-info (real-op op) 'ival))
                   #:op->itypes (lambda (op) (operator-info (real-op op) 'itype))
                   #:if-procedure ival-if))
  (compile specs vars))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
(define (compile-progs exprs ctx)
  (define compile
    (make-compiler 'fl
                   #:input->value real->repr
                   #:op->procedure (lambda (op) (impl-info op 'fl))
                   #:op->itypes (lambda (op) (impl-info op 'itype))
                   #:if-procedure (λ (c ift iff) (if c ift iff))))
  (compile exprs (context-vars ctx)))

;; Like `compile-specs`, but for a single spec.
(define (compile-spec spec vars)
  (define core (compile-specs (list spec) vars))
  (define (<compiled-spec> . xs) (vector-ref (apply core xs) 0))
  <compiled-spec>)

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (<compiled-prog> . xs) (vector-ref (apply core xs) 0))
  <compiled-prog>)

(define (backward-pass ivec varc vregs vprecs vstart-precs roots)
  (vector-fill! vprecs 0)
  (for ([root-reg (in-vector roots)])
    (when (and
           (<= 0 (- root-reg varc))
           (not (equal? (vector-ref ivec (- root-reg varc)) const))
           (bigfloat? (ival-lo (vector-ref vregs root-reg))))
      (define result (vector-ref vregs root-reg))
      (when
          (equal? 1 (flonums-between
                     (bigfloat->flonum (ival-lo result))
                     (bigfloat->flonum (ival-hi result))))
        (vector-set! vprecs (- root-reg varc) (get-slack)))))
  
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)] ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)])         ; reversed over indices of vregs

    (define op (car instr)) ; current operation
    (define tail-registers (rest instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n)) ; output of the current instr
    
    (define exps-from-above (vector-ref vprecs (- n varc))) ; vprecs is shifted by varc elements from vregs
    (define new-exponents (get-exponent op output srcs))

    (define final-parent-precision (min (*max-mpfr-prec*)
                                        (+ exps-from-above
                                           (vector-ref vstart-precs (- n varc)))))

    ; This case is weird. Basically if we have cancellation inside fma - then multiplication in fma should be in higher precision
    (when (equal? op ival-fma)
      (set! final-parent-precision (+ final-parent-precision new-exponents)))
    
    (when (equal? final-parent-precision (*max-mpfr-prec*))
      (*sampling-iteration* (*max-sampling-iterations*)))
    (vector-set! vprecs (- n varc) final-parent-precision)
    
    (define child-exponents (+ exps-from-above new-exponents))
    (for-each (lambda (x) (when (>= x varc)  ; when tail register is not a variable
                            (vector-set! vprecs (- x varc)
                                         (max ; check whether this op already has a precision that is higher
                                          (vector-ref vprecs (- x varc))
                                          child-exponents))))
              tail-registers)))

(define (get-exponent op output srcs)
  (cond
    [(member op (list ival-mult ival-div ival-sqrt ival-cbrt))
     1]
    [(equal? op ival-add)
     ; log[Г+] = max(log[x], log[y]) - log[x + y]
     ;                               ^^^^^^^^^^^^
     ;                               possible cancellation
     (define x (first srcs))
     (define xlo (ival-lo x))
     (define xlo-exp (log2-approx xlo))
     (define xlo-sgn (bigfloat-signbit xlo))
     (define xhi (ival-hi x))
     (define xhi-exp (log2-approx xhi))
     (define xhi-sgn (bigfloat-signbit xhi))

     (define y (second srcs))
     (define ylo (ival-lo y))
     (define ylo-exp (log2-approx ylo))
     (define ylo-sgn (bigfloat-signbit ylo))
     (define yhi (ival-hi y))
     (define yhi-exp (log2-approx yhi))
     (define yhi-sgn (bigfloat-signbit yhi))
       
     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))
       
     (max 0 (+ 1 ; subtraction of logarithms doesn't consider mantissa - which can be +1 to the result
               (max
                (- (max xlo-exp ylo-exp) outlo-exp)
                (- (max xhi-exp yhi-exp) outhi-exp))
               (if (and (or (not (equal? xlo-sgn ylo-sgn)) ; slack part
                            (not (equal? xhi-sgn yhi-sgn)))
                        (or (>= 2 (abs (- xlo-exp ylo-exp)))
                            (>= 2 (abs (- xhi-exp yhi-exp)))))
                   (get-slack)
                   0)))]
      
    [(equal? op ival-sub)
     ; log[Г-] = max(log[x], log[y]) - log[x - y]
     ;                               ^^^^^^^^^^^^
     ;                               possible cancellation
     (define x (first srcs))
     (define xlo (ival-lo x))
     (define xlo-exp (log2-approx xlo))
     (define xlo-sgn (bigfloat-signbit xlo))
     (define xhi (ival-hi x))
     (define xhi-exp (log2-approx xhi))
     (define xhi-sgn (bigfloat-signbit xhi))

     (define y (second srcs))
     (define ylo (ival-lo y))
     (define ylo-exp (log2-approx ylo))
     (define ylo-sgn (bigfloat-signbit ylo))
     (define yhi (ival-hi y))
     (define yhi-exp (log2-approx yhi))
     (define yhi-sgn (bigfloat-signbit yhi))

     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))

     (max 0 (+ 1 ; subtraction of logarithms doesn't consider mantissa - which can be +1 to the result
               (max
                (- (max xlo-exp yhi-exp) outlo-exp)
                (- (max xhi-exp ylo-exp) outhi-exp))
               (if (and (or (equal? xlo-sgn yhi-sgn) ; slack part
                            (equal? xhi-sgn ylo-sgn))
                        (or (>= 2 (abs (- xlo-exp yhi-exp)))
                            (>= 2 (abs (- xhi-exp ylo-exp)))))
                   (get-slack)
                   0)))]
      
    [(equal? op ival-pow)
     ; log[Гpow] = max[ log(y) , log(y) + log[log(x)] ]
     ;                                    ^^^^^^^^^^^ less than 30
     (define xlo-exp (log2-approx (ival-lo (first srcs))))
     (define xhi-exp (log2-approx (ival-hi (first srcs))))
     (define ylo-exp (log2-approx (ival-lo (second srcs))))
     (define yhi-exp (log2-approx (ival-hi (second srcs))))

     (max 0 (+ (max ylo-exp yhi-exp)
               (if (> (max xlo-exp xhi-exp) 2) ; if x-exp > 2 (actually 2.718),
                   30                          ;    then at least 1 additional bit is needed
                   0)))]

    [(equal? ival-exp op)
     (max 0
          (log2-approx (ival-lo (car srcs)))
          (log2-approx (ival-hi (car srcs))))]

    [(equal? ival-tan op)
     ; log[Гtan] = log[x] - log[sin(x)*cos(x)] <= log[x] + |log[tan(x)]| + 1
     ;                                                      ^^^^^^^^^^^
     ;                                                 tan can be (-inf, +inf) or around to zero
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
       
     (define out-exp
       (+ 1 (max (abs (log2-approx outlo))
                 (abs (log2-approx outhi)))
          (if (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi))
              0                            ; both bounds are positive or negative
              (get-slack))))               ; tan is (-inf, +inf) or around zero

     (max 0
          (+ (log2-approx (ival-lo (car srcs))) out-exp)
          (+ (log2-approx (ival-hi (car srcs))) out-exp))]
              
    [(member op (list ival-sin ival-cos ival-sinh ival-cosh))
     ; log[Гcos] = log[x] + log[sin(x)] - log[cos(x)] <= log[x] - log[cos(x)]
     ; log[Гsin] = log[x] + log[cos(x)] - log[sin(x)] <= log[x] - log[sin(x)]
     ;                      ^^^^^^^^^^^                         ^^^^^^^^^^^^^
     ;                      can be pruned                       a possible uncertainty
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))

     (define out-exp
       (+ (min (log2-approx outlo)
               (log2-approx outhi))
          (if (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi))
              0                         ; both bounds are positive or negative
              (- (get-slack)))))        ; Condition of uncertainty,
     ; slack is negated because it is to be subtracted below
     (max 0 (+ 1 ; subtraction of logarithms doesn't consider mantissa - which can be +1 to the result
               (- (max
                   (log2-approx (ival-lo (car srcs)))
                   (log2-approx (ival-hi (car srcs))))
                  out-exp)))]
               
    [(member op (list ival-log ival-log2 ival-log10))
     ; log[Гlog]   = log[1/logx] = -log[log(x)]
     ; log[Гlog2]  = log[1/(log2(x) * ln(2))] <= -log[log2(x)] + 1    < main formula
     ; log[Гlog10] = log[1/(log10(x) * ln(10))] <= -log[log10(x)] - 1
     ;                    ^ a possible uncertainty
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
       
     (max 0
          (+ 1 (max (- (log2-approx outlo))  ; main formula
                    (- (log2-approx outhi)))
             (if (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi)) ; slack part
                 0                           ; both bounds are positive or negative
                 (get-slack))))]             ; output crosses 0.bf - uncertainty

          
              
    [(member op (list ival-asin ival-acos))
     ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)]
     ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)]
     ;                       ^^^^^^^^^^^^
     ;                       condition of uncertainty
     (define xlo (ival-lo (car srcs)))
     (define xhi (ival-hi (car srcs)))
     (define xlo-exp (log2-approx xlo))
     (define xhi-exp (log2-approx xhi))
     (define out-exp
       (+ (min                                   ; log[acos(x)|asin(x)] part
           (log2-approx (ival-lo output))      
           (log2-approx (ival-hi output)))
          (if (or (>= xlo-exp 0) (>= xhi-exp 0)) ; Condition of uncertainty when argument > sqrt(3)/2
              (- (get-slack))                    ; assumes that log[1-x^2]/2 is equal to (- slack)
              0)))
     (max 0 (- xlo-exp out-exp) (- xhi-exp out-exp))] ; main formula

    [(equal? op ival-atan)
     ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)] <= -|log[x]| - log[atan(x)] <= 0
     (define xlo-exp (- (abs (log2-approx (ival-lo (car srcs))))))
     (define xhi-exp (- (abs (log2-approx (ival-hi (car srcs))))))
     (max 0 ; never greater than 0...
          (- xlo-exp (log2-approx (ival-lo output)))
          (- xhi-exp (log2-approx (ival-hi output))))]
      
    [(member op (list ival-fmod ival-remainder))
     ; x mod y = x - y*q, where q is a coef
     ; log[Гmod] ~ log[ max(x, y*x/y) / mod(x,y)] ~ log[x] - log[mod(x,y)] + 1
     ;                            ^   ^
     ;                     conditions of uncertainty
     (define x (first srcs))
     (define xlo-exp (log2-approx (ival-lo x)))
     (define xhi-exp (log2-approx (ival-hi x)))
       
     (define y (second srcs))
     (define ylo (ival-lo y))
     (define yhi (ival-hi y))

     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))
       
     (max 0
          (+ (max (+ (- xlo-exp outlo-exp) 1)
                  (+ (- xhi-exp outhi-exp) 1))
       
             (if (and (xor (bigfloat-signbit ylo) (bigfloat-signbit yhi))
                      (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi)))
                 0                ; y and out don't cross 0
                 (get-slack))))]   ; y or output crosses 0
    [(equal? op ival-fma)
     ; log[Гfma] = log[ max(x*y, -z) / fma(x,y,z)] ~ max(log[x] + log[y], log[z]) - log[fma(x,y,z)] + 1
     ;                               ^^^^^^^^^^^^
     ;                               possible uncertainty
     (define x (first srcs))
     (define xlo-exp (log2-approx (ival-lo x)))
     (define xhi-exp (log2-approx (ival-hi x)))
       
     (define y (second srcs))
     (define ylo-exp (log2-approx (ival-lo y)))
     (define yhi-exp (log2-approx (ival-hi y)))

     (define z (third srcs))
     (define zlo-exp (log2-approx (ival-lo z)))
     (define zhi-exp (log2-approx (ival-hi z)))

     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))
       
     (define condition-lhs (+ 1 ; +1 because logarithms subtraction doesn't consider mantissa bits
                              (max (+ xlo-exp ylo-exp) ; max(log[x] + log[y], log[z])
                                   (+ xhi-exp yhi-exp)
                                   zlo-exp
                                   zhi-exp)))
     (max 0
          (- condition-lhs (min outlo-exp outhi-exp)
             (if (xor (bigfloat-signbit outhi) (bigfloat-signbit outlo)) ; cancellation when output crosses 0
                 0
                 (- (get-slack)))))]
    
    [(equal? op ival-hypot)
     ; hypot = sqrt(x^2+y^2)
     ; log[Гhypot] = log[ (2 * max(x,y) / hypot(x,y))^2 ] = 2 * (1 + log[max(x,y)] - log[hypot]) + 1
     ;                              ^ 
     ;                              a possible division by zero, catched by log2-approx's slack
     (define x (first srcs))
     (define xlo-exp (log2-approx (ival-lo x)))
     (define xhi-exp (log2-approx (ival-hi x)))
       
     (define y (second srcs))
     (define ylo-exp (log2-approx (ival-lo y)))
     (define yhi-exp (log2-approx (ival-hi y)))

     (define outlo-exp (log2-approx (ival-lo output)))
     (define outhi-exp (log2-approx (ival-hi output)))
     
     (max 0
          (+ 1 (* 2 (- ; division in condition number - +1 to the result
                     (+ (max xlo-exp ylo-exp xhi-exp yhi-exp) 1)
                     (min outlo-exp outhi-exp)))))]
    
    ; Currently log1p has a very poor approximation
    [(equal? op ival-log1p)
     ; log[Гlog1p] = log[x] - log[1+x] - log[log1p] + 1
     ;                      ^^^^^^^^^^
     ;                      treated like a slack if x < 0
     (define x (first srcs))
     (define xlo (ival-lo x))
     (define xlo-exp (log2-approx xlo))
     (define xhi (ival-hi x))
     (define xhi-exp (log2-approx xhi))

     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))
     
     (max 0
          (+ (- (+ (max xlo-exp xhi-exp) 1)                   ; main formula: log[x] - log[log1p] + 1
                (min outlo-exp outhi-exp))
             (if (or (equal? (bigfloat-signbit xlo) 1)        ; slack part
                     (equal? (bigfloat-signbit xhi) 1))       ; if x in negative
                 (get-slack)
                 0)))]
    
    ; Currently expm1 has a very poor solution for negative values
    [(equal? op ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1+log[x], 1+log[x/expm1] +1)
     ;                                                                    ^^ division accounting
     (define x (first srcs))
     (define xlo (ival-lo x))
     (define xlo-exp (log2-approx xlo))
     (define xhi (ival-hi x))
     (define xhi-exp (log2-approx xhi))
     (define xmax-exp (max xlo-exp xhi-exp))

     (define outlo (ival-lo output))
     (define outlo-exp (log2-approx outlo))
     (define outhi (ival-hi output))
     (define outhi-exp (log2-approx outhi))

     (max 0
          (+ 1 xmax-exp)
          (+ 2 (- xmax-exp (min outlo-exp outhi-exp))))]

    [(equal? op ival-atan2)
     ; log[Гatan2] = log[xy / ((x^2+y^2)*atan2)] <= log[x] + log[y] - 2*max[logx, logy] - log[atan2]
     (define x (first srcs))
     (define x-exp (max (log2-approx (ival-hi x))
                        (log2-approx (ival-lo x))))
     
     (define y (second srcs))
     (define y-exp (max (log2-approx (ival-hi y))
                        (log2-approx (ival-lo y))))
     
     (define out-exp (min (log2-approx (ival-lo output))
                          (log2-approx (ival-hi output))))

     (max 0
          (- (+ x-exp y-exp) (* 2 (max x-exp y-exp)) out-exp))]

    ; Currently has a poor implementation
    [(equal? op ival-tanh)
     ; log[Гtanh] = log[x / (sinh(x) * cosh(x))] <= -log[x] + log[tanh]. Never greater than 0
     (define x (first srcs))
     (define x-exp (min (log2-approx (ival-hi x))
                        (log2-approx (ival-lo x))))
     (define out-exp (max (log2-approx (ival-lo output))
                          (log2-approx (ival-hi output))))
     (max 0
          (+ (- x-exp) out-exp))]
    
    [(equal? op ival-atanh)
     ; log[Гarctanh] = log[x / ((1-x^2) * atanh)] = 1 if x < 0.5, otherwise slack
     ;                          ^^^^^^^
     ;                          a possible uncertainty
     (define x (first srcs))
     (define x-exp (max (log2-approx (ival-hi x))
                        (log2-approx (ival-lo x))))

     (if (>= x-exp 0)
         (get-slack)
         1)]
    
    ; TODO
    [(member op (list ival-erfc ival-erf ival-lgamma ival-tgamma))
     (get-slack)]
    [else 0]))
