#lang racket

(require math/bigfloat math/flonum rival)
;; Faster than bigfloat-exponent and avoids an expensive offset & contract check.
(require (only-in math/private/bigfloat/mpfr mpfr-exp mpfr-sign))
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

(define (log2-approx x)
  (define exp (mpfr-exp x))
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
  (define iveclen (vector-length ivec))
  (define varc (length vars))
  (define vreg-count (+ varc iveclen))
  (define vregs (make-vector vreg-count))
  (define vrepeats (make-vector iveclen #f))           ; flags whether an op should be evaluated
  (define vprecs (make-vector iveclen))                ; vector that stores working precisions
  (define vstart-precs (setup-vstart-precs ivec varc)) ; starting precisions for the tuning mode

  (define prec-threshold (/ (*max-mpfr-prec*) 25))     ; parameter for sampling histogram table
  (if (equal? name 'ival)
      (λ args
        (define timeline-stop! (timeline-start!/unsafe 'mixsample "backward-pass"
                                                       (* (*sampling-iteration*) 1000)))
        (if (zero? (*sampling-iteration*))
            (vector-fill! vrepeats #f)
            (backward-pass ivec varc vregs vprecs vstart-precs rootvec rootlen vrepeats)) ; back-pass
        (timeline-stop!)
        
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)]
                                       [precision (in-vector (if (zero? (*sampling-iteration*)) vstart-precs vprecs))]
                                       [repeat (in-vector vrepeats)]
                                       #:unless repeat)
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
; are equal to (+ (*base-tuning-precision*) (* depth (*ampl-tuning-bits*))),
; where depth is the depth of a node in the given computational tree (ivec)
(define (setup-vstart-precs ivec varc)
  (define ivec-len (vector-length ivec))
  (define vstart-precs (make-vector ivec-len))
  (unless (vector-empty? ivec)
    (for ([instr (in-vector ivec (- ivec-len 1) -1 -1)] ; reversed over ivec
          [n (in-range (- ivec-len 1) -1 -1)])          ; reversed over indices of vstart-precs
      (define current-prec (max (vector-ref vstart-precs n) (*base-tuning-precision*)))
      (vector-set! vstart-precs n current-prec)
    
      (define tail-registers (rest instr))
      (for ([idx (in-list tail-registers)])
        (when (>= idx varc)          ; if tail register is not a variable
          (define idx-prec (vector-ref vstart-precs (- idx varc)))
          (set! idx-prec (max        ; sometimes an instruction can be in many tail registers
                          idx-prec   ; We wanna make sure that we do not tune a precision down
                          (+ current-prec (*ampl-tuning-bits*))))
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
      (define instruction ; This compiles to the register machine
        (match prog
          [(? number?) prog]
          [(? literal?) prog]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list 'if (munge c) (munge t) (munge f))]
          [(list op args ...)
           (cons op (map munge args))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash instruction
                 (λ ()
                   (define expr ; Think links to actual functions to execute
                     (match instruction
                       [(? number? value)
                        (list (const (input->value value 'real)))]
                       [(literal value (app get-representation repr))
                        (list (const (input->value value repr)))]
                       ;No (? variable? var) case, already in the cache
                       [`(if ,c ,t ,f) (list if-proc c t f)]
                       [(list op args ...) (cons (op->proc op) args)]))
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

; Function writes into vprecs new precisions that are propogated using condition number logic
;   and into vrepeats whether a reevaluation is needed for an instruction
(define (backward-pass ivec varc vregs vprecs vstart-precs rootvec rootlen vrepeats)
  (define vprecs-new (make-vector (vector-length ivec) 0))          ; new vprecs vector
  ; Step 1. Adding slack in case of a rounding boundary issue
  (for/vector #:length rootlen ([root-reg (in-vector rootvec)])
    (when (and
           (<= varc root-reg)                                       ; when root is not a variable
           (bigfloat? (ival-lo (vector-ref vregs root-reg))))       ; when root is a real op
      (define result (vector-ref vregs root-reg))
      (when
          (equal? 1 (flonums-between
                     (bigfloat->flonum (ival-lo result))
                     (bigfloat->flonum (ival-hi result))))
        (vector-set! vprecs-new (- root-reg varc) (get-slack)))))

  ; Since Step 2 writes into *sampling-iteration* if the max prec was reached - save the iter number for step 3
  (define current-iter (*sampling-iteration*))
  
  ; Step 2. Exponents calculation
  (exponents-propogation ivec vregs vprecs-new varc vstart-precs)
  
  ; Step 3. Repeating precisions check
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (for ([instr (in-vector ivec)]
        [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
        [prec-new (in-vector vprecs-new)]
        [n (in-naturals)])
    (vector-set! vrepeats n  (and (equal? prec-new prec-old)
                                  (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc))))
                                          (rest instr)))))
        
  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new))

; This function goes through ivec and vregs and calculates (+ exponents base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-new[i] = min( *max-mpfr-prec* max( *base-tuning-precision* (+ exponents-from-above vstart-precs[i])),
;   exponents-from-above = get-exponent(parent)
(define (exponents-propogation ivec vregs vprecs-new varc vstart-precs)
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]   ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)])           ; reversed over indices of vregs

    (define op (car instr))                                         ; current operation
    (define tail-registers (rest instr))
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n))                            ; output of the current instr
    
    (define exps-from-above (vector-ref vprecs-new (- n varc)))     ; vprecs-new is shifted by varc elements from vregs
    (define new-exponents (get-exponent op output srcs))

    (define final-parent-precision (min (*max-mpfr-prec*)
                                        (max (+ exps-from-above
                                                (vector-ref vstart-precs (- n varc)))
                                             (vector-ref vstart-precs (- n varc)))))

    ; This case is weird. if we have a cancellation in fma -> ival-mult in fma should be in higher precision
    (when (equal? op ival-fma)
      (set! final-parent-precision (+ final-parent-precision (first new-exponents))))
    
    (when (equal? final-parent-precision (*max-mpfr-prec*))         ; Early stopping
      (*sampling-iteration* (*max-sampling-iterations*)))
    (vector-set! vprecs-new (- n varc) final-parent-precision)

    (for ([x (in-list tail-registers)]
          [new-exp (in-list new-exponents)]
          #:when (>= x varc)) ; when tail register is not a variable
      (when (> (+ exps-from-above new-exp (vector-ref vstart-precs (- x varc))) (vector-ref vprecs-new (- x varc))) ; check whether this op already has a precision that is higher
        (vector-set! vprecs-new (- x varc) (+ exps-from-above new-exp))))))

(define (ival-max-log2-approx x)
  (max (log2-approx (ival-hi x)) (log2-approx (ival-lo x))))
(define (ival-min-log2-approx x)
  (min (log2-approx (ival-hi x)) (log2-approx (ival-lo x))))

; Function calculates an exponent per input for a certain output and inputs using condition formulas,
;   where an exponent is an additional precision that needs to be added to srcs evaluation so,
;   that the output will be fixed in its precision when evaluating again
(define (get-exponent op output srcs)
  (case (object-name  op)
    [(ival-mult ival-div ival-sqrt ival-cbrt)
     ; log[Г*]'x = log[Г*]'y = log[Г/]'x = log[Г/]'y = 1
     ; log[Гsqrt] = 0.5
     ; log[Гcbrt] = 0.3
     (make-list (length srcs) 0)]                     ; assume that *ampl-bits* already introduces this 1 bit
    
    [(ival-add ival-sub)
     ; log[Г+]'x = log[x] - log[x + y] + 1 (1 for mantissa approximation when dividing)
     ; log[Г+]'y = log[y] - log[x + y] + 1
     ; log[Г-]'x = log[x] - log[x - y] + 1
     ; log[Г-]'y = log[y] - log[x - y] + 1
     ;                    ^^^^^^^^^^^^
     ;                    possible cancellation
     (define x (first srcs))
     (define y (second srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                       0
                       (get-slack)))
     
     (list (+ (- x-exp out-exp) slack 1)              ; exponent per x
           (+ (- y-exp out-exp) slack 1))]            ; exponent per y 
    
    [(ival-pow)
     ; log[Гpow]'x = log[y]
     ; log[Гpow]'y = log[y] + log[log(x)]
     ;                        ^^^^^^^^^^^
     ;                        always less than 30
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define slack (if (>= x-exp 3)                   ; if x > 2 (actually 2.718),
                       30                             ; then at least 1 additional bit is needed
                       0)) 
     
     (list y-exp                                      ; exponent per x
           (+ y-exp slack))]                          ; exponent per y
    
    [(ival-exp)
     ; log[Гexp] = log[x]
     (define x (car srcs))
     (define x-exp (ival-max-log2-approx x))
     (list x-exp)]
    
    [(ival-tan)
     ; log[Гtan] = log[x] - log[sin(x)*cos(x)] <= log[x] + |log[tan(x)]| + 1
     ;                                                      ^^^^^^^^^^^
     ;                                                      possible uncertainty
     (define x (first srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp-abs (max (abs (log2-approx outlo))
                              (abs (log2-approx outhi))))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                        (>= x-exp 2))                 ; x >= 1.bf [log2-approx(1.bf) = 2], ideally x > pi.bf
                       (get-slack)                    ; tan is (-inf, +inf) or around zero (but x != 0)
                       0))
     
     (list (+ x-exp out-exp-abs 1 slack))]
    
    [(ival-sin ival-cos ival-sinh ival-cosh)
     ; log[Гcos] = log[x] + log[sin(x)] - log[cos(x)] <= log[x] - log[cos(x)] + 1
     ; log[Гsin] = log[x] + log[cos(x)] - log[sin(x)] <= log[x] - log[sin(x)] + 1
     ;                                                          ^^^^^^^^^^^^^
     ;                                                          a possible uncertainty
     (define x (first srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (and
                        (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                        (>= x-exp 2))                 ; if x >= 1 [log2-approx(1.bf) = 2], when x=0 - no slack needed
                       (get-slack)                    ; Condition of uncertainty
                       0))
     
     (list (+ (- x-exp out-exp) slack 1))]
    
    [(ival-log ival-log2 ival-log10)
     ; log[Гlog]   = log[1/logx] = -log[log(x)]
     ; log[Гlog2]  = log[1/(log2(x) * ln(2))] <= -log[log2(x)] + 1    < main formula
     ; log[Гlog10] = log[1/(log10(x) * ln(10))] <= -log[log10(x)] - 1
     ; log[Гlog10] < log[Гlog] < log[Гlog2]
     
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     (define out-exp (- (ival-min-log2-approx output))) ; -log[log10(x)]
     
     (define slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                       0
                       (get-slack)))                  ; output crosses 0 - uncertainty
     
     (list (+ out-exp 1 slack))]
    
    [(ival-asin ival-acos)
     ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)] + 1
     ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)] + 1
     ;                       ^^^^^^^^^^^^
     ;                       condition of uncertainty
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (>= x-exp 1)                   ; Condition of uncertainty when argument > sqrt(3)/2
                       (get-slack)                    ; assumes that log[1-x^2]/2 is equal to slack
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]
    
    [(ival-atan)
     ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)] <= -|log[x]| - log[atan(x)] <= 0
     (define x (first srcs))
     
     (define x-exp-abs (max (abs (log2-approx (ival-hi x)))
                            (abs (log2-approx (ival-lo x)))))
     (define out-exp (ival-min-log2-approx output))
     
     (list (- (- x-exp-abs) out-exp))]
    
    [(ival-fmod ival-remainder)
     ; x mod y = x - y*q, where q is rnd_down(x/y)
     ; log[Гmod]'x ~ log[x]                 - log[mod(x,y)] + 1
     ; log[Гmod]'y ~ log[y * rnd_down(x/y)] - log[mod(x,y)] + 1 <= log[x] - log[mod(x,y)] + 1
     ;                                 ^    ^
     ;                                 conditions of uncertainty
     (define x (first srcs))
     (define y (second srcs))
     (define ylo (ival-lo y))
     (define yhi (ival-hi y))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define x-slack (if (equal? (mpfr-sign outlo) (mpfr-sign outhi))
                         0                
                         (get-slack)))                ; output crosses 0
     
     (define y-slack (if (equal? (mpfr-sign ylo) (mpfr-sign yhi))
                         x-slack                
                         (+ x-slack (get-slack))))    ; y crosses zero
     
     (list (+ (- x-exp out-exp) 1 x-slack)            ; exponent per x
           (+ (- x-exp out-exp) 1 y-slack))]          ; exponent per y
    
    [(ival-fma)
     ; log[Гfma] = log[ max(x*y, -z) / fma(x,y,z)] ~ max(log[x] + log[y], log[z]) - log[fma(x,y,z)] + 1
     ;                               ^^^^^^^^^^^^
     ;                               possible uncertainty
     (define x (first srcs))
     (define y (second srcs))
     (define z (third srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define z-exp (ival-max-log2-approx z))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (equal? (mpfr-sign outhi) (mpfr-sign outlo))
                                0
                                (get-slack)))         ; cancellation when output crosses 0
     
     (define lhs-exp (max (+ x-exp y-exp)             ; max(log[x] + log[y], log[z])
                          z-exp))
     (make-list 3 (+ (- lhs-exp out-exp) 1 slack))]
    
    [(ival-hypot)
     ; hypot = sqrt(x^2+y^2)
     ; log[Гhypot] = log[ (2 * max(x,y) / hypot(x,y))^2 ] = 2 * (1 + log[max(x,y)] - log[hypot]) + 1
     ;                                  ^
     ;                                  a possible division by zero, catched by log2-approx's slack
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (make-list 2 (+ (* 2 (- (+ 1 (max x-exp y-exp)) out-exp)) 1))]
    
    ; Currently log1p has a very poor approximation
    [(ival-log1p)
     ; log[Гlog1p] = log[x] - log[1+x] - log[log1p] + 1
     ;                      ^^^^^^^^^^
     ;                      treated like a slack if x < 0
     (define x (first srcs))
     (define xhi (ival-hi x))
     (define xlo (ival-lo x))
     
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (define slack (if (or (equal? (mpfr-sign xlo) -1)
                           (equal? (mpfr-sign xhi) -1))
                       (get-slack)                    ; if x in negative
                       0))
     
     (list (+ (- x-exp out-exp) 1 slack))]
    
    ; Currently expm1 has a very poor solution for negative values
    [(ival-expm1)
     ; log[Гexpm1] = log[x * e^x / expm1] <= max(1+log[x], 1+log[x/expm1] +1)
     ;                                                                    ^^ division accounting
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     (define out-exp (ival-min-log2-approx output))
     
     (list (+ 1 x-exp) (+ 2 (- x-exp out-exp)))]
    
    [(ival-atan2)
     ; log[Гatan2]'x = log[Гatan2]'y = log[xy / ((x^2+y^2)*atan2)] <= log[x] + log[y] - 2*max[logx, logy] - log[atan2]
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (make-list 2 (- (+ x-exp y-exp) (* 2 (max x-exp y-exp)) out-exp))]
    
    ; Currently has a poor implementation
    [(ival-tanh)
     ; log[Гtanh] = log[x / (sinh(x) * cosh(x))] <= -log[x] + log[tanh]. Never greater than 0
     (define x (first srcs))
     (define x-exp (ival-min-log2-approx x))
     (define out-exp (ival-max-log2-approx output))
     
     (list (+ (- x-exp) out-exp))]
    
    [(ival-atanh)
     ; log[Гarctanh] = log[x / ((1-x^2) * atanh)] = 1 if x < 0.5, otherwise slack
     ;                          ^^^^^^^
     ;                          a possible uncertainty
     (define x (first srcs))
     (define x-exp (ival-max-log2-approx x))
     
     (list (if (>= x-exp 1)
               (get-slack)
               1))]
    
    [(ival-acosh)
     ; log[Гacosh] = log[x / (sqrt(x-1) * sqrt(x+1) * acosh)] <= -log[acosh] + slack
     (define out-exp (ival-min-log2-approx output))
     (define slack (if (< out-exp 2)                 ; when acosh(x) < 1
                       (get-slack)
                       0))
     
     (list (+ (- out-exp) slack))]
    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma)
     (list (get-slack))]
    [else (make-list (length srcs) 0)]))
