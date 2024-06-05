#lang racket

(require math/bigfloat (only-in math/flonum flonums-between) rival)
(require (only-in math/private/bigfloat/mpfr mpfr-exp mpfr-sign))
;; Faster than bigfloat-exponent and avoids an expensive offset & contract check.

(provide rival-machine-load rival-machine-run rival-machine-return rival-machine-adjust
         compile-specs (struct-out my-discretization) (struct-out rival-machine)
         *sampling-iteration* *my-rival-max-precision* *my-rival-max-iterations*
         *my-rival-profile-executions*)

(define *my-rival-max-precision* (make-parameter 10000))
(define *my-rival-max-iterations* (make-parameter 5))
(define *my-rival-profile-executions* (make-parameter 1000))

(define *ampl-tuning-bits* (make-parameter 5))
(define *sampling-iteration* (make-parameter 0))
(define *base-tuning-precision* (make-parameter 73))

(struct my-discretization (convert distance))

(struct rival-machine
  (arguments instructions outputs discs
   registers repeats precisions initial-precisions
   [iteration #:mutable] [bumps #:mutable]
   [profile-ptr #:mutable]
   profile-instruction profile-number profile-time profile-precision))

(define (rival-machine-load machine args)
  (vector-copy! (rival-machine-registers machine) 0 args)
  #;(set-rival-machine-iteration! machine 0))

(define (rival-machine-record machine name number precision time)
  (define profile-ptr (rival-machine-profile-ptr machine))
  (define profile-instruction (rival-machine-profile-instruction machine))
  (when (< profile-ptr (vector-length profile-instruction))
    (define profile-number (rival-machine-profile-number machine))
    (define profile-time (rival-machine-profile-time machine))
    (define profile-precision (rival-machine-profile-precision machine))
    (vector-set! profile-instruction profile-ptr name)
    (vector-set! profile-number profile-ptr number)
    (vector-set! profile-precision profile-ptr precision)
    (vector-set! profile-time profile-ptr time)
    (set-rival-machine-profile-ptr! machine (add1 profile-ptr))))

(define (rival-machine-run machine)
  (define ivec (rival-machine-instructions machine))
  (define varc (vector-length (rival-machine-arguments machine)))
  (define precisions
    (if (zero? (rival-machine-iteration machine))
        (rival-machine-initial-precisions machine)
        (rival-machine-precisions machine)))
  (define repeats (rival-machine-repeats machine))
  (define vregs (rival-machine-registers machine))

  ; parameter for sampling histogram table
  (define first-iter? (zero? (rival-machine-iteration machine)))

  (for ([instr (in-vector ivec)]
        [n (in-naturals varc)]
        [precision (in-vector precisions)]
        [repeat (in-vector repeats)]
        #:unless (and (not first-iter?) repeat))
    (define start (current-inexact-milliseconds))
    (parameterize ([bf-precision precision])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (define name (object-name (car instr)))
    (define time (- (current-inexact-milliseconds) start))
    (rival-machine-record machine name n precision time)))

(define (rival-machine-return machine)
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define rootvec (rival-machine-outputs machine))
  #;(set-rival-machine-iteration! machine (add1 (rival-machine-iteration machine)))
  (for/vector #:length (vector-length rootvec)
              ([root (in-vector rootvec)] [disc (in-vector discs)])
    (vector-ref vregs root)))

(define (rival-machine-adjust machine)
  (define iter (rival-machine-iteration machine))
  (unless (zero? iter)
    (define start (current-inexact-milliseconds))
    (backward-pass machine)
    (rival-machine-record machine 'adjust 0 (* iter 1000) (- (current-inexact-milliseconds) start))))

(define (rival-machine-full machine args)
  (set-rival-machine-iteration! machine (*sampling-iteration*))
  (rival-machine-adjust machine)
  (rival-machine-load machine args)
  (rival-machine-run machine)
  (rival-machine-return machine))

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

(define (progs->batch exprs vars)
  (define icache (reverse vars))
  (define exprhash
    (make-hash
     (for/list ([var vars] [i (in-naturals)])
       (cons var i))))
  ; Counts
  (define exprc 0)
  (define varc (length vars))

  ; Translates programs into an instruction sequence of operations
  (define (munge prog)
    (define node ; This compiles to the register machine
      (match prog
        [(list op args ...)
         (cons op (map munge args))]
        [_
         prog]))
    (hash-ref! exprhash node
               (lambda ()
                 (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                   (set! exprc (+ 1 exprc))
                   (set! icache (cons node icache))))))

  (define roots (list->vector (map munge exprs)))
  (define nodes (list->vector (reverse icache)))

  (values nodes roots))

(define (ival-infinity)
  (ival (bfprev +inf.bf) +inf.bf))

(define (ival-nan)
  ival-illegal)

(define (ival-true)
  (ival-bool true))

(define (ival-false)
  (ival-bool false))

(define (compile-specs exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots)
    (progs->batch exprs vars))

  (define instructions
    (for/vector #:length (- (vector-length nodes) num-vars)
                ([node (in-vector nodes num-vars)])
      (match node
        [(? number?)
         (define x (real->ival node))
         (if (point-ival? x)
             (list (const x))
             (list (lambda () (real->ival node))))]

        [(list 'PI)     (list ival-pi)]
        [(list 'E)      (list ival-e)]
        [(list 'INFINITY) (list ival-infinity)]
        [(list 'NAN)    (list ival-nan)]
        [(list 'TRUE)   (list ival-true)]
        [(list 'FALSE)  (list ival-false)]

        [(list 'if c y f) (list ival-if c y f)]

        [(list 'neg x)   (list ival-neg x)]
        [(list 'acos x)  (list ival-acos x)]
        [(list 'acosh x) (list ival-acosh x)]
        [(list 'asin x)  (list ival-asin x)]
        [(list 'asinh x) (list ival-asinh x)]
        [(list 'atan x)  (list ival-atan x)]
        [(list 'atanh x) (list ival-atanh x)]
        [(list 'cbrt x)  (list ival-cbrt x)]
        [(list 'ceil x)  (list ival-ceil x)]
        [(list 'cos x)   (list ival-cos x)]
        [(list 'cosh x)  (list ival-cosh x)]
        [(list 'erf x)   (list ival-erf x)]
        [(list 'erfc x)  (list ival-erfc x)]
        [(list 'exp x)   (list ival-exp x)]
        [(list 'exp2 x)  (list ival-exp2 x)]
        [(list 'expm1 x) (list ival-expm1 x)]
        [(list 'fabs x)  (list ival-fabs x)]
        [(list 'floor x) (list ival-floor x)]
        [(list 'lgamma x) (list ival-lgamma x)]
        [(list 'log x)   (list ival-log x)]
        [(list 'log10 x) (list ival-log10 x)]
        [(list 'log1p x) (list ival-log1p x)]
        [(list 'log2 x)  (list ival-log2 x)]
        [(list 'logb x)  (list ival-logb x)]
        [(list 'rint x)  (list ival-rint x)]
        [(list 'round x) (list ival-round x)]
        [(list 'sin x)   (list ival-sin x)]
        [(list 'sinh x)  (list ival-sinh x)]
        [(list 'sqrt x)  (list ival-sqrt x)]
        [(list 'tan x)   (list ival-tan x)]
        [(list 'tanh x)  (list ival-tanh x)]
        [(list 'tgamma x) (list ival-tgamma x)]
        [(list 'trunc x) (list ival-trunc x)]

        [(list '+ x y)     (list ival-add x y)]
        [(list '- x y)     (list ival-sub x y)]
        [(list '* x y)     (list ival-mult x y)]
        [(list '/ x y)     (list ival-div x y)]
        [(list 'atan2 x y) (list ival-atan2 x y)]
        [(list 'copysign x y) (list ival-copysign x y)]
        [(list 'hypot x y) (list ival-hypot x y)]
        [(list 'fdim x y)  (list ival-fdim x y)]
        [(list 'fmax x y)  (list ival-fmax x y)]
        [(list 'fmin x y)  (list ival-fmin x y)]
        [(list 'fmod x y)  (list ival-fmod x y)]
        [(list 'pow x y)   (list ival-pow x y)]
        [(list 'remainder x y) (list ival-remainder x y)]

        [(list 'fma x y z) (list ival-fma x y z)]

        [(list '== x y) (list ival-== x y)]
        [(list '!= x y) (list ival-!= x y)]
        [(list '<= x y) (list ival-<= x y)]
        [(list '>= x y) (list ival->= x y)]
        [(list '< x y)  (list ival-< x y)]
        [(list '> x y)  (list ival-> x y)]

        [(list 'not x)   (list ival-not x)]
        [(list 'and x y) (list ival-and x y)]
        [(list 'or x y)  (list ival-or x y)]

        [(list 'cast x)  (list identity x)]

        [(list 'assert x)  (list ival-assert x)]

        [(list op args ...)
         (error 'compile-specs "Unknown operator ~a" op)])))

  (define register-count (+ (length vars) (vector-length instructions)))
  (define registers (make-vector register-count))
  (define repeats (make-vector register-count #f)) ; flags whether an op should be evaluated
  (define precisions (make-vector register-count)) ; vector that stores working precisions
  ;; starting precisions for the first, un-tuned iteration
  (define initial-precisions (setup-vstart-precs instructions (length vars)))

  (rival-machine
   (list->vector vars) instructions roots (list->vector discs)
   registers repeats precisions initial-precisions
   0 0 0
   (make-vector (*my-rival-profile-executions*))
   (make-vector (*my-rival-profile-executions*))
   (make-vector (*my-rival-profile-executions*))
   (make-vector (*my-rival-profile-executions*))))

(define (real->ival val)
  (define lo (parameterize ([bf-rounding-mode 'down]) (bf val)))
  (define hi (parameterize ([bf-rounding-mode 'up]) (bf val)))
  (ival lo hi))

(define (point-ival? x)
  (bf= (ival-lo x) (ival-hi x)))

(define (backward-pass machine)
  ; Since Step 2 writes into *sampling-iteration* if the max prec was reached - save the iter number for step 3
  (define args (rival-machine-arguments machine))
  (define ivec (rival-machine-instructions machine))
  (define rootvec (rival-machine-outputs machine))
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define vrepeats (rival-machine-repeats machine))
  (define vprecs (rival-machine-precisions machine))
  (define vstart-precs (rival-machine-initial-precisions machine))
  (define current-iter (rival-machine-iteration machine))
  (define bumps (rival-machine-bumps machine))

  (define varc (vector-length args))
  (define rootlen (vector-length rootvec))
  (define vprecs-new (make-vector (vector-length ivec) 0))          ; new vprecs vector
  ; Step 1. Adding slack in case of a rounding boundary issue
  (for/vector #:length rootlen
              ([root-reg (in-vector rootvec)] [disc (in-vector discs)]
               #:when (>= root-reg varc))          ; when root is not a variable
    (when (bigfloat? (ival-lo (vector-ref vregs root-reg)))         ; when root is a real op
      (define result (vector-ref vregs root-reg))
      (when
          ; 1 ulp apart means double rounding issue possible
          (= 1 ((my-discretization-distance disc)
                ((my-discretization-convert disc) (ival-lo result))
                ((my-discretization-convert disc) (ival-hi result))))
        (vector-set! vprecs-new (- root-reg varc) (get-slack)))))

  ; Step 2. Exponents calculation
  (exponents-propogation ivec vregs vprecs-new varc vstart-precs)

  ; Step 3. Repeating precisions check
  ; vrepeats[i] = #t if the node has the same precision as an iteration before and children have #t flag as well
  ; vrepeats[i] = #f if the node doesn't have the same precision as an iteration before or at least one child has #f flag
  (for ([instr (in-vector ivec)]
        [prec-old (in-vector (if (equal? 1 current-iter) vstart-precs vprecs))]
        [prec-new (in-vector vprecs-new)]
        [n (in-naturals)])
    (vector-set! vrepeats n (and (<= prec-new prec-old)
                                 (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc))))
                                         (rest instr)))))

  ; Step 4. Copying new precisions into vprecs
  (vector-copy! vprecs 0 vprecs-new)

  ; Step 5. If precisions have not changed but the point didn't converge. A problem exists - add slack to every op
  (when (false? (vector-member #f vrepeats))
    (set-rival-machine-bumps! (add1 bumps))
    (define slack (get-slack))
    (for ([prec (in-vector vprecs)]
          [n (in-range (vector-length vprecs))])
      (define prec* (min (*my-rival-max-precision*) (+ prec slack)))
      (when (equal? prec* (*my-rival-max-precision*)) (*sampling-iteration* (*my-rival-max-iterations*)))
      (vector-set! vprecs n prec*))))

; This function goes through ivec and vregs and calculates (+ exponents base-precisions) for each operator in ivec
; Roughly speaking:
;   vprecs-new[i] = min( *rival-max-precision* max( *base-tuning-precision* (+ exponents-from-above vstart-precs[i])),
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

    (define final-parent-precision (max (+ exps-from-above
                                           (vector-ref vstart-precs (- n varc)))
                                        (*base-tuning-precision*)))

    ; This case is weird. if we have a cancellation in fma -> ival-mult in fma should be in higher precision
    (when (equal? op ival-fma)
      (set! final-parent-precision (+ final-parent-precision (first new-exponents))))
    
    (when (>= final-parent-precision (*my-rival-max-precision*))         ; Early stopping
      (*sampling-iteration* (*my-rival-max-iterations*)))
    (vector-set! vprecs-new (- n varc) (min final-parent-precision (*my-rival-max-precision*)))

    (for ([x (in-list tail-registers)]
          [new-exp (in-list new-exponents)]
          #:when (>= x varc)) ; when tail register is not a variable
      ; check whether this op already has a precision that is higher
      (when (> (+ exps-from-above new-exp) (vector-ref vprecs-new (- x varc)))
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
     ; log[Гpow]'y = log[y] + log[log(x)] <= log[y] + |log[x]|

     (define x (first srcs))
     (define y (second srcs))
     (define outlo (ival-lo output))
     (define outhi (ival-hi output))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     
     ; when output crosses zero and x is negative - means that y was fractional and not fixed
     ; solution - add more slack for y to converge
     (define y-slack (if (and (not (equal? (mpfr-sign outlo) (mpfr-sign outhi)))
                              (bfnegative? (ival-lo x)))
                         (get-slack)
                         0))

     #;(define xlo-exp (mpfr-exp (ival-lo x)))
     #;(define xhi-exp (mpfr-exp (ival-hi x)))
     #;(define x-slack ; it is likely to be a handling case from IEEE, x is not close enough
       (if (or (equal? xlo-exp -9223372036854775807)
               (equal? xhi-exp -9223372036854775807)        ; if interval contains 0
               (and (equal? 1 xlo-exp) (equal? 1 xhi-exp))) ; if interval possibly contains 1
           (get-slack)
           0))
        
     (list (+ y-exp #;x-slack)                        ; exponent per x
           (+ y-exp (abs x-exp) y-slack))]            ; exponent per y
     
    [(ival-exp ival-exp2)
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
     ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)] <= -|log[x]| - log[atan(x)] + 1 <= 0
     (define x (first srcs))
     
     (define x-exp-abs (max (abs (log2-approx (ival-hi x)))
                            (abs (log2-approx (ival-lo x)))))
     (define out-exp (ival-min-log2-approx output))
     
     (list (+ (- (- x-exp-abs) out-exp) 1))]
    
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
     (make-list 3 (+ (- lhs-exp out-exp) 1 slack))]   ; exponents per arguments
    
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
     
     (list (max (+ 1 x-exp) (+ 2 (- x-exp out-exp))))]
    
    [(ival-atan2)
     ; log[Гatan2]'x = log[Гatan2]'y = log[xy / ((x^2+y^2)*atan2)] <= log[x] + log[y] - 2*max[logx, logy] - log[atan2] + 1
     (define x (first srcs))
     (define y (second srcs))
     
     (define x-exp (ival-max-log2-approx x))
     (define y-exp (ival-max-log2-approx y))
     (define out-exp (ival-min-log2-approx output))
     
     (make-list 2 (+ (- (+ x-exp y-exp) (* 2 (max x-exp y-exp)) out-exp) 1))]
    
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
     (define slack (if (< out-exp 2)                  ; when acosh(x) < 1
                       (get-slack)
                       0))
     
     (list (+ (- out-exp) slack))]
    ; TODO
    [(ival-erfc ival-erf ival-lgamma ival-tgamma ival-asinh ival-logb)
     (list (get-slack))]
    [else (make-list (length srcs) 0)]))        ; exponents for argumetns

(define (log2-approx x)
  (define exp (mpfr-exp x))
  (if (or (equal? exp -9223372036854775807) (equal? exp -1073741823))
      (- (get-slack))  ; 0.bf/min.bf
      (if (<= 1073741823 (abs exp))
          (get-slack)  ; max.bf/inf.bf/nan.bf
          (+ exp 1)))) ; +1 because mantissa is not considered

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
