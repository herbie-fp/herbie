#lang racket

(require math/bigfloat rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

(define (true-exponent x)
  (define exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
  (if (equal? exp -9223372036854775807)
      (- (get-slack))  ; 0.bf
      (if (or (< 1000000000 (abs exp)))
          (get-slack)  ; overflow/inf.bf/nan.bf
          exp)))

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
  (define varc (length vars))
  (define vreg-count (+ varc (vector-length ivec)))
  (define vregs (make-vector vreg-count))
  (define vprecs (make-vector (vector-length ivec)))   ; vector that stores working precisions
  (define vstart-precs (setup-vstart-precs ivec varc)) ; starting precisions for the tuning mode
  (define prec-threshold (/ (*max-mpfr-prec*) 25))     ; parameter for sampling histogram table
  (if (equal? name 'ival)
      (λ args
        (printf "\n")
        (match (*use-mixed-precision*)
          [#t (define timeline-stop! (timeline-start!/unsafe 'mixsample "backward-pass"
                                                             (* (*sampling-iteration*) 1000)))
              (if (equal? (*sampling-iteration*) 0)
                  (vector-copy! vprecs 0 vstart-precs) ; clear precisions from the last args
                  (backward-pass ivec varc vregs vprecs vstart-precs)) ; back-pass
              (timeline-stop!)]
          [#f (vector-fill! vprecs (bf-precision))])
        
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)] [precision (in-vector vprecs)])
          (define srcs
            (for/list ([idx (in-list (cdr instr))])
              (vector-ref vregs idx)))
          (printf "instr=~a, prec=~a\n" instr precision)
          
          (define timeline-stop! (timeline-start!/unsafe 'mixsample
                                                         (symbol->string (object-name (car instr)))
                                                         (- precision (remainder precision prec-threshold))))
            
          (define output
            (parameterize ([bf-precision precision]) (apply (car instr) srcs)))
          (vector-set! vregs n output)
          (timeline-stop!))

        (for/list ([root (in-list roots)])
          (vector-ref vregs root)))
   
      ; name is 'fl
      (λ args
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)])
          (define srcs
            (for/list ([idx (in-list (cdr instr))])
              (vector-ref vregs idx)))
          (vector-set! vregs n (apply (car instr) srcs)))
        (for/list ([root (in-list roots)])
          (vector-ref vregs root)))))

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
                       #:if-procedure if-proc
                       #:cond-type cond-type)
  (lambda (exprs vars type)
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
    (define (munge prog type)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? literal?) (list (const (input->value (literal-value prog) type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge c cond-type) (munge t type) (munge f type))]
          [(list op args ...)
           (cons (op->proc op) (map munge args (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                           (set! exprc (+ 1 exprc))
                           (set! icache (cons expr icache))))))
    
    (define names (map (curryr munge type) exprs))
    (timeline-push! 'compiler (+ varc size) (+ exprc varc))
    (define ivec (list->vector (reverse icache)))
    (define interpret (make-progs-interpreter name vars ivec names))
    (procedure-rename interpret (sym-append 'eval-prog '- name))))

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
                   #:input->value (lambda (prog _) (ival (bf prog)))
                   #:op->procedure (lambda (op) (operator-info (real-op op) 'ival))
                   #:op->itypes (lambda (op) (operator-info (real-op op) 'itype))
                   #:if-procedure ival-if
                   #:cond-type 'bool))
  (compile specs vars 'real))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
(define (compile-progs exprs ctx)
  (define compile
    (make-compiler 'fl
                   #:input->value real->repr
                   #:op->procedure (lambda (op) (impl-info op 'fl))
                   #:op->itypes (lambda (op) (impl-info op 'itype))
                   #:if-procedure (λ (c ift iff) (if c ift iff))
                   #:cond-type (get-representation 'bool)))
  (compile exprs (context-vars ctx) (context-repr ctx)))

;; Like `compile-specs`, but for a single spec.
(define (compile-spec spec vars)
  (compose first (compile-specs (list spec) vars)))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (compose first (compile-progs (list expr) ctx)))

(define (backward-pass ivec varc vregs vprecs vstart-precs)
  (vector-fill! vprecs 0)
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)] ; reversed over ivec
        [n (in-range (- (vector-length vregs) 1) -1 -1)])         ; reversed over indices of vregs

    (define op (car instr)) ; current operation
    (define tail-registers (rest instr))               
    (define srcs (map (lambda (x) (vector-ref vregs x)) tail-registers)) ; tail of the current instr
    (define output (vector-ref vregs n)) ; output of the current instr

    (define exps-from-above (vector-ref vprecs (- n varc))) ; vprecs is shifted by varc elements from vregs
    (define final-parent-precision (min (*max-mpfr-prec*)
                                        (+ exps-from-above (vector-ref vstart-precs (- n varc)))))
    (when (equal? final-parent-precision (*max-mpfr-prec*))
      (*sampling-iteration* (*max-sampling-iterations*)))
    (vector-set! vprecs (- n varc) final-parent-precision)

    (define new-exponents 0)
    (cond
      [(equal? op ival-add)
       (define x (first srcs))
       (define xlo (ival-lo x))
       (define xlo-exp (true-exponent xlo))
       (define xlo-sgn (bigfloat-signbit xlo))
       (define xhi (ival-hi x))
       (define xhi-exp (true-exponent xhi))
       (define xhi-sgn (bigfloat-signbit xhi))

       (define y (second srcs))
       (define ylo (ival-lo y))
       (define ylo-exp (true-exponent ylo))
       (define ylo-sgn (bigfloat-signbit ylo))
       (define yhi (ival-hi y))
       (define yhi-exp (true-exponent yhi))
       (define yhi-sgn (bigfloat-signbit yhi))
       
       (define outlo-exp (true-exponent (ival-lo output)))
       (define outhi-exp (true-exponent (ival-hi output)))
       
       (set! new-exponents
             (max 0
                  (match (and (or (not (equal? xlo-sgn ylo-sgn))
                                  (not (equal? xhi-sgn yhi-sgn)))
                              (or (>= 1 (abs (- xlo-exp ylo-exp)))
                                  (>= 1 (abs (- xhi-exp yhi-exp)))))
                    [#f (max
                         (- (max xlo-exp ylo-exp) outlo-exp)
                         (- (max xhi-exp yhi-exp) outhi-exp))]
                    [#t (+ (get-slack)
                           (max
                            (- (max xlo-exp ylo-exp) outlo-exp)
                            (- (max xhi-exp yhi-exp) outhi-exp)))])))]
      
      [(equal? op ival-sub)
       (define x (first srcs))
       (define xlo (ival-lo x))
       (define xlo-exp (true-exponent xlo))
       (define xlo-sgn (bigfloat-signbit xlo))
       (define xhi (ival-hi x))
       (define xhi-exp (true-exponent xhi))
       (define xhi-sgn (bigfloat-signbit xhi))

       (define y (second srcs))
       (define ylo (ival-lo y))
       (define ylo-exp (true-exponent ylo))
       (define ylo-sgn (bigfloat-signbit ylo))
       (define yhi (ival-hi y))
       (define yhi-exp (true-exponent yhi))
       (define yhi-sgn (bigfloat-signbit yhi))
       
       (define outlo-exp (true-exponent (ival-lo output)))
       (define outhi-exp (true-exponent (ival-hi output)))
       
       (set! new-exponents
         (max 0
              (match (and (or (equal? xlo-sgn yhi-sgn)
                              (equal? xhi-sgn ylo-sgn))
                          (or (>= 1 (abs (- xlo-exp yhi-exp)))
                              (>= 1 (abs (- xhi-exp ylo-exp)))))
                [#f (max
                     (- (max xlo-exp yhi-exp) outlo-exp)
                     (- (max xhi-exp ylo-exp) outhi-exp))]
                [#t (+ (get-slack)
                       (max
                        (- (max xlo-exp yhi-exp) outlo-exp)
                        (- (max xhi-exp ylo-exp) outhi-exp)))])))]
      
      [(equal? op ival-pow)
       ; log[Гpow] = max[ log(y) , log(y) + log[log(x)] ]
       (define xlo-exp (true-exponent (ival-lo (first srcs))))
       (define xhi-exp (true-exponent (ival-hi (first srcs))))
       (define ylo-exp (true-exponent (ival-lo (second srcs))))
       (define yhi-exp (true-exponent (ival-hi (second srcs))))

       (set! new-exponents
             (if (> (max xlo-exp xhi-exp) 2) ; if x >= 4 (actually 7.3890561), then at least 1 additional bit is needed
                 (+ (max ylo-exp yhi-exp) 30)
                 (max ylo-exp yhi-exp)))]

      [(equal? ival-exp op)
       (set! new-exponents (max 0
                                (true-exponent (ival-lo (car srcs)))
                                (true-exponent (ival-hi (car srcs)))))]

      ; TODO: tanh - it is actually a different case
      [(equal? ival-tan op)
       ; log[Гtan] = log[x] - log[sin(x)*cos(x)] <= log[x] + |log[tan(x)]| + 1
       ;                                                      ^^^^^^^^^^^
       ;                                                 tan can be (-inf, +inf) or close to zero
       (define outlo (ival-lo output))
       (define outhi (ival-hi output))
       
       (define out-exp
         (match (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi))
           [#t (+   ; both bounds are positive or negative
                   (max (abs (true-exponent outlo))
                        (abs (true-exponent outhi)))
                   1)]
           [#f (+   ; tan is (-inf, +inf) or around zero
                (max (abs (true-exponent outlo))
                     (abs (true-exponent outhi)))
                1
                (get-slack))]))

       (set! new-exponents (max 0
                                (+ (true-exponent (ival-lo (car srcs))) out-exp)
                                (+ (true-exponent (ival-hi (car srcs))) out-exp)))]
              
      [(member op (list ival-sin ival-cos ival-sinh ival-cosh))
       ; log[Гcos] = log[x] + log[sin(x)] - log[cos(x)], where log[sin(x)] <= 0
       ;                      ^^^^^^^^^^^
       ;                      pruning opportunity
       (define outlo (ival-lo output))
       (define outhi (ival-hi output))

       (define out-exp
         (match (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi))
           [#t (min    ; both bounds are positive or negative
                (true-exponent outlo)
                (true-exponent outhi))]
           [#f (- (min ; Condition of uncertainty, 0.bf can be included
                   (true-exponent outlo)
                   (true-exponent outhi))
                  (get-slack))] ; assumes that log[cos(x)/sin(x)]'s exponent is slack bits less  (zero case)
           ))
               
       (set! new-exponents (max 0 (- (max
                                      (true-exponent (ival-lo (car srcs)))
                                      (true-exponent (ival-hi (car srcs))))
                                     out-exp)))]
               
      [(equal? op ival-log)
       ; log[Гlog] = log[1/logx] = -log[log(x)]
       (define outlo (ival-lo output))
       (define outhi (ival-hi output))
       
       (set! new-exponents
             (match (xor (bigfloat-signbit outlo) (bigfloat-signbit outhi))
               [#t   ; both bounds are positive or negative
                (max (- (true-exponent outlo))
                     (- (true-exponent outhi)))]
               [#f ; output crosses 0.bf - uncertainty
                (+ (get-slack)
                   (max 0
                        (- (true-exponent outlo))
                        (- (true-exponent outhi))))]
               ))]
              
      [(member op (list ival-asin ival-acos))
       ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)]
       ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)]
       ;                       ^^^^^^^^^^^^
       ;                       condition of uncertainty
       (define xlo (ival-lo (car srcs)))
       (define xhi (ival-hi (car srcs)))
       (define xlo-exp (true-exponent xlo))
       (define xhi-exp (true-exponent xhi))
       (define out-exp
         (match* ((>= xlo-exp 0) (>= xhi-exp 0))
           [(#f #f)
            (min
             (true-exponent (ival-lo output))
             (true-exponent (ival-hi output)))]
           [(_ _) ; Condition of uncertainty when argument is possibly > 0.9 (actually sqrt(3)/2)
            (+ (min
                (true-exponent (ival-lo output))
                (true-exponent (ival-hi output)))
               (- (get-slack)))] ; assumes that log[1-x^2]/2 is equal to (- slack)
           ))   
       (set! new-exponents (max 0 (- xlo-exp out-exp) (- xhi-exp out-exp)))]

      [(equal? op ival-atan)
       ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)] <= -log[x] - log[atan(x)]
       (define xlo-exp (- (true-exponent (ival-lo (car srcs)))))
       (define xhi-exp (- (true-exponent (ival-hi (car srcs)))))
       (set! new-exponents (max 0
                                (- xlo-exp (true-exponent (ival-lo output)))
                                (- xhi-exp (true-exponent (ival-hi output)))))])
    
    (define child-precision (+ exps-from-above new-exponents))
    (map (lambda (x) (when (>= x varc)  ; when tail register is not a variable
                       (vector-set! vprecs (- x varc) child-precision)))
         tail-registers)))
