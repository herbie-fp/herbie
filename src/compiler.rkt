#lang racket

(require math/bigfloat rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

(define (operator-precision working-prec extra-precision)
  (min (*max-mpfr-prec*)
       (+ extra-precision
          working-prec)))

(define (true-exponent x)
  (define exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
  (if (< 1000000000 exp)
      (get-slack)  ; overflow
      (if (equal? x -9223372036854775807)
          (- (get-slack))  ; underflow
          exp)))

(define (fixed-in-prec? iv prec)
  (parameterize ([bf-precision prec])
    (bf= (bf+ (ival-lo iv) 0.bf) (bf+ (ival-hi iv) 0.bf))))

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; <op-procedure> ::= #(<operation> <working-precision> <extra-precision> <exponents-checkpoint>)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
;; name ::= 'fl or 'ival
(define (make-progs-interpreter name vars ivec roots)
  (define varc (length vars))
  (define vreg-count (+ varc (vector-length ivec)))
  (define vregs (make-vector vreg-count))
  (define prec-threshold (/ (*max-mpfr-prec*) 25))
  
  (if (equal? name 'ival)
      (λ args
        (when (*use-mixed-precision*)
            ;; remove all the precision values we assigned previously when a new point comes
          (when (equal? (*sampling-iteration*) 0)
            (for ([instr (in-vector ivec)])
              (vector-copy! (car instr) 1 (vector (box 0) (box 0) (box 0)))))
          (backward-pass ivec varc))
        
        (for ([arg (in-list args)] [n (in-naturals)])
          (vector-set! vregs n arg))
        (for ([instr (in-vector ivec)] [n (in-naturals varc)])
          (define srcs
            (for/list ([idx (in-list (cdr instr))])
              (vector-ref vregs idx)))
          
          (match-define (vector op working-precision extra-precision exponents-checkpoint) (car instr))
          (define precision (if (*use-mixed-precision*)
                                (operator-precision
                                 (unbox working-precision)
                                 (unbox extra-precision))
                                (bf-precision)))
          (define timeline-stop! (timeline-start!/unsafe 'mixsample
                                                         (symbol->string (object-name op))
                                                         (- precision (remainder precision prec-threshold))))
            
          (define output
            (parameterize ([bf-precision precision]) (apply op srcs)))
          (vector-set! vregs n output)
          
          (when (*use-mixed-precision*)
            (cond
              [(equal? op ival-add)
               ; TODO: add stop-condition
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
               
               (define yhi (ival-lo y))
               (define yhi-exp (true-exponent yhi))
               (define yhi-sgn (bigfloat-signbit yhi))

               (define outlo-exp (true-exponent (ival-lo output)))
               (define outhi-exp (true-exponent (ival-hi output)))

               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents
                 (max 0
                      (match (or
                              (and (not (equal? xlo-sgn ylo-sgn))
                                   (>= 1 (abs (- xlo-exp ylo-exp))))
                              (and (not (equal? xhi-sgn yhi-sgn))
                                   (>= 1 (abs (- xhi-exp yhi-exp)))))
                        [#f (max (- (max xlo-exp ylo-exp) outlo-exp)
                                 (- (max xhi-exp yhi-exp) outhi-exp))]
                        [#t (+ (get-slack) (max (- xlo-exp outlo-exp)
                                                (- xhi-exp outhi-exp)))])))
               
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]
              [(equal? op ival-sub)
               ; TODO: add stop-condition
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
               
               (define yhi (ival-lo y))
               (define yhi-exp (true-exponent yhi))
               (define yhi-sgn (bigfloat-signbit yhi))

               (define outlo-exp (true-exponent (ival-lo output)))
               (define outhi-exp (true-exponent (ival-hi output)))

               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents
                 (max 0
                      (match (or
                              (and (equal? xlo-sgn yhi-sgn)
                                   (>= 1 (abs (- xlo-exp yhi-exp))))
                              (and (equal? xhi-sgn ylo-sgn)
                                   (>= 1 (abs (- xhi-exp ylo-exp)))))
                        [#f (max (- (max xlo-exp yhi-exp) outlo-exp)
                                 (- (max xhi-exp ylo-exp) outhi-exp))]
                        [#t (+ (get-slack) (max (- xlo-exp outlo-exp)
                                                (- xhi-exp outhi-exp)))])))
               
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]
              [(equal? op ival-pow)
               ; log[Гpow] = max[ log(y) , log(y) + log[log(x)] ]
               (define xlo-exp (true-exponent (ival-lo (first srcs))))
               (define xhi-exp (true-exponent (ival-hi (first srcs))))

               (define ylo-exp (true-exponent (ival-lo (second srcs))))
               (define yhi-exp (true-exponent (ival-hi (second srcs))))

               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents
                 (if (> (max xlo-exp xhi-exp) 2) ; if x >= 4 (actually 7.3890561), then at least 1 additional bit is needed
                     (+ (max ylo-exp yhi-exp) 30)
                     (max ylo-exp yhi-exp)))
               
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]
              
              [(equal? (or ival-tan ival-exp ival-tanh) op)
               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents (max 0
                                          (true-exponent (ival-lo (car srcs)))
                                          (true-exponent (ival-hi (car srcs)))))
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  (+ (get-slack) new-exponents)
                                                  prev-exponents))]
              
              [(equal? (or ival-sin ival-cos ival-sinh ival-cosh) op)
               ; log[Гcos] = log[x] + log[sin(x)] - log[cos(x)], where log[sin(x)] <= 0
               ;                      ^^^^^^^^^^^
               ;                      pruning opportunity
               (define outlo (ival-lo output))
               (define outhi (ival-hi output))

               (define out-exp
                 (match* ((bigfloat-signbit outlo) (bigfloat-signbit outhi))
                   [(0 0) (min
                           (true-exponent outlo)
                           (true-exponent outhi))]
                   [(1 1) (min
                           (true-exponent outlo)
                           (true-exponent outhi))]
                   [(_ _) (- (min ; Condition of uncertainty, 0.bf can be included
                              (true-exponent outlo)
                              (true-exponent outhi))
                             (get-slack))]))
               
               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents (max 0
                                          (- (max
                                              (true-exponent (ival-lo (car srcs)))
                                              (true-exponent (ival-hi (car srcs))))
                                             out-exp)))
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]
               
              [(equal? op ival-log)
               ; log[Гlog] = log[1/logx] = -log[log(x)]
               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents (max 0
                                          (- (true-exponent (ival-lo output)))
                                          (- (true-exponent (ival-hi output)))))
               
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]
              
              [(equal? op (or ival-asin ival-acos))
               ; log[Гasin] = log[x] - log[1-x^2]/2 - log[asin(x)]
               ; log[Гacos] = log[x] - log[1-x^2]/2 - log[acos(x)]
               ;                       ^^^^^^^^^^^^
               ;                       condition of uncertainty
               
               (define xlo (ival-lo (car srcs)))
               (define xhi (ival-hi (car srcs)))
               (define xlo-exp (true-exponent xlo))
               (define xhi-exp (true-exponent xhi))

               (define out-exp
                 (match* ((bf> (bfabs xlo) (bf 0.9)) (bf> (bfabs xhi) (bf 0.9))) ; 0.9 seems to be a threshold by looking at graph
                   [(#f #f)
                    (min
                     (true-exponent (ival-lo output))
                     (true-exponent (ival-hi output)))]
                   [(_ _) ; Condition of uncertainty when argument is > 0.9 (actually sqrt(3)/2)
                    (- (min
                        (true-exponent (ival-lo output))
                        (true-exponent (ival-hi output)))
                       (get-slack))]))
                 
               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents (max 0
                                          (- xlo-exp out-exp)
                                          (- xhi-exp out-exp)))
               
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]

              [(equal? op ival-atan)
               ; log[Гatan] = log[x] - log[x^2+1] - log[atan(x)], where log[x^2+1] > 0
               ;                       ^^^^^^^^^^
               ;                       can be considered for pruning
               
               (define xlo-exp (true-exponent (ival-lo (car srcs))))
               (define xhi-exp (true-exponent (ival-hi (car srcs))))
               
               (define prev-exponents (unbox exponents-checkpoint))
               (define new-exponents (max 0
                                          (- xlo-exp (true-exponent (ival-lo output)))
                                          (- xhi-exp (true-exponent (ival-hi output)))))
               (set-box! exponents-checkpoint (if (> new-exponents prev-exponents)
                                                  new-exponents
                                                  prev-exponents))]))

          
          #;(when (and (*use-mixed-precision*) (bigfloat? (ival-lo output)) (> (*sampling-iteration*) 4))
            (printf "~a fixed=~a " (symbol->string (object-name op)) (fixed-in-prec? output (- precision (*ground-truth-extra-bits*))))
            (printf "exonents=~a for ~a\n" (unbox exponents-checkpoint) (parameterize ([bf-precision 53]) output)))
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

;; Function does backward-pass
(define (backward-pass ivec varc)
  (let ([root-working-prec (vector-ref (car (vector-ref ivec (- (vector-length ivec) 1))) 1)])
    (set-box! root-working-prec (*tuning-final-output-prec*)))

  (for ([instr (in-vector ivec (- (vector-length ivec) 1) 0 -1)]) ; go over operations top-down
    (define tail-registers (rest instr))
    (for ([idx (in-list tail-registers)])                         
      (let ([tail-index (- idx varc)]) ; index of the op's child within ivec
        (when (>= tail-index 0)         ; if the child is not a variable
          ; parent
          (match-define (vector _ working-prec extra-prec exponents) (car instr))
          ; make sure that parent is at least ~53smth bits. Important for precondition, otherwise it is 0bits
          (set-box! working-prec (max (*tuning-final-output-prec*) (unbox working-prec)))
          ; child
          (match-define (vector _ workig-prec* extra-prec* _) (car (vector-ref ivec tail-index)))
          (set-box! extra-prec* (+ (unbox extra-prec) (unbox exponents)))
          
          (set-box! workig-prec* (+ (*ground-truth-extra-bits*) (unbox working-prec))))))))

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
    
    ; Translates programs into an instruction sequence of ival operations
    (define (munge-ival prog type)
      (set! size (+ 1 size))

      (define expr
        (match prog
          [(? number?) (list (vector (const (input->value prog type)) (box 0) (box 0) (box 0)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list (vector if-proc (box 0) (box 0) (box 0))
                 (munge-ival c cond-type)
                 (munge-ival t type)
                 (munge-ival f type))]
          [(list op args ...)
           (cons (vector (op->proc op) (box 0) (box 0) (box 0))
                    (map munge-ival
                         args
                         (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc)
                     (set! exprc (+ 1 exprc))
                     (set! icache (cons expr icache))))))

    ; Translates programs into an instruction sequence of flonum operations
    (define (munge-fl prog type)
      (set! size (+ 1 size))
      (define expr
        (match prog
          [(? number?) (list (const (input->value prog type)))]
          [(? variable?) prog]
          [`(if ,c ,t ,f)
           (list if-proc (munge-fl c cond-type) (munge-fl t type) (munge-fl f type))]
          [(list op args ...)
           (cons (op->proc op) (map munge-fl args (op->itypes op)))]
          [_ (raise-argument-error 'compile-specs "Not a valid expression!" prog)]))
      (hash-ref! exprhash expr
                 (λ ()
                   (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                           (set! exprc (+ 1 exprc))
                           (set! icache (cons expr icache))))))
    
    (define names (map (curryr (if (equal? name 'fl) munge-fl munge-ival) type) exprs))
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
