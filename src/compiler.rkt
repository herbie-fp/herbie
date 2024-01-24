#lang racket

(require math/bigfloat rival)
(require "syntax/syntax.rkt" "syntax/types.rkt"
         "common.rkt" "timeline.rkt" "float.rkt" "config.rkt")

(provide compile-specs compile-spec compile-progs compile-prog)

;; Function calculates a precision for the operation
;;    with respect to the given extra-precision (exponents from a previous run)
(define (operator-precision working-prec extra-precision)
  (min (*max-mpfr-prec*)
       (+ extra-precision
          (*ground-truth-extra-bits*)
          working-prec)))

(define (true-exponent x)
  (define exp (+ (bigfloat-exponent x) (bigfloat-precision x)))
  (if (< 1000000000 exp)               ; overflow
       0
       exp))

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; <op-procedure> ::= #(<operation> <extra-precision> <exponents-checkpoint>)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
;; name ::= 'fl or 'ival
(define (make-progs-interpreter name vars ivec roots)
  (define varc (length vars))
  (define vreg-count (+ varc (vector-length ivec)))
  (define vregs (make-vector vreg-count))
  (define start-precision (*starting-prec*))
  
  (if (equal? name 'ival)
      (λ args
        (when (*use-mixed-precision*)
            ;; remove all the precision values we assigned previously when a new point comes
            (if (equal? (*sampling-iteration*) 0)
                (for ([instr (in-vector ivec)])
                  (vector-copy! (car instr) 1 (vector (box start-precision) (box 0) (box 0))))
                  ;(match-define (vector op working-precision extra-precision exponents-checkpoint) (car instr))
                  ;(set-box! working-precision (*starting-prec*))
                  ;(set-box! extra-precision 0)
                  ;(set-box! exponents-checkpoint 0))
                
                ;; backward pass
                (backward-pass ivec varc)))
      
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
          (define timeline-stop! (timeline-start! 'mixsample (~a (object-name op)) precision))
            
          (define output
            (parameterize ([bf-precision precision]) (apply op srcs)))
          (vector-set! vregs n output)
            
          (when (*use-mixed-precision*)
            (cond
              [(equal? op ival-add) ; x + y
               (define x (first srcs))
               (define y (second srcs))
               (define x-exponents ((monotonic->ival true-exponent) x))
               (define x-sings ((monotonic->ival bigfloat-signbit) x))
               (define y-exponents ((monotonic->ival true-exponent) y))
               (define y-sings ((monotonic->ival bigfloat-signbit) y))
               (define output-exponents ((monotonic->ival true-exponent) output))
               (set! output-exponents ((monotonic->ival ; if 0.bf was detected at the output then add 300bits just in case
                                        (lambda (x) (if (equal? x -9223372036854775807) -300 x)))
                                       output-exponents))
               (set-box! exponents-checkpoint
                         (max 0
                              (match* ((and (not (equal? (ival-lo x-sings) (ival-lo y-sings)))
                                            (>= 1 (abs (- (ival-lo x-exponents) (ival-lo y-exponents)))))
                                       (and (not (equal? (ival-hi x-sings) (ival-hi y-sings)))
                                            (>= 1 (abs (- (ival-hi x-exponents) (ival-hi y-exponents))))))
                                [(#f #f) 0]                                                          ; no extra precision
                                [(#t #t) (max (- (ival-lo x-exponents) (ival-lo output-exponents))
                                              (- (ival-hi x-exponents) (ival-hi output-exponents)))] ; cancellation at both bounds
                                [(#t #f) (- (ival-lo x-exponents) (ival-lo output-exponents))]       ; cancellation at lower bound
                                [(#f #t) (- (ival-hi x-exponents) (ival-hi output-exponents))])))]   ; cancellation at upper bound
              [(equal? op ival-sub)
               (define x (first srcs)) ; x - y
               (define y (second srcs))
               (define x-exponents ((monotonic->ival true-exponent) x))
               (define x-sings ((monotonic->ival bigfloat-signbit) x))
               (define y-exponents ((monotonic->ival true-exponent) y))
               (define y-sings ((monotonic->ival bigfloat-signbit) y))
               (define output-exponents ((monotonic->ival true-exponent) output))
               (set! output-exponents ((monotonic->ival ; if 0.bf was detected at the output then add 300bits just in case
                                        (lambda (x) (if (equal? x -9223372036854775807) -300 x)))
                                       output-exponents))
                 
               (set-box! exponents-checkpoint
                         (max 0
                              (match* ((and (equal? (ival-lo x-sings) (ival-hi y-sings))
                                            (>= 1 (abs (- (ival-lo x-exponents) (ival-hi y-exponents)))))
                                       (and (equal? (ival-hi x-sings) (ival-lo y-sings))
                                            (>= 1 (abs (- (ival-hi x-exponents) (ival-lo y-exponents))))))
                                [(#f #f) 0]                                                          ; no extra precision
                                [(#t #t) (max (- (ival-lo x-exponents) (ival-lo output-exponents))
                                              (- (ival-hi x-exponents) (ival-hi output-exponents)))] ; cancellation at both bounds
                                [(#t #f) (- (ival-lo x-exponents) (ival-lo output-exponents))]       ; cancellation at lower bound
                                [(#f #t) (- (ival-hi x-exponents) (ival-hi output-exponents))])))]   ; cancellation at upper bound
          
              [(member op (list ival-sin ival-cos ival-tan))
               (set-box! exponents-checkpoint ; Save exponents with the passed precision for the next run
                         (max 0
                              (true-exponent (ival-lo (car srcs)))
                              (true-exponent (ival-hi (car srcs)))))]))
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
        [1 (*ground-truth-extra-bits*)]
        [2 256]
        [3 512]
        [4 1024]
        [5 2048]))

;; Function does backward-pass
(define (backward-pass ivec varc)
  (let ([root-working-prec (vector-ref (car (vector-ref ivec (- (vector-length ivec) 1))) 1)])
    (set-box! root-working-prec (*tuning-final-output-prec*)))
  (define slack (get-slack))
  
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) 0 -1)]) ; go over operations top-down
    (define tail-registers (rest instr))
    (for ([idx (in-list tail-registers)])                         
      (let ([tail-index (- idx varc)]) ; index of the op's child within ivec
        (when (> tail-index 0)         ; if the child is not a variable
          ; parent
          (match-define (vector _ working-prec extra-prec exponents) (car instr))
          ; child
          (match-define (vector _ workig-prec* extra-prec* _) (car (vector-ref ivec tail-index)))
          (set-box! extra-prec* (+ (unbox extra-prec) (unbox exponents)))
          (set-box! workig-prec* (+ slack (unbox working-prec))))))))


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
