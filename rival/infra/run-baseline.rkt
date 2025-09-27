#lang racket/base

(require racket
         math/bigfloat
         math/flonum)

(require (only-in "../eval/compile.rkt" exprs->batch fn->ival-fn make-initial-repeats)
         (only-in "../eval/machine.rkt" *rival-max-precision* *rival-profile-executions*)
         "../eval/main.rkt"
         (only-in "../ops/core.rkt" new-ival)
         "../ops/all.rkt")

(provide baseline-compile
         baseline-analyze
         baseline-apply
         baseline-profile
         (struct-out baseline-machine))

(struct baseline-machine
        (arguments instructions
                   outputs
                   discs
                   registers
                   precisions
                   best-known-precisions
                   repeats
                   initial-repeats
                   default-hint
                   [iteration #:mutable]
                   [precision #:mutable]
                   [profile-ptr #:mutable]
                   profile-instruction
                   profile-number
                   profile-time
                   profile-memory
                   profile-precision
                   profile-iteration))

(define (make-hint machine old-hint)
  (define args (baseline-machine-arguments machine))
  (define ivec (baseline-machine-instructions machine))
  (define rootvec (baseline-machine-outputs machine))
  (define vregs (baseline-machine-registers machine))

  (define varc (vector-length args))
  (define vhint (make-vector (vector-length ivec) #f))
  (define converged? #t)

  ; helper function
  (define (vhint-set! idx val)
    (when (>= idx varc)
      (vector-set! vhint (- idx varc) val)))

  ; roots always should be executed
  (for ([root-reg (in-vector rootvec)])
    (vhint-set! root-reg #t))
  (for ([instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
        [hint (in-vector vhint (- (vector-length vhint) 1) -1 -1)]
        [o-hint (in-vector old-hint (- (vector-length old-hint) 1) -1 -1)]
        [n (in-range (- (vector-length vhint) 1) -1 -1)]
        #:when hint)
    (define hint*
      (match o-hint
        [(? ival? _) o-hint] ; instr is already "hinted" by old hint, no children are to be recomputed
        [(? integer? ref) ; instr is already "hinted" by old hint,
         (define idx (list-ref instr ref)) ; however, one child needs to be recomputed
         (when (>= idx varc)
           (vhint-set! idx #t))
         o-hint]
        [#t
         (case (object-name (car instr))
           [(ival-assert)
            (match-define (list _ bool-idx) instr)
            (define bool-reg (vector-ref vregs bool-idx))
            (match* ((ival-lo bool-reg) (ival-hi bool-reg) (ival-err? bool-reg))
              ; assert and its children should not be reexecuted if it is true already
              [(#t #t #f) (ival-bool #t)]
              ; assert and its children should not be reexecuted if it is false already
              [(#f #f #f) (ival-bool #f)]
              [(_ _ _) ; assert and its children should be reexecuted
               (vhint-set! bool-idx #t)
               (set! converged? #f)
               #t])]
           [(ival-if)
            (match-define (list _ cond tru fls) instr)
            (define cond-reg (vector-ref vregs cond))
            (match* ((ival-lo cond-reg) (ival-hi cond-reg) (ival-err? cond-reg))
              [(#t #t #f) ; only true path should be executed
               (vhint-set! tru #t)
               2]
              [(#f #f #f) ; only false path should be executed
               (vhint-set! fls #t)
               3]
              [(_ _ _) ; execute both paths and cond as well
               (vhint-set! cond #t)
               (vhint-set! tru #t)
               (vhint-set! fls #t)
               (set! converged? #f)
               #t])]
           [(ival-fmax)
            (match-define (list _ arg1 arg2) instr)
            (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              [(#t #t #f) ; only arg1 should be executed
               (vhint-set! arg1 #t)
               1]
              [(#f #f #f) ; only arg2 should be executed
               (vhint-set! arg2 #t)
               2]
              [(_ _ _) ; both paths should be executed
               (vhint-set! arg1 #t)
               (vhint-set! arg2 #t)
               (set! converged? #f)
               #t])]
           [(ival-fmin)
            (match-define (list _ arg1 arg2) instr)
            (define cmp (ival-> (vector-ref vregs arg1) (vector-ref vregs arg2)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              [(#t #t #f) ; only arg2 should be executed
               (vhint-set! arg2 #t)
               2]
              [(#f #f #f) ; only arg1 should be executed
               (vhint-set! arg1 #t)
               1]
              [(_ _ _) ; both paths should be executed
               (vhint-set! arg1 #t)
               (vhint-set! arg2 #t)
               (set! converged? #f)
               #t])]
           [(ival-< ival-<= ival-> ival->= ival-== ival-!= ival-and ival-or ival-not)
            (define cmp (vector-ref vregs (+ varc n)))
            (match* ((ival-lo cmp) (ival-hi cmp) (ival-err? cmp))
              ; result is known
              [(#t #t #f) (ival-bool #t)]
              ; result is known
              [(#f #f #f) (ival-bool #f)]
              [(_ _ _) ; all the paths should be executed
               (define srcs (rest instr))
               (for-each (λ (x) (vhint-set! x #t)) srcs)
               (set! converged? #f)
               #t])]
           [else ; at this point we are given that the current instruction should be executed
            (define srcs
              (drop-self-pointers (rest instr)
                                  (+ n
                                     varc))) ; then, children instructions should be executed as well
            (for-each (λ (x) (vhint-set! x #t)) srcs)
            #t])]))
    (vector-set! vhint n hint*))
  (values vhint converged?))

; ----------------------------------------- COMPILATION ----------------------------------------------
(define (baseline-compile exprs vars discs)
  (define num-vars (length vars))
  (define-values (nodes roots) (exprs->batch exprs vars)) ; translations are taken from Rival machine
  (define register-count (vector-length nodes))
  (define registers (make-vector register-count))
  (define instructions
    (for/vector #:length (- register-count num-vars)
                ([node (in-vector nodes num-vars)]
                 [n (in-naturals num-vars)])
      (fn->ival-fn node ; mappings are taken from Rival machine
                   (lambda ()
                     (vector-set! registers n (new-ival))
                     n))))

  (define start-prec (+ (discretization-target (last discs)) 10))
  (define precisions
    (make-vector (- register-count num-vars) start-prec)) ; vector that stores working precisions
  (define best-known-precisions (make-vector (- register-count num-vars) 0)) ; for constant ops

  (define repeats (make-vector (- register-count num-vars)))
  (define initial-repeats
    (make-initial-repeats instructions num-vars registers precisions best-known-precisions))

  (define default-hint (make-vector (- register-count num-vars) #t))

  (baseline-machine (list->vector vars)
                    instructions
                    roots
                    discs
                    registers
                    precisions
                    best-known-precisions
                    repeats
                    initial-repeats
                    default-hint
                    0
                    0
                    0
                    (make-vector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))
                    (make-flvector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))
                    (make-vector (*rival-profile-executions*))))

; ------------------------------------------- APPLY --------------------------------------------------
(define (ival-real x)
  (ival x))

(define (baseline-apply machine pt [hint #f])
  (define discs (baseline-machine-discs machine))
  (define start-prec (+ (discretization-target (last discs)) 10))
  ; Load arguments
  (baseline-machine-load machine (vector-map ival-real pt))
  (let loop ([prec start-prec]
             [iter 0])
    (set-baseline-machine-iteration! machine iter)
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([bf-precision prec])
        (baseline-machine-full machine (or hint (baseline-machine-default-hint machine)))))
    (cond
      [bad? (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done? fvec]
      [stuck? (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(> (* 2 prec) (*rival-max-precision*)) ; max precision is taken from eval/machine.rkt
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else (loop (* 2 prec) (+ iter 1))])))

(define (baseline-analyze machine rect [hint #f])
  (baseline-machine-load machine rect)
  (set-baseline-machine-iteration! machine 0)
  (define-values (good? done? bad? stuck? fvec)
    (baseline-machine-full machine (or hint (baseline-machine-default-hint machine))))
  (define-values (hint* hint*-converged?)
    (make-hint machine (or hint (baseline-machine-default-hint machine))))
  (list (ival (or bad? stuck?) (not good?)) hint* hint*-converged?))

(define (baseline-machine-adjust machine)
  (let ([start (current-inexact-milliseconds)]
        [mem-before (current-memory-use #f)])
    (define new-prec (bf-precision))
    (set-baseline-machine-precision! machine new-prec)
    (vector-fill! (baseline-machine-precisions machine) new-prec)

    ; Whether a register is fixed already
    (define iter (baseline-machine-iteration machine))
    (unless (zero? iter)
      (define ivec (baseline-machine-instructions machine))
      (define vregs (baseline-machine-registers machine))
      (define rootvec (baseline-machine-outputs machine))
      (define vrepeats (baseline-machine-repeats machine))
      (define args (baseline-machine-arguments machine))
      (define vbest-precs (baseline-machine-best-known-precisions machine))
      (define vinitial-repeats (baseline-machine-initial-repeats machine))
      (define varc (vector-length args))
      (define vuseful (make-vector (vector-length ivec) #f))

      ; Useful feature
      (for ([root (in-vector rootvec)]
            #:when (>= root varc))
        (vector-set! vuseful (- root varc) #t))
      (for ([reg (in-vector vregs (- (vector-length vregs) 1) (- varc 1) -1)]
            [instr (in-vector ivec (- (vector-length ivec) 1) -1 -1)]
            [i (in-range (- (vector-length ivec) 1) -1 -1)]
            [useful? (in-vector vuseful (- (vector-length vuseful) 1) -1 -1)])
        (cond
          [(and (ival-lo-fixed? reg) (ival-hi-fixed? reg)) (vector-set! vuseful i #f)]
          [useful?
           (for ([arg (in-list (drop-self-pointers (cdr instr) (+ i varc)))]
                 #:when (>= arg varc))
             (vector-set! vuseful (- arg varc) #t))]))

      ; Constant operations
      (for ([instr (in-vector ivec)]
            [useful? (in-vector vuseful)]
            [best-known-precision (in-vector vbest-precs)]
            [constant? (in-vector vinitial-repeats)]
            [n (in-naturals)])
        (define tail-registers (drop-self-pointers (cdr instr) (+ n varc)))
        ; When instr is a constant instruction - keep tracks of old precision with vbest-precs vector
        (define no-need-to-reevaluate
          (and constant?
               (<= new-prec best-known-precision)
               (andmap (lambda (x) (or (< x varc) (vector-ref vrepeats (- x varc)))) tail-registers)))
        (define result-is-exact-already (not useful?))
        (define repeat (or result-is-exact-already no-need-to-reevaluate))

        ; Precision of const instruction has increased + it will be reexecuted under that precision
        (when (and constant? (not repeat) (not no-need-to-reevaluate))
          (vector-set! vbest-precs
                       n
                       new-prec)) ; record new best precision for the constant instruction
        (vector-set! vrepeats n repeat)))

    (baseline-machine-record machine
                             'adjust
                             -1
                             (* iter 1000)
                             (- (current-inexact-milliseconds) start)
                             (max 0 (- (current-memory-use #f) mem-before))
                             iter)))

(define (drop-self-pointers tail-regs n)
  (filter (λ (x) (not (equal? x n))) tail-regs))

(define (baseline-machine-full machine vhint)
  (baseline-machine-adjust machine)
  (baseline-machine-run machine vhint)
  (baseline-machine-return machine))

(define (baseline-machine-load machine args)
  (vector-copy! (baseline-machine-registers machine) 0 args))

(define (baseline-machine-run machine vhint)
  (define ivec (baseline-machine-instructions machine))
  (define varc (vector-length (baseline-machine-arguments machine)))
  (define vregs (baseline-machine-registers machine))
  (define precisions (baseline-machine-precisions machine))
  (define repeats (baseline-machine-repeats machine))
  (define initial-repeats (baseline-machine-initial-repeats machine))
  (define iter (baseline-machine-iteration machine))
  (define first-iter? (zero? (baseline-machine-iteration machine)))

  (for ([instr (in-vector ivec)]
        [n (in-naturals varc)]
        [precision (in-vector precisions)]
        [repeat (in-vector (if first-iter? initial-repeats repeats))]
        [hint (in-vector vhint)]
        #:unless (or (not hint) repeat))
    (define out
      (match hint
        [#t
         (define start (current-inexact-milliseconds))
         (define mem-before (current-memory-use #f))
         (define res
           (parameterize ([bf-precision precision])
             (apply-instruction instr vregs)))
         (define name (object-name (car instr)))
         (define time (- (current-inexact-milliseconds) start))
         (define memory-delta (max 0 (- (current-memory-use #f) mem-before)))
         (baseline-machine-record machine name n precision time memory-delta iter)
         res]
        [(? integer? _) (vector-ref vregs (list-ref instr hint))]
        [(? ival? _) hint]))
    (vector-set! vregs n out)))

(define (apply-instruction instr regs)
  ;; By special-casing the 0-3 instruction case,
  ;; we avoid any allocation in the common case.
  ;; We could add more cases if we want wider instructions.
  ;; At some extreme, vector->values plus call-with-values
  ;; becomes the fastest option.
  (match instr
    [(list op) (op)]
    [(list op a) (op (vector-ref regs a))]
    [(list op a b) (op (vector-ref regs a) (vector-ref regs b))]
    [(list op a b c) (op (vector-ref regs a) (vector-ref regs b) (vector-ref regs c))]
    [(list op args ...) (apply op (map (curryr vector-ref regs) args))]))

(define (baseline-machine-return machine)
  (define discs (baseline-machine-discs machine))
  (define vregs (baseline-machine-registers machine))
  (define rootvec (baseline-machine-outputs machine))
  (define ovec (make-vector (vector-length rootvec)))
  (define good? #t)
  (define done? #t)
  (define bad? #f)
  (define stuck? #f)
  (define fvec
    (for/vector #:length (vector-length rootvec)
                ([root (in-vector rootvec)]
                 [disc (in-list discs)]
                 [n (in-naturals)])
      (define out (vector-ref vregs root))
      (define lo ((discretization-convert disc) (ival-lo out)))
      (define hi ((discretization-convert disc) (ival-hi out)))
      (define distance ((discretization-distance disc) lo hi))
      (unless (= distance 0)
        (set! done? #f)
        (when (and (ival-lo-fixed? out) (ival-hi-fixed? out))
          (set! stuck? #t)))
      (cond
        [(ival-err out) (set! bad? #t)]
        [(ival-err? out) (set! good? #f)])
      lo))
  (values good? (and good? done?) bad? stuck? fvec))

; ---------------------------------------- PROFILING -------------------------------------------------
(define (baseline-profile machine param)
  (match param
    ['iteration (baseline-machine-iteration machine)]
    ['precision (baseline-machine-precision machine)]
    ['instructions (vector-length (baseline-machine-instructions machine))]
    ['executions
     (define profile-ptr (baseline-machine-profile-ptr machine))
     (define profile-instruction (baseline-machine-profile-instruction machine))
     (define profile-number (baseline-machine-profile-number machine))
     (define profile-time (baseline-machine-profile-time machine))
     (define profile-memory (baseline-machine-profile-memory machine))
     (define profile-precision (baseline-machine-profile-precision machine))
     (define profile-iteration (baseline-machine-profile-iteration machine))
     (begin0 (for/vector #:length profile-ptr
                         ([instruction (in-vector profile-instruction 0 profile-ptr)]
                          [number (in-vector profile-number 0 profile-ptr)]
                          [precision (in-vector profile-precision 0 profile-ptr)]
                          [time (in-flvector profile-time 0 profile-ptr)]
                          [memory (in-vector profile-memory 0 profile-ptr)]
                          [iter (in-vector profile-iteration 0 profile-ptr)])
               (execution instruction number precision time memory iter))
       (set-baseline-machine-profile-ptr! machine 0))]))

(define (baseline-machine-record machine name number precision time memory iter)
  (define profile-ptr (baseline-machine-profile-ptr machine))
  (define profile-instruction (baseline-machine-profile-instruction machine))
  (when (< profile-ptr (vector-length profile-instruction))
    (define profile-number (baseline-machine-profile-number machine))
    (define profile-time (baseline-machine-profile-time machine))
    (define profile-memory (baseline-machine-profile-memory machine))
    (define profile-precision (baseline-machine-profile-precision machine))
    (define profile-iteration (baseline-machine-profile-iteration machine))
    (vector-set! profile-instruction profile-ptr name)
    (vector-set! profile-number profile-ptr number)
    (vector-set! profile-memory profile-ptr memory)
    (vector-set! profile-precision profile-ptr precision)
    (vector-set! profile-iteration profile-ptr iter)
    (flvector-set! profile-time profile-ptr time)
    (set-baseline-machine-profile-ptr! machine (add1 profile-ptr))))
