#lang racket/base

(require racket/match
         racket/function
         racket/flonum)

(require "machine.rkt"
         "adjust.rkt"
         "../mpfr.rkt"
         "../ops/all.rkt")

(provide rival-machine-load
         rival-machine-run
         rival-machine-return
         rival-machine-adjust
         apply-instruction) ; for compile.rkt

(define (rival-machine-load machine args)
  (vector-copy! (rival-machine-registers machine) 0 args)
  (set-rival-machine-bumps! machine 0)
  (*bumps-activated* #f))

(define (rival-machine-record machine name number precision time memory iter)
  (define profile-ptr (rival-machine-profile-ptr machine))
  (define profile-instruction (rival-machine-profile-instruction machine))
  (when (< profile-ptr (vector-length profile-instruction))
    (define profile-number (rival-machine-profile-number machine))
    (define profile-time (rival-machine-profile-time machine))
    (define profile-memory (rival-machine-profile-memory machine))
    (define profile-precision (rival-machine-profile-precision machine))
    (define profile-iteration (rival-machine-profile-iteration machine))
    (vector-set! profile-instruction profile-ptr name)
    (vector-set! profile-number profile-ptr number)
    (vector-set! profile-memory profile-ptr memory)
    (vector-set! profile-precision profile-ptr precision)
    (vector-set! profile-iteration profile-ptr iter)
    (flvector-set! profile-time profile-ptr time)
    (set-rival-machine-profile-ptr! machine (add1 profile-ptr))))

(define (rival-machine-run machine vhint)
  (define ivec (rival-machine-instructions machine))
  (define varc (vector-length (rival-machine-arguments machine)))
  (define precisions (rival-machine-precisions machine))
  (define initial-precisions (rival-machine-initial-precisions machine))
  (define repeats (rival-machine-repeats machine))
  (define initial-repeats (rival-machine-initial-repeats machine))
  (define vregs (rival-machine-registers machine))
  (define iter (rival-machine-iteration machine))
  ; parameter for sampling histogram table
  (define first-iter? (zero? (rival-machine-iteration machine)))

  (for ([instr (in-vector ivec)]
        [n (in-naturals varc)]
        [precision (in-vector (if first-iter? initial-precisions precisions))]
        [repeat (in-vector (if first-iter? initial-repeats repeats))]
        [hint (in-vector vhint)]
        #:unless (or (not hint) repeat))
    (define out
      (match hint
        [#t ; instruction should be reevaluated
         (define start (current-inexact-milliseconds))
         (define mem-before (current-memory-use #f))
         (define res
           (parameterize ([bf-precision precision])
             (apply-instruction instr vregs)))
         (define name (object-name (car instr)))
         (define time (- (current-inexact-milliseconds) start))
         (define memory-delta (max 0 (- (current-memory-use #f) mem-before)))
         (rival-machine-record machine name n precision time memory-delta iter)
         res]
        [(? integer? _) (vector-ref vregs (list-ref instr hint))] ; result is known
        [(? ival? _) hint])) ; result is known
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

(define (rival-machine-return machine)
  (define discs (rival-machine-discs machine))
  (define vregs (rival-machine-registers machine))
  (define rootvec (rival-machine-outputs machine))
  (define slackvec (rival-machine-output-distance machine))
  (define good? #t)
  (define done? #t)
  (define bad? #f)
  (define stuck? #f)
  (define fvec
    (for/vector #:length (vector-length rootvec)
                ([root (in-vector rootvec)]
                 [disc (in-vector discs)]
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
      (vector-set! slackvec n (= distance 1))
      lo))
  (values good? (and good? done?) bad? stuck? fvec))

(define (rival-machine-adjust machine vhint)
  (define iter (rival-machine-iteration machine))
  (let ([start (current-inexact-milliseconds)])
    (define mem-before (current-memory-use #f))
    (unless (zero? iter)
      (backward-pass machine vhint))
    (rival-machine-record machine
                          'adjust
                          -1
                          (* iter 1000)
                          (- (current-inexact-milliseconds) start)
                          (max 0 (- (current-memory-use #f) mem-before))
                          iter)))
