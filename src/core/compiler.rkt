#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../utils/float.rkt"
         "../utils/timeline.rkt"
         "batch.rkt")

(provide compile-progs
         compile-prog)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<instr> ..+)
;; <instr> ::= '(<op-procedure> <index> ...)
;; ```
;; where <index> refers to a previous virtual register.
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter vars ivec rootvec)
  (define rootlen (vector-length rootvec))
  (define iveclen (vector-length ivec))
  (define varc (length vars))
  (define vregs (make-vector (+ varc iveclen)))
  (define (compiled-prog args)
    (vector-copy! vregs 0 args)
    (for ([instr (in-vector ivec)]
          [n (in-naturals varc)])
      (vector-set! vregs n (apply-instruction instr vregs)))
    (for/vector #:length rootlen
                ([root (in-vector rootvec)])
      (vector-ref vregs root)))
  compiled-prog)

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
    [(list op args ...) (apply op (map (curry vector-ref regs) args))]))

(define (if-proc c a b)
  (if c a b))

(define (batch-remove-approx batch)
  (batch-replace batch
                 (lambda (node)
                   (match node
                     [(approx spec impl) impl]
                     [node node]))))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
;; Translates a Herbie IR into an interpretable IR.
;; Requires some hooks to complete the translation.
(define (compile-progs exprs ctx)
  (define vars (context-vars ctx))
  (define num-vars (length vars))
  (define batch
    (if (batch? exprs)
        exprs
        (progs->batch exprs #:vars vars)))

  (timeline-push! 'compiler (batch-tree-size batch) (batch-length batch))

  ; Here we need to keep vars even though no roots refer to the vars
  (define batch* (batch-remove-zombie (batch-remove-approx batch) #:keep-vars #t))

  (define instructions
    (for/vector #:length (- (batch-length batch*) num-vars)
                ([node (in-vector (batch-nodes batch*) num-vars)])
      (match node
        [(literal value (app get-representation repr)) (list (const (real->repr value repr)))]
        [(list 'if c t f) (list if-proc c t f)]
        [(list op args ...) (cons (impl-info op 'fl) args)])))

  (make-progs-interpreter vars instructions (batch-roots batch*)))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
