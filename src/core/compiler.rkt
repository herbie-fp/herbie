#lang racket

(require "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../syntax/platform.rkt"
         "../syntax/float.rkt"
         "../utils/timeline.rkt"
         "../utils/common.rkt"
         "../utils/dvector.rkt"
         "../syntax/block.rkt")

(provide compile-progs
         compile-block
         compile-prog)

(struct compiler-instruction (op argidxs array?) #:transparent)

;; Interpreter taking a narrow IR
;; ```
;; <prog> ::= #(<thunk> ..+)
;; ```
;; Must also provide the input variables for the program(s)
;; as well as the indices of the roots to extract.
(define (make-progs-interpreter tvec rootvec args vregs)
  (define rootlen (vector-length rootvec))
  (define (compiled-prog args*)
    (vector-copy! args 0 args*)
    (for ([thunk (in-vector tvec)])
      (thunk))
    (for/vector #:length rootlen
                ([root (in-vector rootvec)])
      (vector-ref vregs root)))
  compiled-prog)

(define (make-thunk op argidxs outidx regs)
  (match argidxs
    ['() (λ () (vector-set! regs outidx (op)))]
    [(list a) (λ () (vector-set! regs outidx (op (vector-ref regs a))))]
    [(list a b) (λ () (vector-set! regs outidx (op (vector-ref regs a) (vector-ref regs b))))]
    [(list a b c)
     (λ ()
       (vector-set! regs outidx (op (vector-ref regs a) (vector-ref regs b) (vector-ref regs c))))]
    [(list args ...)
     (define argv (list->vector args))
     (λ ()
       (vector-set! regs
                    outidx
                    (apply op
                           (for/list ([arg (in-vector argv)])
                             (vector-ref regs arg)))))]))

(define (make-array-thunk argidxs outidx regs)
  (define argv (list->vector argidxs))
  (λ ()
    (vector-set! regs
                 outidx
                 (for/vector #:length (vector-length argv)
                             ([arg (in-vector argv)])
                   (vector-ref regs arg)))))

(define (array-impl? op)
  (define repr (impl-info op 'otype))
  (and (array-representation? repr) (eq? op (array-impl-name repr))))

;; This function:
;;   1) copies only nodes associated with provided vs - so, gets rid of useless nodes
;;   2) rewrites these nodes as fl-instructions
(define (block-for-compiler block vs vars args)
  (define out (make-dvector (block-length block)))
  (define (compile-node v recurse)
    (match (val-def v)
      [(approx _ impl) (recurse impl)] ;; do not push, it is already compiled
      [(? symbol? n)
       (define idx (index-of vars n))
       (dvector-add! out (compiler-instruction (λ () (vector-ref args idx)) '() #f))]
      [(literal value (app get-representation repr))
       (dvector-add! out (compiler-instruction (const (real->repr value repr)) '() #f))]
      [(list (? array-impl?) args ...)
       (dvector-add! out (compiler-instruction #f (map recurse args) #t))]
      [(list op args ...)
       (dvector-add! out (compiler-instruction (impl-info op 'fl) (map recurse args) #f))]))
  (define roots (map (block-recurse block compile-node) vs))
  (values (dvector->vector out) roots))

(define (allocate-thunks instructions roots)
  (define instruction-count (vector-length instructions))
  (define last-use
    (for/vector #:length instruction-count
                ([i (in-range instruction-count)])
      i))
  (for ([instruction (in-vector instructions)]
        [i (in-naturals)])
    (for ([arg (in-list (compiler-instruction-argidxs instruction))])
      (vector-set! last-use arg i)))
  (for ([root (in-list roots)])
    ;; Roots must survive until the interpreter copies out all results.
    (vector-set! last-use root instruction-count))

  (define ending-at (make-vector (add1 instruction-count) '()))
  (for ([end (in-vector last-use)]
        [register (in-naturals)])
    (vector-set! ending-at end (cons register (vector-ref ending-at end))))

  (define register-map (make-vector instruction-count))
  (define free-registers '())
  (define register-count 0)
  (for ([i (in-range instruction-count)])
    ;; Thunks read their inputs before saving the result, so a last-use input can be overwritten.
    (for ([register (in-list (vector-ref ending-at i))]
          #:when (< register i))
      (set! free-registers (cons (vector-ref register-map register) free-registers)))
    (define outidx
      (match free-registers
        ['()
         (begin0 register-count
           (set! register-count (add1 register-count)))]
        [(cons register rest)
         (set! free-registers rest)
         register]))
    (vector-set! register-map i outidx))

  (define vregs (make-vector register-count))
  (define thunks
    (for/vector #:length instruction-count
                ([instruction (in-vector instructions)]
                 [i (in-naturals)])
      (match-define (compiler-instruction op argidxs array?) instruction)
      (define physical-args
        (for/list ([arg (in-list argidxs)])
          (vector-ref register-map arg)))
      (define outidx (vector-ref register-map i))
      (if array?
          (make-array-thunk physical-args outidx vregs)
          (make-thunk op physical-args outidx vregs))))
  (define physical-roots
    (for/vector #:length (length roots)
                ([root (in-list roots)])
      (vector-ref register-map root)))
  (values thunks physical-roots vregs))

(define (block-tree-size block vs)
  (define (tree-size v recurse)
    (define args (reap [sow] (expr-recurse (val-def v) sow)))
    (apply + 1 (map recurse args)))
  (apply + (map (block-recurse block tree-size) vs)))

;; Compiles a program of operator implementations into a procedure
;; that evaluates the program on a single input of representation values
;; returning representation values.
;; Translates a Herbie IR into a vector of thunks.
(define (compile-progs exprs ctx)
  (define-values (block vs) (progs->block exprs #:ctx ctx))
  (compile-block block vs))

(define (compile-block block vs)
  (define vars (block-vars block))
  (define args (make-vector (length vars)))
  (define-values (instructions roots) (block-for-compiler block vs vars args))
  (define-values (thunks rootvec vregs) (allocate-thunks instructions roots))
  (timeline-push! 'compiler (block-tree-size block vs) (vector-length thunks))
  (make-progs-interpreter thunks rootvec args vregs))

;; Like `compile-progs`, but a single prog.
(define (compile-prog expr ctx)
  (define core (compile-progs (list expr) ctx))
  (define (compiled-prog . xs)
    (vector-ref (apply core xs) 0))
  compiled-prog)
