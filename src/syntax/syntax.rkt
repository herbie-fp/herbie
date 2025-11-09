#lang racket

(require math/bigfloat
         racket/hash
         (only-in rival/eval/main rival-functions))

(require "../utils/common.rkt"
         "../utils/errors.rkt"
         "matcher.rkt"
         "types.rkt")

(provide (struct-out literal)
         (struct-out approx)
         (struct-out hole)
         operator-exists?
         operator-info
         all-operators ; return a list of operators names
         *functions*
         register-function!
         (struct-out operator-impl)) ; required by platform.rkt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real operators
;; Pure mathematical operations

;; TODO: specs should really be associated with impls
;; unfortunately Herbie still mandates that every impl
;; has an associated operator so the spec is here

;; All real operators come from Rival

;; Checks if an operator has been registered.
(define (operator-exists? op)
  (hash-has-key? rival-functions op))

;; Returns all operators.
(define (all-operators)
  (sort (hash-keys rival-functions) symbol<?))

;; Looks up a property `field` of a real operator `op`.
;; Panics if the operator is not found.
(define/contract (operator-info op field)
  (-> symbol? (or/c 'itype 'otype) any/c)
  (define info
    (hash-ref rival-functions op (lambda () (raise-arguments-error 'operator-info
                                                                   "Unknown operator"
                                                                   "op"
                                                                   op))))
  (match-define (cons otype itypes) info)
  (case field
    [(itype) itypes]
    [(otype) otype]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator implementations
;; Floating-point operations that approximate mathematical operations

;; Operator implementations _approximate_ a program of
;; mathematical operators with fixed input and output representations.
;;
;; An operator implementation requires
;;  - a (unique) name
;;  - input variables/representations
;;  - output representation
;;  - a specification it approximates
;;  - its FPCore representation
;;  - a floating-point implementation
;;
(struct operator-impl (name ctx spec fpcore fl cost aggregate))

;; Floating-point expressions require that numbers
;; be rounded to a particular precision.
(struct literal (value precision) #:prefab)

;; An approximation of a specification by
;; a floating-point expression.
(struct approx (spec impl) #:prefab)

;; An unknown floating-point expression that implements a given spec
(struct hole (precision spec) #:prefab)

;; name -> (vars repr body)	;; name -> (vars prec body)
(define *functions* (make-parameter (make-hasheq)))

(define (register-function! name args repr body) ;; Adds a function definition.
  (hash-set! (*functions*) name (list args repr body)))
