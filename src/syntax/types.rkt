#lang racket

(require "../utils/common.rkt"
         "../utils/errors.rkt")

(provide (struct-out representation)
         repr->prop
         (struct-out context)
         *context*
         context-extend
         context-lookup)

(module+ internals
  (provide define-type
           make-representation))

;; Types

(define type-dict (make-hasheq))
(define (type-name? x)
  (hash-has-key? type-dict x))

(define-syntax-rule (define-type name _ ...)
  (hash-set! type-dict 'name #t))

(define-type real)
(define-type bool)

;; Representations

(struct representation
        (name type repr? bf->repr repr->bf ordinal->repr repr->ordinal total-bits special-value? cost)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

;; Converts a representation into a rounding property
(define (repr->prop repr)
  (match (representation-type repr)
    ['bool '()]
    ['real (list (cons ':precision (representation-name repr)))]))

(define (make-representation #:name name
                             #:type type
                             #:repr? repr?
                             #:bf->repr bf->repr
                             #:repr->bf repr->bf
                             #:ordinal->repr ordinal->repr
                             #:repr->ordinal repr->ordinal
                             #:total-bits total-bits
                             #:special-value? special-value?
                             #:cost [cost #f])
  (unless (type-name? type)
    (raise-herbie-error "Tried to register a representation for type ~a: not found" type))
  (representation name
                  type
                  repr?
                  bf->repr
                  repr->bf
                  ordinal->repr
                  repr->ordinal
                  total-bits
                  special-value?
                  cost))

;; Contexts

(struct context (vars repr var-reprs) #:transparent)

;; Current context
(define *context* (make-parameter #f))

(define (context-extend ctx var repr)
  (struct-copy context
               ctx
               [vars (cons var (context-vars ctx))]
               [var-reprs (cons repr (context-var-reprs ctx))]))

(define (context-lookup ctx var)
  (dict-ref (map cons (context-vars ctx) (context-var-reprs ctx)) var))
