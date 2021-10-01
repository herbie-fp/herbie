#lang racket

(require math/bigfloat math/flonum)
(require "syntax/types.rkt" "errors.rkt")

(provide (struct-out representation) get-representation
          *output-repr* *var-reprs* *needed-reprs* *reprs-with-rules*
          real->repr repr->real
          generate-repr)

(module+ internals 
  (provide define-representation register-generator! register-representation!))

(define *reprs-with-rules* (make-parameter '()))
(define *needed-reprs* (make-parameter '()))
(define *output-repr* (make-parameter #f))
(define *var-reprs* (make-parameter '()))

;; Structs

(struct representation
  (name type repr?
   bf->repr repr->bf ordinal->repr repr->ordinal
   total-bits special-values)
  #:methods gen:custom-write
  [(define (write-proc repr port mode)
     (fprintf port "#<representation ~a>" (representation-name repr)))])

(define representations (hash))

;; Repr / operator generation
;; Some plugins might define 'parameterized' reprs (e.g. fixed point with
;; m integer and n fractional bits). Since defining an infinite number of reprs
;; is impossible, Herbie stores a list of 'repr generators' to query if it comes
;; across a repr that is not known at the time. 

;; Generators take one argument, a repr name, and returns true if knows what the
;; repr is and has generated that repr and its operators, and false otherwise
(define repr-generators '())

(define/contract (register-generator! proc)
  (-> (-> any/c boolean?) void?)
  (unless (set-member? repr-generators proc)
    (set! repr-generators (cons proc repr-generators))))

;; Queries each plugin to generate the representation
(define (generate-repr repr-name)
  (or (hash-has-key? representations repr-name)
      (for/or ([proc repr-generators])
        (proc repr-name))))

;; Returns the representation associated with `name`
;; attempts to generate the repr if not initially found
(define (get-representation name)
  (or (hash-ref representations name #f)
      (and (generate-repr name) (hash-ref representations name #f))
      (raise-herbie-error "Could not find support for ~a representation" name)))

(define (register-representation! name type repr? . args)
  (set! representations
    (hash-set representations name
              (apply representation name (get-type type) repr? args))))

(define-syntax-rule (define-representation (name type repr?) args ...)
  (register-representation! 'name 'type repr? args ...))

;; repr <==> real

(define (real->repr x repr)
  ((representation-bf->repr repr) (bf x)))

(define (repr->real x repr)
  (match x
    [(? boolean?) x]
    [_ (bigfloat->real ((representation-repr->bf repr) x))]))

;; Predicates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; representations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
