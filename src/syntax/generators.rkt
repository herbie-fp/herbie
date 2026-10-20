#lang racket

(require math/bigfloat
         ffi/unsafe
         racket/stxparam
         (for-syntax racket/base))

(require "rival.rkt"
         "../config.rkt"
         "types.rkt"
         "block.rkt")

(provide from-rival
         from-ffi
         from-libm
         from-bigfloat
         with-operator-signature
         (struct-out generator))

;;; FFI callouts need an operator's context at expansion time.
(begin-for-syntax
  (define (ffi2-available?)
    (collection-path "ffi2" #:fail (lambda (_) #f)))

  (define (current-operator-signature stx)
    (or (syntax-parameter-value #'operator-signature)
        (raise-syntax-error #f "generator used outside of `#:impl`" stx))))

(struct generator (gen))

(define-syntax-parameter operator-signature #f)

(define-syntax-rule (with-operator-signature signature body)
  (syntax-parameterize ([operator-signature (quote-syntax signature)])
    body))

(define-syntax-rule (define-generator ((name args ...) spec ctx)
                      body ...)
  (define (name args ...)
    (generator (lambda (spec ctx)
                 body ...))))

; ----------------------- RIVAL GENERATOR ---------------------------

(define/reset caches
              '()
              (lambda ()
                (for ([cache (caches)])
                  (hash-clear! cache))))

(define-generator ((from-rival #:cache? [cache? #t]) spec ctx)
  (define-values (block vs) (progs->block (list spec) #:ctx ctx))
  (define compiler (make-real-compiler block vs (list (context-repr ctx))))
  (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
  (define (compute . pt)
    (define-values (_ exs) (real-apply compiler (list->vector pt)))
    (if exs
        (first exs)
        fail))
  (cond
    [cache?
     (define cache (make-hash))
     (caches (cons cache (caches)))
     (lambda pt (hash-ref! cache pt (lambda () (apply compute pt))))]
    [else compute]))

; ----------------------- FFI GENERATOR -----------------------------

;; Looks up a function `name` with type signature `itype -> ... -> otype`
;; in the given FFI library and returns the function or `#f` if it
;; cannot be found.
(define (make-ffi lib name itypes otype)
  (define (repr->ffi repr)
    (match (representation-name repr)
      ['binary64 _double]
      ['binary32 _float]
      ['integer _int]
      [else (raise-syntax-error 'repr->type "unknown type" repr)]))
  (get-ffi-obj name
               lib
               (_cprocedure (map repr->ffi itypes) (repr->ffi otype) #:atomic? #t)
               (const #f)))

;; The signature syntax is the same context expression used by the runtime
;; generator. It cannot be the runtime context value, which does not exist yet.
(define-for-syntax (operator-signature-parts signature stx)
  (syntax-case signature ()
    [(_ _ output (_ input ...)) (values #'output (syntax->list #'(input ...)))]
    [_ (raise-syntax-error #f "invalid operator signature" stx)]))

;; Start generated identifiers in the call-site context so the lifted require
;; can bind them. Its fresh scope is shared by the import and references, so
;; ffi/unsafe's -> does not conflict with ffi2's ->.
(define-for-syntax (ffi2-id stx name) (datum->syntax stx name stx))

(define-for-syntax (ffi2-type-id stx repr)
  (define type-name
    (cond
      [(free-identifier=? repr #'<binary64>) 'double_t]
      [(free-identifier=? repr #'<binary32>) 'float_t]
      [(eq? (syntax-e repr) '<integer>) 'int_t]
      [else (raise-syntax-error 'from-ffi "unknown FFI representation" repr)]))
  (ffi2-id stx type-name))

(define-for-syntax (ffi2-signature stx inputs output)
  #`(#,(ffi2-id stx '->) #,@(for/list ([repr (in-list inputs)])
                              (ffi2-type-id stx repr))
                         #,(ffi2-type-id stx output)
                         #:atomic))

(define-for-syntax (lift-ffi2 stx form)
  ;; syntax-local-introduce lets the generated require escape this macro's
  ;; introduction scope; syntax-local-lift-require then scopes it together
  ;; with the generated references, avoiding conflicts with ffi/unsafe's ->.
  (syntax-local-introduce (syntax-local-lift-require
                           (syntax-local-introduce (datum->syntax stx 'ffi2 stx))
                           (syntax-local-introduce form))))

(define-for-syntax (ffi2-callout-form stx lib name inputs output)
  (define procedure-id (ffi2-id stx 'ffi2-procedure))
  (define lib-ref-id (ffi2-id stx 'ffi2-lib-ref))
  (define signature (ffi2-signature stx inputs output))
  (define output-name (datum->syntax stx (syntax->datum output) stx))
  (define input-names (datum->syntax stx (map syntax->datum inputs) stx))
  (lift-ffi2 stx
             #`(let ([lib* #,lib]
                     [name* #,name])
                 (#,procedure-id
                  (#,lib-ref-id lib*
                                name*
                                #:fail (lambda ()
                                         (error 'ffi-generator
                                                "Could not find FFI implementation of `~a ~a ~a`"
                                                '#,output-name
                                                name*
                                                '#,input-names)))
                  #,signature))))

(define-syntax (ffi-callout stx)
  (syntax-case stx ()
    [(_ lib name)
     (let-values ([(output inputs) (operator-signature-parts (current-operator-signature stx) stx)])
       (if (ffi2-available?)
           (ffi2-callout-form stx #'lib #'name inputs output)
           #`(let* ([lib* lib]
                    [name* name]
                    [ctx #,(current-operator-signature stx)]
                    [input-reprs (context-var-reprs ctx)]
                    [output-repr (context-repr ctx)])
               (or (make-ffi lib* name* input-reprs output-repr)
                   (error 'ffi-generator
                          "Could not find FFI implementation of `~a ~a ~a`"
                          output-repr
                          name*
                          input-reprs)))))]))

(define libm-lib (ffi-lib #f))

(define-syntax-rule (from-ffi lib name)
  (ffi-callout lib name))

(define-syntax-rule (from-libm name)
  (ffi-callout libm-lib name))

; ----------------------- BIGFLOAT GENERATOR ------------------------

(define (repr->bf x type)
  ((representation-repr->bf type) x))

(define (bf->repr x type)
  ((representation-bf->repr type) x))

(define-generator ((from-bigfloat name) spec ctx)
  (let* ([itypes (context-var-reprs ctx)]
         [otype (context-repr ctx)]
         [op (dynamic-require '(lib "math/bigfloat") name)]
         [working-precision (representation-total-bits otype)])
    (lambda pt
      (define pt* (map repr->bf pt itypes))
      (define bf-out
        (parameterize ([bf-precision working-precision])
          (apply op pt*)))
      (bf->repr bf-out otype))))
