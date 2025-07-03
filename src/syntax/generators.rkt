#lang racket

(require math/flonum
         math/bigfloat
         "../core/rival.rkt"
         "types.rkt"
         "../platforms/runtime/libm.rkt"
         "../utils/common.rkt")

(provide correct-rounding
         from-libm
         generator?)

(define (generator? obj)
  (equal? (object-name obj) 'generator))

; ----------------------- RIVAL GENERATOR ---------------------------
(define (correct-rounding)
  (define (generate-correctly-rounded-function spec ctx)
    (define compiler (make-real-compiler (list spec) (list ctx)))
    (define fail ((representation-bf->repr (context-repr ctx)) +nan.bf))
    (lambda pt
      (define-values (_ exs) (real-apply compiler (list->vector pt)))
      (if exs
          (first exs)
          fail)))
  (procedure-rename generate-correctly-rounded-function 'generator))

; ----------------------- LIBM GENERATOR ----------------------------

;; Libm's accelerators
(define (libm-optimize spec)
  (match spec
    [`(- 1 (erf ,x)) `(erfc ,x)]
    [`(- (exp ,x) 1) `(expm1 ,x)]
    [`(log (+ 1 ,x)) `(log1p ,x)]
    [`(sqrt (+ (* ,x ,x) (* ,y ,y))) `(hypot ,x ,y)]
    [`(+ (* ,x ,y) ,z) `(fma ,x ,y ,z)]
    [_ spec]))

;; Reprs -> ctypegit s
(define (repr->ctype repr)
  (match (representation-name repr)
    ['binary64 'double]
    ['binary32 'float]
    ['integer 'integer]
    [_ (error 'repr->ctype "Unresolved c-type for representation ~a" (representation-name repr))]))

;; Libm generator tries to generate a single-operator implementation of spec,
;; panics if there are more than one operator in spec
(define (from-libm)
  (define (generate-libm-function spec ctx)
    (match (libm-optimize spec)
      [(list op (? symbol? args) ...)
       (let* ([itypes (map repr->ctype (context-var-reprs ctx))]
              [otype (repr->ctype (context-repr ctx))]
              [cname (if (equal? otype 'float)
                         (sym-append op 'f)
                         op)])
         (define fl (make-libm-runtime cname itypes otype))
         (unless fl
           (error 'libm-generator
                  "Could not find libm implementation of `~a ~a ~a`"
                  otype
                  cname
                  itypes))
         fl)]
      [_
       (error 'libm-generator
              "Could not find a libm single-operator implementation of `~a` for ~a context"
              spec
              ctx)]))
  (procedure-rename generate-libm-function 'generator))
