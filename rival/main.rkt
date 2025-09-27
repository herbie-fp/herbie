#lang racket/base

(require math/bigfloat
         racket/contract)
(define value? (or/c bigfloat? boolean?))

(require "ops/all.rkt"
         "infra/run-baseline.rkt")
(define ival-list? (listof ival?))

(provide ival?
         ival
         (contract-out [ival-lo (-> ival? value?)]
                       [ival-hi (-> ival? value?)]
                       [monotonic->ival (-> (-> value? value?) (-> ival? ival?))]
                       [comonotonic->ival (-> (-> value? value?) (-> ival? ival?))]
                       [ival-union (-> ival? ival? ival?)]
                       [ival-split (-> ival? value? (values (or/c ival? #f) (or/c ival? #f)))]
                       [close-enough->ival (-> (-> value? value? boolean?) (-> ival? ival?))])
         (contract-out [ival-pi (-> ival?)]
                       [ival-e (-> ival?)]
                       [ival-bool (-> boolean? ival?)]
                       [ival-add (-> ival? ival? ival?)]
                       [ival-sub (-> ival? ival? ival?)]
                       [ival-neg (-> ival? ival?)]
                       [ival-mult (-> ival? ival? ival?)]
                       [ival-div (-> ival? ival? ival?)]
                       [ival-fma (-> ival? ival? ival? ival?)] ; TODO: untested
                       [ival-fabs (-> ival? ival?)]
                       [ival-sqrt (-> ival? ival?)]
                       [ival-cbrt (-> ival? ival?)]
                       [ival-hypot (-> ival? ival? ival?)]
                       [ival-exp (-> ival? ival?)]
                       [ival-exp2 (-> ival? ival?)]
                       [ival-expm1 (-> ival? ival?)]
                       [ival-log (-> ival? ival?)]
                       [ival-log2 (-> ival? ival?)]
                       [ival-log10 (-> ival? ival?)]
                       [ival-log1p (-> ival? ival?)]
                       [ival-logb (-> ival? ival?)]
                       [ival-pow (-> ival? ival? ival?)]
                       [ival-sin (-> ival? ival?)]
                       [ival-cos (-> ival? ival?)]
                       [ival-tan (-> ival? ival?)]
                       [ival-asin (-> ival? ival?)]
                       [ival-acos (-> ival? ival?)]
                       [ival-atan (-> ival? ival?)]
                       [ival-atan2 (-> ival? ival? ival?)]
                       [ival-sinh (-> ival? ival?)]
                       [ival-cosh (-> ival? ival?)]
                       [ival-tanh (-> ival? ival?)]
                       [ival-asinh (-> ival? ival?)]
                       [ival-acosh (-> ival? ival?)]
                       [ival-atanh (-> ival? ival?)]
                       [ival-erf (-> ival? ival?)]
                       [ival-erfc (-> ival? ival?)]
                       [ival-lgamma (-> ival? ival?)]
                       [ival-tgamma (-> ival? ival?)]
                       [ival-fmod (-> ival? ival? ival?)]
                       [ival-remainder (-> ival? ival? ival?)]
                       [ival-rint (-> ival? ival?)]
                       [ival-round (-> ival? ival?)]
                       [ival-ceil (-> ival? ival?)]
                       [ival-floor (-> ival? ival?)]
                       [ival-trunc (-> ival? ival?)]
                       [ival-fmin (-> ival? ival? ival?)]
                       [ival-fmax (-> ival? ival? ival?)]
                       [ival-copysign (-> ival? ival? ival?)]
                       [ival-fdim (-> ival? ival? ival?)]
                       [ival-sort (-> ival-list? (-> value? value? boolean?) ival-list?)])
         (contract-out [ival-< (->* () #:rest (listof ival?) ival?)]
                       [ival-<= (->* () #:rest (listof ival?) ival?)]
                       [ival-> (->* () #:rest (listof ival?) ival?)]
                       [ival->= (->* () #:rest (listof ival?) ival?)]
                       [ival-== (->* () #:rest (listof ival?) ival?)]
                       [ival-!= (->* () #:rest (listof ival?) ival?)]
                       [ival-if (-> ival? ival? ival? ival?)]
                       [ival-and (->* () #:rest (listof ival?) ival?)]
                       [ival-or (->* () #:rest (listof ival?) ival?)]
                       [ival-not (-> ival? ival?)])
         (contract-out [ival-error? (-> ival? ival?)]
                       [ival-illegal ival?]
                       [ival-assert (->* (ival?) (values) ival?)]
                       [ival-then (->* (ival?) #:rest (listof ival?) ival?)])
         ; Deprecated
         ival-lo-fixed?
         ival-hi-fixed?
         ival-err?
         ival-err
         mk-ival)

(require "eval/main.rkt"
         (only-in "eval/machine.rkt" rival-machine?))
(provide (contract-out
          [rival-compile (-> (listof any/c) (listof symbol?) (listof discretization?) rival-machine?)]
          [rival-apply
           (->* (rival-machine? (vectorof value?))
                ((or/c (vectorof any/c) boolean?))
                (vectorof any/c))]
          [rival-analyze-with-hints
           (->* (rival-machine? (vectorof ival?)) ((or/c (vectorof any/c) boolean?)) (listof any/c))]
          [rival-analyze (-> rival-machine? (vectorof ival?) ival?)]
          [rival-profile (-> rival-machine? symbol? any/c)]
          [baseline-compile
           (-> (listof any/c) (listof symbol?) (listof discretization?) baseline-machine?)]
          [baseline-analyze
           (->* (baseline-machine? (vectorof ival?))
                ((or/c (vectorof any/c) boolean?))
                (listof any/c))]
          [baseline-apply
           (->* (baseline-machine? (vectorof value?))
                ((or/c (vectorof any/c) boolean?))
                (vectorof any/c))]
          [baseline-profile (-> baseline-machine? symbol? any/c)])
         (struct-out baseline-machine)
         (struct-out discretization)
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         *rival-max-precision*
         *rival-max-iterations*
         *rival-profile-executions*
         *rival-use-shorthands*
         *rival-name-constants*
         (struct-out execution))

(require "utils.rkt")
(provide flonum-discretization
         boolean-discretization
         bf-discretization)

(module+ main
  (require "repl.rkt"
           racket/cmdline)
  (command-line #:program "racket -l rival"
                #:args ([file #f])
                (if file
                    (call-with-input-file file rival-repl)
                    (rival-repl (current-input-port)))))
