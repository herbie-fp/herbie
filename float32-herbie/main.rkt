#lang racket

(require herbie/plugin math/bigfloat rival "float.rkt")

; This plugin provides support for single flonum arithmetic if single flonums
; are not supported in Racket (most likely running on CS)
; Only load it if needed

(unless (single-flonum-available?)
  (printf "Loading single float (arithmetic-only) ...\n")

  (define (shift bits fn)
    (define shift-val (expt 2 bits))
    (λ (x) (fn (- x shift-val))))

  (define (unshift bits fn)
    (define shift-val (expt 2 bits))
    (λ (x) (+ (fn x) shift-val)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;; Representation ;;;;;;;;;;;;;;;;;;;;;;;;

  (define-representation (binary32 real flonum?)
    bigfloat->flonum
    bf
    (shift 31 ordinal->float32)
    (unshift 31 float32->ordinal)
    32
    (disjoin nan? infinite?))  

  (define-operator (+ +.f32 binary32 binary32) binary32 
    [fl float32-add] [bf bf+] [ival ival-add] [nonffi +])

  (define-operator (- -.f32 binary32 binary32) binary32
    [fl float32-sub] [bf bf-] [ival ival-sub] [nonffi -])

  (define-operator (- neg.f32 binary32) binary32
    [fl float32-neg] [bf bf-] [ival ival-neg] [nonffi -])

  (define-operator (* *.f32 binary32 binary32) binary32
    [fl float32-mul] [bf bf*] [ival ival-mult] [nonffi *])

  (define-operator (/ /.f32 binary32 binary32) binary32
    [fl float32-div] [bf bf/] [ival ival-div] [nonffi /])
)