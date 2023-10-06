#lang racket

;; "Hardware" platform:
;;  - double-precision, single-precision, bool types
;;  - floating-point arithmetic operators, sqrt, log2
;;  - boolean operators

(require math/flonum math/bigfloat ffi/unsafe)
(require "runtime/bool.rkt"
         "runtime/binary64.rkt"
         "runtime/binary32.rkt"
         "runtime/float32.rkt"
         "runtime/utils.rkt"
         (only-in "runtime/libm.rkt"
                  define-binary64-impls/libm
                  define-binary32-impls/libm))

(define-operator-impl (neg neg.f64 binary64) binary64 [fl -])
(define-operator-impl (+ +.f64 binary64 binary64) binary64 [fl +])
(define-operator-impl (- -.f64 binary64 binary64) binary64 [fl -])
(define-operator-impl (* *.f64 binary64 binary64) binary64 [fl *])
(define-operator-impl (/ /.f64 binary64 binary64) binary64 [fl /])

(define-operator-impl (neg neg.f32 binary32) binary32 [fl fl32-])
(define-operator-impl (+ +.f32 binary32 binary32) binary32 [fl fl32+])
(define-operator-impl (- -.f32 binary32 binary32) binary32 [fl fl32-])
(define-operator-impl (* *.f32 binary32 binary32) binary32 [fl fl32*])
(define-operator-impl (/ /.f32 binary32 binary32) binary32 [fl fl32/])

(define-binary64-impls/libm
  [sqrt log2 fabs floor ceil]
  [fmin fmax])

(define-binary32-impls/libm
  [sqrt log2 fabs floor ceil]
  [fmin fmax])

(define-syntax-rule (define-comparator-binary64-impls [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name binary64 binary64) bool
      [fl impl-fn])
    ...))

(define-syntax-rule (define-comparator-binary32-impls [name impl-name impl-fn] ...)
  (begin
    (define-operator-impl (name impl-name binary32 binary32) bool
      [fl impl-fn])
    ...))

(define-comparator-binary64-impls
  [== ==.f64 =]
  [!= !=.f64 (negate =)]
  [< <.f64 <]
  [> >.f64 >]
  [<= <=.f64 <=]
  [>= >=.f64 >=])

(define-comparator-binary32-impls
  [== ==.f32 =]
  [!= !=.f32 (negate =)]
  [< <.f32 <]
  [> >.f32 >]
  [<= <=.f32 <=]
  [>= >=.f32 >=])

(define-operator-impl (cast binary64->binary32 binary64) binary32
  [fl (curryr ->float32)])

(define-operator-impl (cast binary32->binary64 binary32) binary64
  [fl identity])
