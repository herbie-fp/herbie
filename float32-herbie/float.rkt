#lang racket

(require ffi/unsafe)
(provide float32-neg
         float32-add
         float32-sub
         float32-mul
         float32-div
         float32->ordinal
         ordinal->float32)

(define float32-neg (get-ffi-obj "neg_f32" "float32-arith" (_fun _float -> _float)))
(define float32-add (get-ffi-obj "add_f32" "float32-arith" (_fun _float _float -> _float)))
(define float32-sub (get-ffi-obj "sub_f32" "float32-arith" (_fun _float _float -> _float)))
(define float32-mul (get-ffi-obj "mul_f32" "float32-arith" (_fun _float _float -> _float)))
(define float32-div (get-ffi-obj "div_f32" "float32-arith" (_fun _float _float -> _float)))

(define float32->ordinal (get-ffi-obj "f32_to_ordinal" "float32-arith" (_fun _float -> _int)))
(define ordinal->float32 (get-ffi-obj "ordinal_to_f32" "float32-arith" (_fun _int -> _float)))