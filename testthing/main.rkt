#lang typed/racket
(require x64asm)

(define-cast d->d
  #:type (Flonum -> Flonum)
  #:ctype (_fun _float -> _float))

(define-λ! rcp d->d
  (rcpss xmm0 xmm0)
  (ret))

(define-λ! rsqrt d->d
  (rsqrtss xmm0 xmm0)
  (ret))

(module+ main
  (eprintf "rcp(3.14) -> ~a\n" (rcp 3.14))
  (eprintf "rsqrt(3.14) -> ~a\n" (rsqrt 3.14)))
