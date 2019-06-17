#lang racket
(require racket/lazy-require racket/runtime-path)

(define (module-exists? module)
  (with-handlers ([exn:fail:filesystem:missing-module? (const false)])
    (dynamic-require module #f)
    true))

(define-runtime-path posits-module "softposit.rkt")
(when (module-exists? 'softposit-rkt)
  (dynamic-require (make-resolved-module-path posits-module) #f))
