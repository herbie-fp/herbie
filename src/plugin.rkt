#lang racket
(require racket/lazy-require racket/runtime-path setup/getinfo)
(require (submod "syntax/types.rkt" internals) (submod "interface.rkt" internals)
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide define-type define-representation declare-parametric-operator define-operator define-ruleset)

(define (module-exists? module)
  (with-handlers ([exn:fail:filesystem:missing-module? (const false)])
    (dynamic-require module #f)
    true))

(define (load-herbie-plugins)
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when (and value (module-exists? value))
      (dynamic-require value #f))))

;; Legacy code to load softposit file (should be moved to softposit-rkt package)
(define-runtime-path posits-module "softposit.rkt")
(when (module-exists? 'softposit-rkt)
  (dynamic-require (make-resolved-module-path posits-module) #f))
