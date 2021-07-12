#lang racket
(require setup/getinfo)
(require (submod "syntax/types.rkt" internals) (submod "interface.rkt" internals)
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide define-type define-representation define-constant-impl define-operator-impl
         define-constant define-operator define-ruleset define-ruleset*
         register-ruleset! register-constant-impl! register-operator-impl! register-representation! 
         register-generator! register-constant! register-operator!
         load-herbie-plugins load-herbie-builtins)

(define (module-exists? module)
  (with-handlers ([exn:fail:filesystem:missing-module? (const false)])
    (dynamic-require module #f)
    true))

(define (load-herbie-builtins)
  (dynamic-require 'herbie/bool #f)
  (dynamic-require 'herbie/binary64 #f)
  (dynamic-require 'herbie/binary32 #f))

(define (load-herbie-plugins)
  (load-herbie-builtins)    ; automatically load default representations
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when (and value (module-exists? value))
      (dynamic-require value #f))))
