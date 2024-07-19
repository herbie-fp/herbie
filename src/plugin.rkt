#lang racket

(require "syntax/types.rkt"
         "utils/errors.rkt"
         (submod "syntax/types.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         (submod "core/rules.rkt" internals)
         (submod "syntax/platform.rkt" internals)
         "syntax/defaccelerator.rkt")

(provide define-type
         define-representation
         define-operator-impl
         define-operator
         define-ruleset
         define-ruleset*
         register-ruleset!
         register-operator-impl!
         register-representation!
         register-representation-alias!
         register-conversion-generator!
         register-generator!
         register-operator!
         (struct-out representation)
         get-representation
         warn
         platform
         get-platform
         register-platform!
         platform-product
         platform-union
         platform-intersect
         platform-subtract
         platform-filter
         operator-set
         platform-operator-set
         with-terminal-cost
         cost-map
         cost-map-scale
         register-accelerator!
         register-accelerator-impl!
         define-accelerator
         define-accelerator-impl)
