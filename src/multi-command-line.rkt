#lang racket
(require (for-syntax syntax/parse))
(provide multi-command-line)

(define-syntax (multi-command-line stx)
  (syntax-parse stx
   [(_ #:program big-name args ... #:subcommands [name subargs ...] ... #:args else-subargs body)
    #'(let ([true-name big-name])
        (command-line
         #:program true-name
         #:args cmdline-args
         (match cmdline-args
           [(cons name rest)
            (multi-command-line
             #:program (format "~a ~a" true-name name)
             #:argv rest
             args ...
             subargs ...)] ...
           [fallthrough
            (apply (λ else-subargs body) fallthrough)])))]
   [(_ args ...)
    #'(command-line args ...)]))

