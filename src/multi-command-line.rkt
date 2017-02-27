#lang racket
(require (for-syntax syntax/parse))
(provide multi-command-line)

(define-syntax (multi-command-line stx)
  (syntax-parse stx
   [(_ #:program big-name args ... #:subcommands [name:id help:str subargs ...] ... #:args else-subargs body)
    #`(let ([true-name big-name])
        (command-line
         #:program true-name
         #:usage-help
         "This command has subcommands:"
         #,@(for/list ([name (syntax->list #'(name ...))] [help (syntax->list #'(help ...))])
              (datum->syntax name (format "  ~a:\t~a" (syntax->datum name) (syntax->datum help))))
         "Learn more about a subcommand with <subcommand> --help"
         #:args cmdline-args
         (match cmdline-args
           [(cons (== (~a 'name)) rest)
            (multi-command-line
             #:program (format "~a ~a" true-name 'name)
             #:argv rest
             args ...
             subargs ...)] ...
           [fallthrough
            (apply (λ else-subargs body) fallthrough)])))]
   [(_ args ...)
    #'(command-line args ...)]))

