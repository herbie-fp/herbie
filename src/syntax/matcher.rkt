;; Minimal pattern matcher/substituter for S-expressions

#lang racket

(provide pattern-match
         pattern-substitute)

;; Pattern matcher that returns a substitution or #f.
;; A substitution is an immutable hash of symbols and expressions.
(define (pattern-match pattern expr)
  (let/ec fail
    (define (loop pattern expr bindings)
      (match* (pattern expr)
        [((? number?) _)
         (if (equal? pattern expr)
             bindings
             (fail #f))]
        [((? symbol?) _)
         (hash-update bindings
                      pattern
                      (lambda (old)
                        (if (equal? old expr)
                            old
                            (fail #f)))
                      expr)]
        [((list phead prest ...) (list head rest ...))
         (unless (and (equal? phead head) (= (length prest) (length rest)))
           (fail #f))
         (for/fold ([bindings bindings])
                   ([pat (in-list prest)]
                    [term (in-list rest)])
           (loop pat term bindings))]
        [(_ _) (fail #f)]))
    (loop pattern expr (hasheq))))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (match pattern
    [(? number?) pattern]
    [(? symbol?) (dict-ref bindings pattern)]
    [(list phead pargs ...) (cons phead (map (curryr pattern-substitute bindings) pargs))]))
