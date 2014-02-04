#lang racket

(define (my-match pattern expr)
  ; pattern expr -> bindings

  (define (merge . bindings)
    ; (list bindings) -> binding
    (foldl merge2 '() bindings))

  (define (merge2 binding1 binding2)
    ; binding binding -> binding
    (let loop ([acc binding1] [rest binding2])
      (if (null? rest)
          acc
          (let* ([curr (car rest)]
                 [lookup (assoc (car curr) acc)])
            (if lookup
                (if (equal? (cdr lookup) (cdr curr))
                    (loop acc (cdr rest))
                    (fail "my-match: Variable has two different bindings"
                          (car curr) (cdr lookup) (cdr curr)))
                (loop (cons curr acc) (cdr rest)))))))

  (define (fail . irr)
    (print irr)
    (/ 0 0))

  ; patterns:
  ; ,a -> variable "a"
  ; numbers -> literals
  ; (op _ _ _) -> a list with _ being recursively patterns

  (cond
   [(number? pattern)
    (if (and (number? expr) (= pattern expr))
        '()
        (fail "my-match: Literals do not match"
              pattern expr))]
   [(and (list? pattern) (eq? (car pattern) 'unquote))
    (let ([var (cadr pattern)])
      (list (cons var expr)))]
   ; TODO : test for allowed operators
   [(and (list? pattern) (> (length pattern) 1))
    (if (and (list? expr) (eq? (car expr) (car pattern))
             (= (length expr) (length pattern)))
        (apply merge
         (for/list ([pat (cdr pattern)] [subterm (cdr expr)])
           (my-match pat subterm)))
        (fail "my-match: Not a list, or wrong length, or wrong operator."
              "Don't ask me, I don't know!"
              pattern expr))]
   [#t (fail "my-match: Confused by pattern term" pattern)]))
