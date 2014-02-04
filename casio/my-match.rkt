#lang racket

;; Our own pattern matcher.
;
; The racket (match) macro doesn't give us access to the bindings made
; by the matcher, so we wrote our own.
;
; The syntax is simple:
;   numbers are literals ; symbols are variables ; lists are expressions
;
; Bindings are stored as association lists

(define (pattern-match pattern expr)
  ; pattern expr -> bindings

  (define (merge . bindings)
    ; (list bindings) -> binding
    (foldl merge2 '() bindings))

  (define (merge2 binding1 binding2)
    ; binding binding -> binding
    (if (and binding1 binding2)
        (let loop ([acc binding1] [rest binding2])
          (if (null? rest)
              acc
              (let* ([curr (car rest)]
                     [lookup (assoc (car curr) acc)])
                (if lookup
                    (if (equal? (cdr lookup) (cdr curr))
                        (loop acc (cdr rest))
                        (fail "pattern-match: Variable has two different bindings"
                              (car curr) (cdr lookup) (cdr curr)))
                    (loop (cons curr acc) (cdr rest))))))
        #f))

  (define (fail . irr) #f)

  (cond
   [(number? pattern)
    (if (and (number? expr) (= pattern expr))
        '()
        (fail "pattern-match: Literals do not match"
              pattern expr))]
   [(symbol? pattern)
    (list (cons pattern expr))]
   ; TODO : test for allowed operators
   [(list? pattern)
    (if (and (list? expr) (eq? (car expr) (car pattern))
             (= (length expr) (length pattern)))
        (apply merge
         (for/list ([pat (cdr pattern)] [subterm (cdr expr)])
           (pattern-match pat subterm)))
        (fail "pattern-match: Not a list, or wrong length, or wrong operator."
              "Don't ask me, I don't know!"
              pattern expr))]
   [#t (fail "pattern-match: Confused by pattern term" pattern)]))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (cond
   [(number? pattern) pattern]
   [(symbol? pattern)
    (cdr (assoc pattern bindings))]
   [(list? pattern)
    (cons (car pattern)
          (for/list ([pat (cdr pattern)])
            (pattern-substitute pat bindings)))]
   [#t (error "pattern-substitute: Confused by pattern term" pattern)]))

; Now for rules.
