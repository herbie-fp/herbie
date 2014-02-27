#lang racket
(require casio/common)
(require casio/programs)

(provide *rules* pattern-match rewrite-expression rewrite-tree change-apply (struct-out change) (struct-out rule))

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

(struct rule (name input output)
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (display "#<rule " port)
           (write (rule-name rule) port)
           (display ">" port))])

(define *rules* '())

(define-syntax (define-rule stx)
  (syntax-case stx ()
    [(_ name input output)
     #'(set! *rules* (cons (rule 'name 'input 'output) *rules*))]))

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

(define (rule-apply-force-destructs rule expr)
  (and (not (symbol? (rule-input rule))) (rule-apply rule expr)))

(struct change (rule location bindings) #:transparent
        #:methods gen:custom-write
        [(define (write-proc cng port mode)
           (display "#<change " port)
           (write (rule-name (change-rule cng)) port)
           (display " at " port)
           (write (change-location cng) port)
           (let ([bindings (change-bindings cng)])
             (when (not (null? bindings))
               (display " with " port)
               (for ([bind bindings])
                 (write (car bind) port)
                 (display "=" port)
                 (write (cdr bind) port)
                 (display ", " port))))
           (display ">" port))])

(define (rewrite-expression expr #:destruct [destruct? #f] #:root [root-loc '()])
  (reap [sow]
    (for ([rule *rules*])
      (let* ([applyer (if destruct? rule-apply-force-destructs rule-apply)]
             [result (applyer rule expr)])
        (when result
            (sow (change rule root-loc (cdr result))))))))

(define (rewrite-tree expr #:root [root-loc '()])
  (reap [sow]
    (let ([try-rewrites
           (λ (expr loc)
              (map sow (rewrite-expression expr #:root (append root-loc loc)))
              expr)])
      (location-induct expr
        #:constant try-rewrites #:variable try-rewrites #:primitive try-rewrites))))

(define (change-apply cng prog)
  (let ([loc (change-location cng)]
        [template (rule-output (change-rule cng))]
        [bnd (change-bindings cng)])
    (location-do loc prog (λ (expr) (pattern-substitute template bnd)))))

; Now we define some rules

; Commutativity
(define-rule   +-commutative     (+ a b)               (+ b a))
(define-rule   *-commutative     (* a b)               (* b a))
; Associativity
(define-rule   associate-+-lft   (+ a (+ b c))         (+ (+ a b) c))
(define-rule   associate-+-rgt   (+ (+ a b) c)         (+ a (+ b c)))
(define-rule   associate---lft   (+ a (- b c))         (- (+ a b) c))
(define-rule   associate---rgt   (- (+ a b) c)         (+ a (- b c)))
(define-rule   associate-*-lft   (* a (* b c))         (* (* a b) c))
(define-rule   associate-*-rgt   (* (* a b) c)         (* a (* b c)))
(define-rule   associate-/-lft   (* a (/ b c))         (/ (* a b) c))
(define-rule   associate-/-rgt   (/ (* a b) c)         (* a (/ b c)))
; Distributivity
(define-rule   distribute-lft-in     (* a (+ b c))         (+ (* a b) (* a c)))
(define-rule   distribute-rgt-in     (* a (+ b c))         (+ (* b a) (* c a)))
(define-rule   distribute-lft-out    (+ (* a b) (* a c))   (* a (+ b c)))
(define-rule   distribute-rgt-out    (+ (* b a) (* c a))   (* a (+ b c)))
; Identity
(define-rule   +-identity        (+ a 0)               a)
(define-rule   +-inverses        (- a a)               0)
(define-rule   *-identity        (* a 1)               a)
(define-rule   *-inverses        (/ a a)               1)
; Dealing with fractions
(define-rule   div-sub           (/ (- a b) c)         (- (/ a c) (/ b c)))
(define-rule   sub-div           (- (/ a c) (/ b c))   (/ (- a b) c))
(define-rule   frac-sub          (- (/ a b) (/ c d))   (/ (- (* a d) (* b c)) (* b d)))
; Square root  
(define-rule   add-square-sqrt   x                     (square (sqrt x)))
(define-rule   add-sqrt-square   x                     (sqrt (square x)))
(define-rule   rem-square-sqrt   (square (sqrt x))     x)
(define-rule   rem-sqrt-square   (sqrt (square x))     x)
(define-rule   square-mult       (square x)            (* x x))
(define-rule   square-unmult     (* x x)               (square x))
(define-rule   square-prod       (square (* x y))      (* (square x) (square y)))
(define-rule   square-unprod     (* (square x) (square y)) (square (* x y)))
(define-rule   square-div        (square (/ x y))      (/ (square x) (square y)))
(define-rule   square-undiv      (/ (square x) (square y)) (square (/ x y)))
; Exponentials
(define-rule   add-exp-log       x                     (exp (log x)))
(define-rule   add-log-exp       x                     (log (exp x)))
(define-rule   rem-exp-log       (exp (log x))         x)
(define-rule   rem-log-exp       (log (exp x))         x)
; Multiplying by x / x
(define-rule   flip-+            (+ a b)               (/ (- (square a) (square b)) (- a b)))
(define-rule   flip--            (- a b)               (/ (- (square a) (square b)) (+ a b)))
