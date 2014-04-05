#lang racket
(require casio/common)
(require casio/programs)

(provide *rules* pattern-substitute pattern-match rewrite-expression rewrite-tree change-apply (struct-out change) (struct-out rule))

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

(struct rule (name input output slocations)
        #:methods gen:custom-write
        [(define (write-proc rule port mode)
           (display "#<rule " port)
           (write (rule-name rule) port)
           (display ">" port))])

(define *rules* '())

(define-syntax (define-rule stx)
  (syntax-case stx ()
    [(_ name input output #:simplify slocations)
     #'(set! *rules* (cons (rule 'name 'input 'output 'slocations) *rules*))]
    [(_ name input output)
     #'(set! *rules* (cons (rule 'name 'input 'output '()) *rules*))]))

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
(define-rule   associate-+-lft   (+ a (+ b c))         (+ (+ a b) c)     #:simplify ((cdr car)))
(define-rule   associate-+-rgt   (+ (+ a b) c)         (+ a (+ b c))     #:simplify ((cdr cdr car)))
(define-rule   associate---lft   (+ a (- b c))         (- (+ a b) c)     #:simplify ((cdr car)))
(define-rule   associate---rgt   (- (+ a b) c)         (+ a (- b c))     #:simplify ((cdr cdr car)))
(define-rule   associate-*-lft   (* a (* b c))         (* (* a b) c)     #:simplify ((cdr car)))
(define-rule   associate-*-rgt   (* (* a b) c)         (* a (* b c))     #:simplify ((cdr cdr car)))
(define-rule   associate-/-lft   (* a (/ b c))         (/ (* a b) c)     #:simplify ((cdr car)))
(define-rule   associate-/-rgt   (/ (* a b) c)         (* a (/ b c))     #:simplify ((cdr cdr car)))

; Distributivity
(define-rule   distribute-lft-in     (* a (+ b c))         (+ (* a b) (* a c))     #:simplify ((cdr car) (cdr cdr car)))
(define-rule   distribute-rgt-in     (* a (+ b c))         (+ (* b a) (* c a))     #:simplify ((cdr car) (cdr cdr car)))
(define-rule   distribute-lft-out    (+ (* a b) (* a c))   (* a (+ b c))           #:simplify ((cdr cdr car)))
(define-rule   distribute-rgt-out    (+ (* b a) (* c a))   (* a (+ b c))           #:simplify ((cdr cdr car)))

; Identity
(define-rule   +-lft-identity    (+ 0 a)               a)
(define-rule   +-rgt-identity    (+ a 0)               a)
(define-rule   +-inverses        (- a a)               0)
(define-rule   sub-neg           (- a b)               (+ a (- b)))
(define-rule   *-lft-identity    (* 1 a)               a)
(define-rule   *-rgt-identity    (* a 1)               a)
(define-rule   *-inverses        (/ a a)               1)

; Dealing with fractions
(define-rule   div-sub     (/ (- a b) c)        (- (/ a c) (/ b c))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   sub-div     (- (/ a c) (/ b c))  (/ (- a b) c)
  #:simplify ((cdr car)))
(define-rule   frac-sub    (- (/ a b) (/ c d))  (/ (- (* a d) (* b c)) (* b d))
  #:simplify ((cdr car)))
(define-rule   frac-times  (* (/ a b) (/ c d))  (/ (* a c) (* b d))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   times-frac  (/ (* a b) (* c d))  (* (/ a c) (/ b d))
  #:simplify ((cdr car) (cdr cdr car)))

; Square root
(define-rule   add-sqr-sqrt      x                  (sqr (sqrt x)))
(define-rule   add-sqrt-sqr      x                  (sqrt (sqr x)))
(define-rule   rem-square-sqrt   (sqr (sqrt x))     x)
(define-rule   rem-sqrt-square   (sqrt (sqr x))     x)
(define-rule   square-mult       (sqr x)            (* x x))
(define-rule   square-unmult     (* x x)            (sqr x))
(define-rule   square-prod       (sqr (* x y))      (* (sqr x) (sqr y))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   square-unprod     (* (sqr x) (sqr y)) (sqr (* x y))
  #:simplify ((cdr car)))
(define-rule   square-div        (sqr (/ x y))      (/ (sqr x) (sqr y))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   square-undiv      (/ (sqr x) (sqr y)) (sqr (/ x y))
  #:simplify ((cdr car)))

; Exponentials
(define-rule   add-exp-log  x                    (exp (log x)))
(define-rule   add-log-exp  x                    (log (exp x)))
(define-rule   rem-exp-log  (exp (log x))        x)
(define-rule   rem-log-exp  (log (exp x))        x)
(define-rule   exp-sum      (exp (+ a b))        (* (exp a) (exp b)))
(define-rule   prod-exp     (* (exp a) (exp b))  (exp (+ a b)))
(define-rule   exp-diff     (exp (- a b))        (/ (exp a) (exp b)))
(define-rule   div-exp      (/ (exp a) (exp b))  (exp (- a b)))

; Multiplying by x / x
(define-rule   flip-+     (+ a b)  (/ (- (sqr a) (sqr b)) (- a b))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   flip--     (- a b)  (/ (- (sqr a) (sqr b)) (+ a b))
  #:simplify ((cdr car) (cdr cdr car)))
(define-rule   clear-num  (/ a b)  (/ 1 (/ b a))
  #:simplify ((cdr cdr car)))
