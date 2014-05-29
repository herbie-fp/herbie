#lang racket
(require casio/common)
(require casio/programs)

(provide *rules* pattern-substitute pattern-match rewrite-expression rewrite-expression-head rewrite-tree change-apply (struct-out change) (struct-out rule) change-add-hardness change*-hardness changes-apply (struct-out change*))

;; Our own pattern matcher.
;
; The racket (match) macro doesn't give us access to the bindings made
; by the matcher, so we wrote our own.
;
; The syntax is simple:
;   numbers are literals ; symbols are variables ; lists are expressions
;
; Bindings are stored as association lists

(define (merge-bindings . bindings)
  ; (list bindings) -> binding
  (foldl merge-2-bindings '() bindings))

(define (merge-2-bindings binding1 binding2)
  (define (fail . irr) #f)

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

; The matcher itself

(define (pattern-match pattern expr)
  ; pattern expr -> bindings

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
        (apply merge-bindings
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

(struct change* change (hardness))

(define (change-add-hardness chng hardness)
  (change* (change-rule chng) (change-location chng)
	   (change-bindings chng) hardness))

(define (rewrite-expression expr #:destruct [destruct? #f] #:root [root-loc '()])
  (reap [sow]
    (for ([rule *rules*])
      (let* ([applyer (if destruct? rule-apply-force-destructs rule-apply)]
             [result (applyer rule expr)])
        (when result
            (sow (change rule root-loc (cdr result))))))))

(define (rewrite-expression-head expr #:root [root-loc '()] #:depth [depth 1])

  (define (rewriter expr ghead glen loc cdepth)
    ; expr _ _ _ _ -> (list (list change))
    (reap (sow)
          (for ([rule *rules*])
            (when (and (list? (rule-output rule))
                       (or
                        (not ghead) ; Any results work for me
                        (and
                         (= (length (rule-output rule)) glen)
                         (eq? (car (rule-output rule)) ghead))))
              (let ([options (matcher expr (rule-input rule) loc (- cdepth 1))])
                (for ([option options])
                  ; Each option is a list of change lists
                  (sow (cons (change rule (reverse loc) (cdr option))
                             (car option)))))))))

  (define (reduce-children options)
    ; (list (list ((list change) * bindings)))
    ; -> (list ((list change) * bindings))
    (reap (sow)
      (for ([children options])
        (let ([bindings* (apply merge-bindings (map cdr children))])
          (when bindings*
            (sow (cons (apply append (map car children)) bindings*)))))))

  (define (fix-up-variables pattern options)
    ; pattern (list (list change)) -> (list (list change) * pattern)
    (reap (sow)
      (for ([cngs options])
        (let* ([out-pattern (rule-output (change-rule (car cngs)))]
               [result (pattern-substitute out-pattern
                                           (change-bindings (car cngs)))]
               [bindings* (pattern-match pattern result)])
          (when bindings*
            (sow (cons cngs bindings*)))))))

  (define (matcher expr pattern loc cdepth)
    ; expr pattern _ -> (list ((list change) * bindings))
      (cond
       [(symbol? pattern)
        ; Do nothing, bind variable
        (list (cons '() (list (cons pattern expr))))]
       [(number? pattern)
        (if (and (number? expr) (= expr pattern))
            '((()) . ()) ; Do nothing, bind nothing
            '())] ; No options
       [(and (list? expr) (list? pattern))
        (if (and (eq? (car pattern) (car expr))
                 (= (length pattern) (length expr)))
            ; Everything is terrible
            (reduce-children
              (apply list-product ; (list (list ((list cng) * bnd)))
                (idx-map ; (list (list ((list cng) * bnd)))
                 ; Note: we reset the fuel to "depth", not "cdepth"
                 (λ (x i) (matcher (car x) (cdr x) (cons i loc) depth)) ; (expr * pattern) nat -> (list (list cng))
                 (map cons (cdr expr) (cdr pattern)) ; list (expr * pattern)
                 #:from 1)))
            (if (> cdepth 0)
                ; Sort of a brute force approach to getting the bindings
                (fix-up-variables
                 pattern
                 (rewriter expr (car pattern) (length pattern) loc (- cdepth 1)))
                '()))]
       [(and (list? pattern) (not (list? expr)))
        '()]
       [else
        (error "Unknown pattern" pattern)]))

  ; The #f #f mean that any output result works. It's a bit of a hack
  (rewriter expr #f #f (reverse root-loc) depth))

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

(define (changes-apply chngs prog)
  (pipe prog (map (λ (chng) (curry change-apply chng))
		  chngs)))

; Now we define some rules

; Commutativity
(define-rule   +-commutative     (+ a b)               (+ b a))
(define-rule   *-commutative     (* a b)               (* b a))

; Associativity
(define-rule   associate-+-lft   (+ a (+ b c))         (+ (+ a b) c)     #:simplify ((2)))
(define-rule   associate-+-rgt   (+ (+ a b) c)         (+ a (+ b c))     #:simplify ((2)))
(define-rule   associate---lft   (+ a (- b c))         (- (+ a b) c)     #:simplify ((2)))
(define-rule   associate---rgt   (- (+ a b) c)         (+ a (- b c))     #:simplify ((2)))
(define-rule   associate-*-lft   (* a (* b c))         (* (* a b) c)     #:simplify ((2)))
(define-rule   associate-*-rgt   (* (* a b) c)         (* a (* b c))     #:simplify ((2)))
(define-rule   associate-/-lft   (* a (/ b c))         (/ (* a b) c)     #:simplify ((2)))
(define-rule   associate-/-rgt   (/ (* a b) c)         (* a (/ b c))     #:simplify ((2)))

; Distributivity
(define-rule   distribute-lft-in     (* a (+ b c))         (+ (* a b) (* a c))     #:simplify ((1) (2)))
(define-rule   distribute-rgt-in     (* a (+ b c))         (+ (* b a) (* c a))     #:simplify ((1) (2)))
(define-rule   distribute-lft-out    (+ (* a b) (* a c))   (* a (+ b c))           #:simplify ((2)))
(define-rule   distribute-rgt-out    (+ (* b a) (* c a))   (* a (+ b c))           #:simplify ((2)))
(define-rule   distribute-lft1-in    (+ (* b a) a)         (* (+ b 1) a)           #:simplify ((1)))
(define-rule   distribute-rgt1-in    (+ a (* c a))         (* (+ c 1) a)           #:simplify ((1)))
(define-rule   distribute-lft-neg-in (- (* a b))           (* (- a) b))
(define-rule   distribute-rgt-neg-in (- (* a b))           (* a (- b)))
(define-rule   distribute-lft-neg-out (* (- a) b)          (- (* a b)))
(define-rule   distribute-rgt-neg-out (* a (- b))          (- (* a b)))

; Difference of squares
(define-rule   difference-of-squares (- (sqr a) (sqr b))   (* (+ a b) (- a b))     #:simplify ((1) (2)))
(define-rule   difference-of-sqr-1   (- (sqr a) 1)         (* (+ a 1) (- a 1))     #:simplify ((1) (2)))

; Identity
(define-rule   +-lft-identity    (+ 0 a)               a)
(define-rule   +-rgt-identity    (+ a 0)               a)
(define-rule   +-inverses        (- a a)               0)
(define-rule   sub-neg           (- a b)               (+ a (- b)))
(define-rule   unsub-neg         (+ a (- b))           (- a b))
(define-rule   remove-double-neg (- (- a))             a)
(define-rule   *-lft-identity    (* 1 a)               a)
(define-rule   *-rgt-identity    (* a 1)               a)
(define-rule   *-inverses        (/ a a)               1)
(define-rule   div-inv           (/ a b)               (* a (/ b)))
(define-rule   un-div-inv        (* a (/ b))           (/ a b))
(define-rule   remove-double-div (/ (/ a))             a)
(define-rule   div1              (/ 1)                 1)

; Dealing with fractions
(define-rule   div-sub     (/ (- a b) c)        (- (/ a c) (/ b c))
  #:simplify ((1) (2)))
(define-rule   sub-div     (- (/ a c) (/ b c))  (/ (- a b) c)
  #:simplify ((1)))
(define-rule   frac-add    (+ (/ a b) (/ c d))  (/ (+ (* a d) (* b c)) (* b d))
  #:simplify ((1)))
(define-rule   frac-sub    (- (/ a b) (/ c d))  (/ (- (* a d) (* b c)) (* b d))
  #:simplify ((1)))
(define-rule   frac-times  (* (/ a b) (/ c d))  (/ (* a c) (* b d))
  #:simplify ((1) (2)))
(define-rule   times-frac  (/ (* a b) (* c d))  (* (/ a c) (/ b d))
  #:simplify ((1) (2)))

; Square root
(define-rule   add-sqr-sqrt      x                  (sqr (sqrt x)))
(define-rule   rem-square-sqrt   (sqr (sqrt x))     x)
(define-rule   rem-sqrt-square   (sqrt (sqr x))     x)
(define-rule   square-mult       (sqr x)            (* x x))
(define-rule   square-unmult     (* x x)            (sqr x))
(define-rule   square-prod       (sqr (* x y))      (* (sqr x) (sqr y))
  #:simplify ((1) (2)))
(define-rule   square-unprod     (* (sqr x) (sqr y)) (sqr (* x y))
  #:simplify ((1)))
(define-rule   square-div        (sqr (/ x y))      (/ (sqr x) (sqr y))
  #:simplify ((1) (2)))
(define-rule   square-undiv      (/ (sqr x) (sqr y)) (sqr (/ x y))
  #:simplify ((1)))
(define-rule   sqrt-prod         (sqrt (* x y))     (* (sqrt x) (sqrt y))
  #:simplify ((1) (2)))
(define-rule   sqrt-unprod       (* (sqrt x) (sqrt y)) (sqrt (* x y))
  #:simplify ((1)))
(define-rule   sqrt-div          (sqrt (/ x y))     (/ (sqrt x) (sqrt y))
  #:simplify ((1) (2)))
(define-rule   sqrt-undiv        (/ (sqrt x) (sqrt y)) (sqrt (/ x y))
  #:simplify ((1)))

; Exponentials
(define-rule   add-exp-log  x                    (exp (log x)))
(define-rule   add-log-exp  x                    (log (exp x)))
(define-rule   rem-exp-log  (exp (log x))        x)
(define-rule   rem-log-exp  (log (exp x))        x)
(define-rule   exp-sum      (exp (+ a b))        (* (exp a) (exp b)))
(define-rule   prod-exp     (* (exp a) (exp b))  (exp (+ a b))
  #:simplify ((1)))
(define-rule   exp-neg      (exp (- a))          (/ (exp a)))
(define-rule   rec-exp      (/ (exp a))          (exp (- a)))
(define-rule   exp-diff     (exp (- a b))        (/ (exp a) (exp b)))
(define-rule   div-exp      (/ (exp a) (exp b))  (exp (- a b))
  #:simplify ((1)))
(define-rule   exp-prod     (exp (* a b))        (expt (exp a) b))
(define-rule   expt-exp     (expt (exp a) b)     (exp (* a b))
  #:simplify ((1)))
(define-rule   expt-to-exp  (expt a b)           (exp (* (log a) b)))
(define-rule   exp-to-expt  (exp (* (log a) b))  (expt a b))

; Logarithms
(define-rule   sum-log      (+ (log a) (log b))  (log (* a b))
  #:simplify ((1)))
(define-rule   log-prod     (log (* a b))        (+ (log a) (log b)))
(define-rule   diff-log     (- (log a) (log b))  (log (/ a b))
  #:simplify ((1)))
(define-rule   log-div      (log (/ a b))        (- (log a) (log b)))
(define-rule   neg-log      (- (log a))          (log (/ a))
  #:simplify ((1)))
(define-rule   log-rec      (log (/ a))          (- (log a))
  #:simplify ((1)))

; Multiplying by x / x
(define-rule   flip-+     (+ a b)  (/ (- (sqr a) (sqr b)) (- a b))
  #:simplify ((1) (2)))
(define-rule   flip--     (- a b)  (/ (- (sqr a) (sqr b)) (+ a b))
  #:simplify ((1) (2)))
(define-rule   clear-num  (/ a b)  (/ 1 (/ b a))
  #:simplify ((2)))

; Trigonometry
(define-rule   cos-sin-sum (+ (sqr (cos a)) (sqr (sin a))) 1)
(define-rule   1-sub-cos   (- 1 (sqr (cos a))) (sqr (sin a)) #:simplify ((1)))
(define-rule   1-sin-sin   (- 1 (sqr (sin a))) (sqr (cos a)) #:simplify ((1)))
