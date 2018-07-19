#lang racket

(require "../common.rkt")
(require "../programs.rkt")
(require "../syntax/rules.rkt")
(require "../type-check.rkt")

(provide
 (all-from-out "../syntax/rules.rkt")
 pattern-substitute pattern-match
 rewrite-expression-head rewrite-expression
 (struct-out change) change-apply changes-apply rule-rewrite)

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
   [(constant? pattern)
    (if (and (constant? expr) (equal? pattern expr))
        '()
        (fail "pattern-match: Literals do not match"
              pattern expr))]
   [(variable? pattern)
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
   [(constant? pattern) pattern]
   [(variable? pattern)
    (cdr (assoc pattern bindings))]
   [(list? pattern)
    (cons (car pattern)
          (for/list ([pat (cdr pattern)])
            (pattern-substitute pat bindings)))]
   [#t (error "pattern-substitute: Confused by pattern term" pattern)]))

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

(define (rule-rewrite rule prog [loc '()])
  (let/ec return
    (location-do loc prog
                 (λ (x) (match (rule-apply rule x)
                          [(cons out bindings) out]
                          [#f (return #f)])))))

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
  (define env (for/hash ([v (free-variables expr)]) (values v 'real)))
  (reap [sow]
    (for ([rule (*rules*)])
      (let* ([applyer (if destruct? rule-apply-force-destructs rule-apply)]
             [result (applyer rule expr)])
        (when result
            (sow (change rule root-loc (cdr result))))))))

(define (rewrite-expression-head expr #:root [root-loc '()] #:depth [depth 1])

  (define env (for/hash ([v (free-variables expr)]) (values v 'real)))
  (define (rewriter expr ghead glen loc cdepth)
    ; expr _ _ _ _ -> (list (list change))
    (reap (sow)
          (for ([rule (*rules*)])
            (when (or
                    (not ghead) ; Any results work for me
                    (and
                      (list? (rule-output rule))
                      (= (length (rule-output rule)) glen)
                      (eq? (car (rule-output rule)) ghead)))
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
       [(variable? pattern)
        ; Do nothing, bind variable
        (list (cons '() (list (cons pattern expr))))]
       [(constant? pattern)
        (if (and (constant? expr) (equal? expr pattern))
            '((()) . ()) ; Do nothing, bind nothing
            '())] ; No options
       [(and (list? expr) (list? pattern))
        (if (and (eq? (car pattern) (car expr))
                 (= (length pattern) (length expr)))
            ; Everything is terrible
            (reduce-children
              (apply cartesian-product ; (list (list ((list cng) * bnd)))
                     (for/list ([i (in-naturals)] [sube expr] [subp pattern]
                                #:when (> i 0)) ; (list (list ((list cng) * bnd)))
                       ;; Note: we reset the fuel to "depth", not "cdepth"
                       (matcher sube subp (cons i loc) depth)))) ; list (expr * pattern)
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

(define (change-apply cng prog)
  (let ([loc (change-location cng)]
        [template (rule-output (change-rule cng))]
        [bnd (change-bindings cng)])
    (location-do loc prog (λ (expr) (pattern-substitute template bnd)))))

(define (changes-apply chngs prog)
  (for/fold ([prog prog]) ([chng chngs])
    (change-apply chng prog)))
