#lang racket

(require "../common.rkt" "../programs.rkt" "matcher.rkt"
         "../syntax/rules.rkt" "../syntax/syntax.rkt")

(provide simplify load-rule-hacks)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define fn-inverses '())
(define fn-evaluations (make-hash))
(define simplify-cache (make-hash))
(define simplify-node-cache (make-hash))

(register-reset
 (λ ()
   (set! fn-inverses '())
   (set! fn-evaluations (make-hash))
   (set! simplify-cache (make-hash))
   (set! simplify-node-cache (make-hash))))

(define (load-rule-hacks)
  (set! fn-inverses
        (remove-duplicates
         (map
          (λ (r) (rule-input r))
          (filter (λ (r) (variable? (rule-output r))) (*rules*)))))
  (set! fn-evaluations
        (for/hash ([r (*rules*)] #:when (null? (free-variables (rule-input r))))
          (values (rule-input r) (rule-output r)))))

(define (simplify expr)
  (hash-ref! simplify-cache expr (λ () (simplify* expr))))

(define (simplify* expr)
  (match expr
    [(? number?) expr]
    [(? symbol?) expr]
    [`(λ ,vars ,body)
     `(λ ,vars ,(simplify body))]
    [`(lambda ,vars ,body)
     `(λ ,vars ,(simplify body))]
    [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
     (list op (simplify body))]
    [`(,(and (or '+ '- '*) op) ,args ...) ; v-ary
     (define args* (map simplify args))
     (define val (apply eval-application op args*))
     (or val (simplify-node (list* op args*)))]
    [`(,op ,args ...)
     (define args* (map simplify args))
     (define val (apply eval-application op args*))
     (or val (simplify-node (list* op args*)))]))

(define (simplify-node expr)
  (hash-ref! simplify-node-cache expr (λ () (simplify-node* expr))))

(define (simplify-node* expr)
  (match expr
    [(? (curry hash-has-key? fn-evaluations)) (hash-ref fn-evaluations expr)]
    [(? number?) expr]
    [(? variable?) expr]
    [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
     (make-addition-node (combine-aterms (gather-additive-terms expr)))]
    [(or `(* ,_ ...) `(/ ,_ ...) `(sqrt ,_) `(cbrt ,_))
     (make-multiplication-node (combine-mterms (gather-multiplicative-terms expr)))]
    [`(exp (* ,c (log ,x)))
     `(pow ,x ,c)]
    [else
     (let/ec return
       (for ([pattern fn-inverses])
         (match (pattern-match pattern expr)
           [#f (void)]
           [`((,var . ,body)) (return body)]
           [else (error "Function inverse pattern match returned multiple bindings")]))
       expr)]))

(define (negate-term term)
  (cons (- (car term)) (cdr term)))

(define (gather-additive-terms expr #:label [label #f])
  (define (recurse subexpr #:label [label #f])
    (gather-additive-terms subexpr #:label label))

  (let ([label (or label expr)])
    (match expr
      [(? number?) `((,expr 1))]
      [(? variable?) `((1 ,expr))]
      [`(+ ,args ...) (append-map recurse args)]
      [`(neg ,arg) (map negate-term (recurse arg))]
      [`(- ,arg ,args ...)
       (append (recurse arg)
               (map negate-term (append-map recurse args)))]

      [`(/ ,arg) ; Prevent fall-through to the next case
       `((1 ,expr))]
      [`(/ ,arg ,args ...)
       (for/list ([term (recurse arg)])
         (list* (car term) (simplify-node (list* '/ (cadr term) args)) (cons label (cddr term))))]

      [`(pow ,arg 1)
       `((1 1))]
      [else
       `((1 ,expr))])))

(define (gather-multiplicative-terms expr)
  (match expr
    [(? number?) `(,expr)]
    [(? symbol?) `(1 (1 . ,expr))]
    [`(neg ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (- (car terms)) (cdr terms)))]
    [`(* ,args ...)
     (let ([terms (map gather-multiplicative-terms args)])
       (cons (apply * (map car terms)) (apply append (map cdr terms))))]
    [`(/ ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (if (= (car terms) 0) 'NAN (/ (car terms))) (map negate-term (cdr terms))))]
    [`(/ ,arg ,args ...)
     (let ([num (gather-multiplicative-terms arg)]
           [dens (map gather-multiplicative-terms args)])
       (cons (if (ormap (compose (curry = 0) car) dens) 'NAN (apply / (car num) (map car dens)))
             (append (cdr num)
                     (map negate-term (append-map cdr dens)))))]
    ;; Commented out due to unsoundness issues
    #;[`(sqrt ,arg)
       (let ([terms (gather-multiplicative-terms arg)])
         (define exact-sqrt (eval-application 'sqrt (car terms)))
         (if exact-sqrt
             (cons exact-sqrt
                   (for/list ([term (cdr terms)])
                     (cons (/ (car term) 2) (cdr term))))
             (list* 1
                    (cons 1 `(sqrt ,(car terms)))
                    (for/list ([term (cdr terms)])
                      (cons (/ (car term) 2) (cdr term))))))]
    [`(cbrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (define exact-cbrt (eval-application 'cbrt (car terms)))
       (if exact-cbrt
           (cons exact-cbrt
                 (for/list ([term (cdr terms)])
                   (cons (/ (car term) 3) (cdr term))))
           (list* 1
                  (cons 1 `(cbrt ,(car terms)))
                  (for/list ([term (cdr terms)])
                    (cons (/ (car term) 3) (cdr term))))))]
    [`(pow ,arg 0)
     '(1)]
    ;; Commented out due to unsoundness issues
    #;[`(pow ,arg ,(? real? a))
       (let ([terms (gather-multiplicative-terms arg)])
         (define exact-pow (eval-application 'pow (car terms) a))
         (if exact-pow
             (cons exact-pow
                   (for/list ([term (cdr terms)])
                     (cons (* a (car term)) (cdr term))))
             (list* 1
                    (cons a (car terms))
                    (for/list ([term (cdr terms)])
                      (cons (* a (car term)) (cdr term))))))]
    [else
     `(1 (1 . ,expr))]))

(define (combine-aterms terms)
  (let ([h (make-hash)])
    (for ([term terms])
      (let ([sum (hash-ref! h (cadr term) (λ () 0))])
        (hash-set! h (cadr term) (+ (car term) sum))))
    (reap [sow]
          (hash-for-each h (λ (k v) (when (not (= v 0)) (sow (cons v k))))))))

(define (combine-mterms terms)
  (cons
   (car terms)
   (let ([h (make-hash)])
     (for ([term (cdr terms)])
       (let ([sum (hash-ref! h (cdr term) (λ () 0))])
         (hash-set! h (cdr term) (+ (car term) sum))))
     (reap [sow]
           (hash-for-each h (λ (k v) (when (not (= v 0)) (sow (cons v k)))))))))

(define (aterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(,x . 1) x]
    [`(-1 . ,x) `(neg ,x)]
    [`(,coeff . ,x) `(* ,coeff ,x)]))

(define (make-addition-node terms)
  (let-values ([(pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms)])
    (cond
      [(and (null? pos) (null? neg))
       0]
      [(null? pos)
       `(neg ,(make-addition-node* (map negate-term neg)))]
      [(null? neg)
       (make-addition-node* pos)]
      [else
       `(- ,(make-addition-node* pos)
           ,(make-addition-node* (map negate-term neg)))])))

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node* terms)
  (match terms
    ['() 0]
    [`(,term)
     (aterm->expr term)]
    [`(,term ,terms ...)
     `(+ ,(aterm->expr term) ,(make-addition-node terms))]))

(define (make-multiplication-node term)
  (match (cons (car term) (make-multiplication-subnode (cdr term)))
    [`(0 . ,e) 0]
    [`(1 . ()) 1]
    [`(1 . ,e) e]
    [`(,a . 1) a]
    [`(,a . (/ 1 ,denom)) `(/ ,a ,denom)]
    [`(,a . ()) a]
    [`(,a . ,e) `(* ,a ,e)]))

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (for/list ([rootgroup (group-by (compose denominator car) terms)])
     (let* ([denom (denominator (caar rootgroup))]
            [newterms (map (λ (term) (cons (* (car term) denom) (cdr term))) rootgroup)])
       (cons 1
             (mterm->expr
              (cons (/ 1 denom)
                    (make-multiplication-subsubnode newterms))))))))

(define (make-multiplication-subsubnode terms)
  (let-values ([(pos neg) (partition (compose positive? car) terms)])
    (cond
      [(and (null? pos) (null? neg)) 1]
      [(null? pos)
       `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg)))]
      [(null? neg)
       (make-multiplication-subsubsubnode pos)]
      [else
       `(/ ,(make-multiplication-subsubsubnode pos)
           ,(make-multiplication-subsubsubnode (map negate-term neg)))])))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() 1]
    [`(,term)
     (mterm->expr term)]
    [`(,term ,terms ...)
     `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms))]))

(define (mterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(-1 . ,x) `(/ 1 ,x)]
    [`(1/2 . ,x) `(sqrt ,x)]
    [`(-1/2 . ,x) `(/ 1 (sqrt ,x))]
    [`(,power . ,x) `(pow ,x ,power)]))

