#lang racket

(require rackunit)
(require casio/rules)
(require casio/matcher)
(require casio/common)
(require casio/syntax)
(require casio/programs)

(provide simplify)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define fn-inverses
  (map rule-input (filter (λ (rule) (symbol? (rule-output rule))) *rules*)))

(define (simplify expr)
  (let ([simpl (simplify* expr)])
    (debug #:from 'simplify "Simplify" expr "into" simpl)
    simpl))

(define (simplify* expr)
  (match expr
    [(? constant?) expr]
    [(? variable?) expr]
    [`(λ ,vars ,body)
     `(λ ,vars ,(simplify* body))]
    [`(lambda ,vars ,body)
     `(λ ,vars ,(simplify* body))]
    [(? (compose null? free-variables) `(,op ,args ...))
     (let ([value
            (with-handlers ([(const #t) (const #f)])
              (casio-eval expr))])
       (if (and (number? value) (real? value) (exact? value))
           value
           (simplify-node `(,op ,@(map simplify* args)))))]
    [`(,op ,args ...)
     (simplify-node `(,op ,@(map simplify* args)))]))

(define (simplify-node expr)
  (match expr
    [(? constant?) expr]
    [(? variable?) expr]
    [(or `(+ ,_ ...) `(- ,_ ...))
     (let* ([labels (combining-labels (gather-additive-terms expr))]
            [terms (combine-aterms (gather-additive-terms expr #:expand labels))])
       (make-addition-node terms))]
    [(or `(* ,_ ...) `(/ ,_ ...) `(sqr ,_) `(sqrt ,_))
     (let ([terms (combine-mterms (gather-multiplicative-terms expr))])
       (make-multiplication-node terms))]
    [`(exp (* ,c (log ,x)))
     `(expt ,x ,c)]
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

(define (gather-additive-terms expr #:expand [expand #f] #:label [label #f] )
  (define (recurse subexpr #:label [label #f])
    (gather-additive-terms subexpr #:expand expand #:label label))

  (let ([label (or label expr)])
    (match expr
      [(? number?) `((,expr 1))]
      [(? constant?) `((1 ,expr))]
      [(? variable?) `((1 ,expr))]
      [`(+ ,args ...) (append-map recurse args)]
      [`(- ,arg) (map negate-term (recurse arg))]
      [`(- ,arg ,args ...)
       (append (recurse arg)
               (map negate-term (append-map recurse args)))]

      [`(* ,args ...)
       (if (or (not expand) (memq label expand))
           (for/list ([term-list (apply list-product (map recurse args))])
             (list* (apply * (map car term-list))
                    (simplify-node (cons '* (map cadr term-list)))
                    (cons label (append-map cddr term-list))))
           `((1 ,label)))]
      [`(/ ,arg) ; Prevent fall-through to the next case
       `((1 ,expr))]
      [`(/ ,arg ,args ...)
       (if (or (not expand) (memq label expand))
           (let ([nums (recurse arg)])
             (for/list ([term nums])
               (list* (car term) (simplify-node (list* '/ (cadr term) args)) (cons label (cddr term)))))
           `((1 ,expr)))]

      [`(sqr ,arg)
       (recurse `(* ,arg ,arg) #:label expr)]
      [`(expt ,arg ,(? integer? n))
       (cond
        [(positive? n)
         (recurse (cons '* (build-list n (const arg))) #:label expr)]
        [(negative? n)
         `((1 ,expr))]
        [(zero? n)
         `((1 1))])]
      [else
       `((1 ,expr))])))

(define (gather-multiplicative-terms expr)
  (match expr
    [(? number?) `(,expr)]
    [(? symbol?) `(1 (1 . ,expr))]
    [`(- ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (- (car terms)) (cdr terms)))]
    [`(* ,args ...)
     (let ([terms (map gather-multiplicative-terms args)])
       (cons (apply * (map car terms)) (apply append (map cdr terms))))]
    [`(/ ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (if (= (car terms) 0) +nan.0 (/ (car terms))) (map negate-term (cdr terms))))]
    [`(/ ,arg ,args ...)
     (let ([num (gather-multiplicative-terms arg)]
           [dens (map gather-multiplicative-terms args)])
       (cons (if (ormap (compose (curry = 0) car) dens) +nan.0 (apply / (car num) (map car dens)))
             (append (cdr num)
                     (map negate-term (append-map cdr dens)))))]
    [`(sqr ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (sqr (car terms))
             (for/list ([term (cdr terms)])
               (cons (* 2 (car term)) (cdr term)))))]
    [`(sqrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (if (negative? (car terms))
           `(1 (1 . ,expr))
           (cons (sqrt (car terms))
                 (for/list ([term (cdr terms)])
                   (cons (/ (car term) 2) (cdr term))))))]
    [`(expt ,arg ,(? real? a))
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (expt (car terms) a)
             (for/list ([term (cdr terms)])
               (cons (* a (car term)) (cdr term)))))]
    [else
     `(1 (1 . ,expr))]))

(define (combining-labels terms)
  (remove-duplicates
   (reap [sow]
         (let ([h (make-hash)])
           (for ([term terms])
             (cond
              [(hash-has-key? h (cadr term))
               (map sow (cddr term))
               (map sow (hash-ref h (cadr term)))
               (hash-set! h (cadr term) '())]
              [else
               (hash-set! h (cadr term) (cddr term))]))))
   eq?))

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
    [`(-1 . ,x) `(- ,x)]
    [`(,coeff . ,x) `(* ,coeff ,x)]))

(define (make-addition-node terms)
  (let-values ([(pos neg) (partition (compose positive? car) terms)])
    (cond
     [(and (null? pos) (null? neg))
      0]
     [(null? pos)
      `(- ,(make-addition-node* (map negate-term neg)))]
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

(define (mterm->expr term)
  (match term
    [`(1 . ,x) x]
    [`(-1 . ,x) `(/ 1 ,x)]
    [`(2 . ,x) `(sqr ,x)]
    [`(-2 . ,x) `(/ 1 (sqr ,x))]
    [`(1/2 . ,x) `(sqrt ,x)]
    [`(-1/2 . ,x) `(/ 1 (sqrt ,x))]
    [`(,pow . ,x) `(expt ,x ,pow)]))

(define (make-multiplication-node term)
  (match term
    [`(0 ,terms ...) 0]
    [`(1 ,terms ...)
     (let-values ([(pos neg) (partition (compose positive? car) terms)])
       (cond
        [(and (null? pos) (null? neg)) 1]
        [(null? pos)
         `(/ 1 ,(make-multiplication-node* (map negate-term neg)))]
        [(null? neg)
         (make-multiplication-node* pos)]
        [else
         `(/ ,(make-multiplication-node* pos)
             ,(make-multiplication-node* (map negate-term neg)))]))]
    [`(,a ,terms ...)
     (let-values ([(pos neg) (partition (compose positive? car) terms)])
       (cond
        [(and (null? pos) (null? neg)) a]
        [(null? pos)
         `(/ ,a ,(make-multiplication-node* (map negate-term neg)))]
        [(null? neg)
         `(* ,a ,(make-multiplication-node* pos))]
        [else
         `(/ (* ,a ,(make-multiplication-node* pos))
             ,(make-multiplication-node* (map negate-term neg)))]))]))

(define (make-multiplication-node* terms)
  (match terms
    ['() 1]
    [`(,term)
     (mterm->expr term)]
    [`(,term ,terms ...)
     `(* ,(mterm->expr term) ,(make-multiplication-node* terms))]))
