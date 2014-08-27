#lang racket

(require rackunit)
(require casio/rules)
(require casio/matcher)
(require casio/common)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define fn-inverses
  (map rule-input (filter (λ (rule) (symbol? (rule-output rule))) *rules*)))

#;(define (has-duplicates? l)
  (not (equal? l (remove-duplicates l))))

(define (simplify expr)
  (match expr
    [(? real?) expr]
    [(? symbol?) expr]
    [`(,op ,args ...)
     (simplify-node `(,op ,@(map simplify args)))]
    [`(λ ,vars ,body)
     `(λ ,vars ,(simplify body))]
    [`(lambda ,vars ,body)
     `(λ ,vars ,(simplify body))]))

(define (simplify-node expr)
  (match expr
    [(? real?) expr]
    [(? symbol?) expr]
    [(or `(+ ,summands ...) `(- ,summands ...))
     (let* ([labels (combining-labels (gather-additive-terms expr))]
            [terms (combine-aterms (gather-additive-terms expr #:expand labels))])
       (make-addition-node terms))]
    [(or `(* ,summands ...) `(/ ,summands ...))
     (let ([terms (combine-mterms (gather-multiplicative-terms expr))])
       (make-multiplication-node terms))]
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
      [(? real?) `((,expr 1))]
      [(? symbol?) `((1 ,expr))]
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
           `((1 ,expr)))]
      [`(/ ,arg) ; Prevent fall-through to the next case
       `((1 ,expr))]
      [`(/ ,arg ,args ...)
       (if (or (not expand) (memq label expand))
           (let ([nums (recurse arg)])
             (for/list ([term nums])
               (cons (car term) (simplify-node (list* '/ (cadr term) args)) (cons label (cddr term)))))
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
    [(? real?) `(,expr)]
    [(? symbol?) `(1 (1 . ,expr))]
    [`(* ,args ...)
     (let ([terms (map gather-multiplicative-terms args)])
       (cons (apply * (map car terms)) (apply append (map cdr terms))))]
    [`(/ ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cons (/ (car terms)) (map negate-term (cdr terms))))]
    [`(/ ,arg ,args ...)
     (let ([num (gather-multiplicative-terms arg)]
           [dens (map gather-multiplicative-terms args)])
       (cons (apply / (car num) (map car dens))
             (append (map cdr num)
                     (map negate-term (append-map cdr dens)))))]
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

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node terms)
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
    [`(,pow . ,x) `(expt ,x ,pow)]))

(define (make-multiplication-node term)
  (match term
    [`(0 ,terms ...) 0]
    [`(1 ,terms ...) (make-multiplication-node* terms)]
    [`(,a ,terms ...) (list '* a (make-multiplication-node* terms))]))

;; TODO : Use (/ a b) when it is simpler
(define (make-multiplication-node* terms)
  (match terms
    ['() 1]
    [`(,term)
     (mterm->expr term)]
    [`(,term ,terms ...)
     `(* ,(mterm->expr term) ,(make-multiplication-node* terms))]))
