#lang racket

(require "../common.rkt" "../programs.rkt" "matcher.rkt"
         "../function-definitions.rkt" "../syntax/rules.rkt" "../syntax/syntax.rkt"
         "../type-check.rkt")

(provide simplify)

;; Cancellation's goal is to cancel (additively or multiplicatively) like terms.
;; It uses commutativity, identities, inverses, associativity,
;; distributativity, and function inverses.

(define fn-inverses
  (map rule-input (filter (λ (rule) (herbie-variable? (rule-output rule))) (*rules*))))

(define (simplify expr)
  (define expr* expr)
  (let loop ([expr expr])
    (match expr
      [`(pow ,base ,(? real?))
       (when (equal? (type-of base (for/hash ([var (free-variables base)]) (values var 'real))) 'complex)
         (error "Bad pow!!!" expr*))]
      [(list f args ...)
       (for-each loop args)]
      [_ 'ok]))

  (let ([simpl (simplify* expr)])
    (debug #:from 'backup-simplify "Simplify" expr "into" simpl)
    simpl))

(define (simplify* expr*)
  (define expr ((get-evaluator) expr*))
  (match expr
    [(? constant-or-num?) expr]
    [(? herbie-variable?) expr]
    [`(λ ,vars ,body)
     `(λ ,vars ,(simplify* body))]
    [`(lambda ,vars ,body)
     `(λ ,vars ,(simplify* body))]
    [(? (compose null? free-variables) `(,op ,args ...))
     (let ([value (with-handlers ([(const #t) (const #f)]) (eval-const-expr expr))])
       (if (and (number? value) (real? value) (exact? value))
           value
           (simplify-node `(,op ,@(map simplify* args)))))]
    [`(,op ,args ...)
     (simplify-node `(,op ,@(map simplify* args)))]))

(define (simplify-node expr)
  (match expr
    [(? constant-or-num?) expr]
    [(? herbie-variable?) expr]
    [(or `(+ ,_ ...) `(- ,_ ...))
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

(define (gather-additive-terms expr #:expand [expand #f] #:label [label #f] )
  (define (recurse subexpr #:label [label #f])
    (gather-additive-terms subexpr #:expand expand #:label label))

  (let ([label (or label expr)])
    (match expr
      [(? number?) `((,expr 1))]
      [(? constant-or-num?) `((1 ,expr))]
      [(? herbie-variable?) `((1 ,expr))]
      [`(+ ,args ...) (append-map recurse args)]
      [`(- ,arg) (map negate-term (recurse arg))]
      [`(- ,arg ,args ...)
       (append (recurse arg)
               (map negate-term (append-map recurse args)))]

      [`(* ,args ...)
       (if (or (not expand) (memq label expand))
           (for/list ([term-list (apply cartesian-product (map recurse args))])
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

      [`(pow ,arg ,(? integer? n))
       (cond
        [(positive? n)
         (recurse (cons '* (build-list (inexact->exact n) (const arg))) #:label expr)]
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
    [`(sqrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (cond
        [(negative? (car terms))
         `(1 (1 . ,expr))]
        [(exact? (sqrt (car terms)))
         (cons (sqrt (car terms))
               (for/list ([term (cdr terms)])
                 (cons (/ (car term) 2) (cdr term))))]
        [else
         (list* 1
                (cons 1 `(sqrt ,(car terms)))
                (for/list ([term (cdr terms)])
                  (cons (/ (car term) 2) (cdr term))))]))]
    [`(cbrt ,arg)
     (let ([terms (gather-multiplicative-terms arg)])
       (define head-cbrt (expt (car terms) 1/3))
       (cond
        [(equal? (expt (inexact->exact head-cbrt) 3) (car terms))
         (cons head-cbrt
               (for/list ([term (cdr terms)])
                 (cons (/ (car term) 3) (cdr term))))]
        [else
         (list* 1
                (cons 1 `(cbrt ,(car terms)))
                (for/list ([term (cdr terms)])
                  (cons (/ (car term) 3) (cdr term))))]))]
    [`(pow ,arg ,(? real? a))
     (let ([terms (gather-multiplicative-terms arg)])
       (cond
        [(and (real? (expt (car terms) a)) (exact? (expt (car terms) a)))
         (cons (expt (car terms) a)
               (for/list ([term (cdr terms)])
                 (cons (* a (car term)) (cdr term))))]
        [else
         (list* 1
                (cons a (car terms))
                (for/list ([term (cdr terms)])
                  (cons (* a (car term)) (cdr term))))]))]
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
  (let-values ([(pos neg) (partition (λ (x) (and (real? (car x)) (positive? (car x)))) terms)])
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
    [`(,power . ,x)
     (match (type-of x (for/hash ([var (free-variables x)]) (values var 'real)))
       ['real `(pow ,x ,power)]
       ['complex `(pow ,x (complex ,power 0))])]))
