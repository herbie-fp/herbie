#lang racket

(require "../syntax/syntax.rkt" "../syntax/types.rkt"
         "../programs.rkt" "../errors.rkt")
(provide egg-expr->expr egg-exprs->exprs
         expr->egglog egglog->expr (struct-out egraph-data))

(struct egraph-data (egg->herbie-dict herbie->egg-dict) #:prefab)

(define (extract-operator impl types)
  (cond
    [(equal? impl 'if)
     (when (not (equal? (second types) (third types)))
       (error (format "if: types of then and else branches must match. Got: ~s and ~s" (second types) (third types))))
     (list 'if (second types))]
    [else
     (list
      (impl->operator impl)
      (operator-info impl 'otype))]))


;; Converts a string expression from egg into a Racket S-expr
(define (egg-expr->expr ctx egg-data expr)
  (define parsed (read (open-input-string expr)))
  (car (egg-parsed->expr ctx parsed (egraph-data-egg->herbie-dict egg-data))))

(define (egglog->expr ctx egg-data expr)
  (car (egg-parsed->expr ctx expr (egraph-data-egg->herbie-dict egg-data))))

;; Like `egg-expr->expr` but expected the string to
;; parse into a list of S-exprs
(define (egg-exprs->exprs ctx exprs eg-data)
  (define port (open-input-string exprs))
  (let loop ([parse (read port)] [exprs '()])
    (cond
      [(eof-object? parse)
       (reverse exprs)]
      [else
       (define expr
         (car
          (egg-parsed->expr
           ctx
           parse (egraph-data-egg->herbie-dict eg-data))))
       (loop (read port) (cons expr exprs))])))

(define special-egg-names
  (make-hash `((+ . Add)
               (- . Sub)
               (* . Mul)
               (/ . Div)
               (< . Less)
               (> . Greater)
               (<= . LessEq)
               (>= . GreaterEq)
               (== . Eq)
               (!= . NotEq)
               (E . E)
               (PI . PI)
               (INFINITY . INFINITY)
               (TRUE . TRUE)
               (FALSE . FALSE)
               (NAN . NAN)
               )))
(define special-egg-names-reversed
  (make-hash
   (map (lambda (x) (cons (cdr x) (car x)))
        (hash->list special-egg-names))))


(define (egg-name->name egg-name-symbol)
  (define egg-name (symbol->string egg-name-symbol))
  (define special (hash-ref special-egg-names-reversed egg-name-symbol #f))
  (if special
      special
      (string->symbol
       (string-append
        (string-downcase (substring egg-name 0 1))
        (substring egg-name 1)))))

(define (name->egg-name name-symbol)
  (define name (symbol->string name-symbol))
  (define special (hash-ref special-egg-names name-symbol #f))
  (if special
      special
      (string->symbol
       (string-append
        (string-upcase (substring name 0 1))
        (substring name 1)))))


;; renames variables back to non upper case
;; special names in map
(define (egg-parsed->expr ctx parsed rename-dict)
  (match parsed
    [`(Num ,type (rational ,num ,denom))
     (cons (/ (string->number num) (string->number denom)) (context-repr ctx))]
    [`(Var ,type ,var-str)
     (define var (string->symbol var-str))
     (define name (hash-ref rename-dict var))
     (cons name (context-lookup ctx name))]
    [(list first-parsed `(Type ,second-parsed) rest-parsed ...)
     (define op (egg-name->name first-parsed))
     (define type (string->symbol second-parsed))
     (define children
       (map (curryr (curry egg-parsed->expr ctx) rename-dict) rest-parsed))
     (define children-types
       (map cdr children))
     (define children-parsed (map car children))

     (define repr
       (cond
         [(equal? op 'if)
          (second children-types)]
         [else
          (get-representation type)]))
     (define parametric
       (cond
         [(equal? op 'if)
          'if]
         [(constant-operator? op)
          (let/ec k
            (for/list ([name (operator-all-impls op)])
              (define rtype (operator-info name 'otype))
              (when (or (equal? rtype repr) (equal? (representation-type rtype) 'bool))
                (k name)))
            (raise-herbie-missing-error "Could not find constant implementation for ~a at ~a"
                                        op (representation-name repr)))]
         [else
           (apply
            (curry get-parametric-operator
                   op)
            children-types)]))

     (cons (cons parametric children-parsed)
           repr)]
    [else
     (error (format "Unrecognized egg expression ~a" parsed))]))

(define (expr->egglog ctx expr egg-data)
  (define egg->herbie-dict (egraph-data-egg->herbie-dict egg-data))
  (define herbie->egg-dict (egraph-data-herbie->egg-dict egg-data))
  (define res (car (expr->egg-expr-helper ctx expr egg->herbie-dict herbie->egg-dict)))
  res
  )

;; Needs the vartypes so we can look up var types
(define (expr->egg-expr-helper ctx expr egg->herbie-dict herbie->egg-dict)
  (cond
    [(list? expr)
     (define children
       (map (lambda (e) (expr->egg-expr-helper ctx e egg->herbie-dict herbie->egg-dict))
            (rest expr)))
     (define children-exprs
       (map car children))
     (define children-types
       (map cdr children))

     (match-define (list name type) (extract-operator (first expr) children-types))
     (cons
      (append
       (list (name->egg-name name) `(Type ,(symbol->string (representation-name type))))
       children-exprs)
      type)]
    [(and (number? expr) (exact? expr) (real? expr))
     (define type (context-repr ctx))
     (cons
      `(Num (Type ,(symbol->string (representation-name type)))
            (rational ,(number->string (numerator expr))
                      ,(number->string (denominator expr))))
      type)]
    [(hash-has-key? herbie->egg-dict expr)
     (define type (context-lookup ctx expr))
     (cons
      `(Var (Type ,(symbol->string (representation-name type)))
            ,(symbol->string (hash-ref herbie->egg-dict expr)))
      type)]
    [(symbol? expr)
     (define new-key (format "h~a" (number->string (hash-count herbie->egg-dict))))
     (define new-key-symbol (string->symbol new-key))

     (hash-set! herbie->egg-dict
                expr
                new-key-symbol)
     (hash-set! egg->herbie-dict
                new-key-symbol
                expr)
     (define type (context-lookup ctx expr))
     (cons
      `(Var (Type ,(symbol->string (representation-name type)))
            ,new-key)
      type)]
    [else
     (error (format "Unrecognized expression ~a" expr))]))


(module+ test
  (require rackunit "../load-plugin.rkt")
  (load-herbie-builtins)
  (define repr (get-representation 'binary64))
  (define ctx (context '(x) repr (list repr)))
  (define type `(Type "binary64"))
  (define bool `(Type "bool"))
  (egglog->expr
    ctx
    (egraph-data #hash((h0 . x)) #hash((x . h0))) 
    `(Fma ,type
          (Num ,type (rational "-1" "1"))
          (If ,type
              (And ,bool (GreaterEq ,bool (Var ,type "h0") (Num ,type (rational "0" "1"))) (GreaterEq ,bool (Var ,type "h0") (Num ,type (rational "0" "1"))))
              (Fabs ,type (Var ,type "h0"))
              (Pow ,type (Pow ,type (Var ,type "h0") (Num ,type (rational "1" "2"))) (Num ,type (rational "2" "1")))) (Var ,type "h0"))))
