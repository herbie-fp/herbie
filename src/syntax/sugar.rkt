#lang racket

(require "types.rkt"
         "syntax.rkt")

(provide fpcore->prog
         prog->fpcore
         prog->spec)

;; Herbie uses three expression languages.
;; All formats are S-expressions with variables, numbers, and applications.
;;
;; FPCore: the input/output language
;;
;; - standardized interchange format with other tools
;; - using loopless, tensorless subset
;; - operators denote real computations while rounding contexts decide format
;;
;;  <FPCore> ::= (FPCore (<var> ...) <props> ... <expr>)
;;             | (FPCore <id> (<var> ...) <props> ... <expr>)
;;
;;  <expr>   ::= (let ([<id> <expr>] ...) <expr>)
;;             | (let* ([<id> <expr>] ...) <expr>)
;;             | (if <expr> <expr> <expr>
;;             | (cast <expr>)
;;             | (! <props> ... <expr>)
;;             | (<op> <expr> ...)
;;             | <number>
;;             | <id>
;;
;;  <var>    ::= <id>
;;             | (! <props> ... <expr>)
;;
;; LImpl: the language of floating-point expressions
;;
;; - internal language, describes floating-point expressions
;; - unary minus represented by `neg`
;; - every operator is rounded
;; - let expressions are inlined
;; - numbers are rounded to a particular format
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<impl> <expr> ...)
;;        ::= (literal <number> <repr>)
;;        ::= <id>
;;
;; Every operator has a type signature where types are representations.
;; In practice, most operator implemenetations have uniform representations but
;; operations like casts are multi-format.
;;
;; LSpec: the language of mathematical formula
;;
;; - internal language, describes real-number formula
;; - unary minus represented by `neg`
;; - every operator is a real-number operation
;; - every operator is supported by Rival
;; - let expressions are inlined
;; - casts are mapped to the identity operation
;; - numbers are formatless
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<op> <expr> ...)
;;        ::= <number>
;;        ::= <id>
;;

;; Expression pre-processing for normalizing expressions.
;; Used for conversion from FPCore to other IRs.
(define (expand-expr expr)
  (let loop ([expr expr]
             [env '()])
    (match expr
      ; empty let/let* expression
      [`(,(or 'let 'let*) () ,body) (loop body env)]
      ; let* expression
      [`(let* ([,var ,val]
               ,rest ...)
          ,body)
       (loop `(let ([,var ,val]) (let* ,rest ,body)) env)]
      ; let expression
      [`(let ([,vars ,vals] ...) ,body)
       (define env*
         (for/fold ([env* env])
                   ([var (in-list vars)]
                    [val (in-list vals)])
           (dict-set env* var (loop val env))))
       (loop body env*)]
      ; nullary expressions
      ['(and) '(TRUE)]
      ['(or) '(FALSE)]
      [`(,(or '+ '-)) 0]
      [`(,(or '* '/)) 1]
      ; unary expressions
      [`(,(or '+ '* 'and 'or) ,a) (loop a env)]
      [`(- ,a) `(neg ,(loop a env))]
      [`(/ ,a) `(/ 1 ,(loop a env))]
      ; expand arithmetic associativity
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b) `(,op ,(loop `(,op ,@as) env) ,(loop b env))]
      ; expand comparison associativity
      [`(,(and (or '< '<= '> '>= '=) op) ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f])
                   ([term as*]
                    [next (cdr as*)])
           (if out (list 'and out (list op term next)) (list op term next))))
       (or out '(TRUE))]
      [`(!= ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f])
                   ([term as*]
                    [i (in-naturals)]
                    #:when true
                    [term2 as*]
                    [j (in-naturals)]
                    #:when (< i j))
           (if out (list 'and out (list '!= term term2)) (list '!= term term2))))
       (or out '(TRUE))]
      ; function calls
      [(list (? (curry hash-has-key? (*functions*)) fname) args ...)
       (match-define (list vars _ body) (hash-ref (*functions*) fname))
       (define env*
         (for/fold ([env* '()])
                   ([var (in-list vars)]
                    [arg (in-list args)])
           (dict-set env* var (loop arg env))))
       (loop body env*)]
      ; applications
      [`(,op ,args ...) `(,op ,@(map (curryr loop env) args))]
      ; constants
      [(? constant-operator?) (list expr)]
      ; variables
      [(? symbol?) (dict-ref env expr expr)]
      ; other
      [_ expr])))

;; Prop list to dict
(define (props->dict props)
  (let loop ([props props]
             [dict '()])
    (match props
      [(list key val rest ...) (loop rest (dict-set dict key val))]
      [(list key) (error 'props->dict "unmatched key" key)]
      [(list) dict])))

;; Translates from FPCore to an LImpl
(define (fpcore->prog prog ctx)
  (define-values (expr* _)
    (let loop ([expr (expand-expr prog)]
               [ctx ctx])
      (match expr
        [`(FPCore ,name (,vars ...) ,props ... ,body)
         (define-values (body* repr*) (loop body ctx))
         (values `(FPCore ,name ,vars ,@props ,body*) repr*)]
        [`(FPCore (,vars ...) ,props ... ,body)
         (define-values (body* repr*) (loop body ctx))
         (values `(FPCore ,vars ,@props ,body*) repr*)]
        [`(if ,cond ,ift ,iff)
         (define-values (cond* cond-repr) (loop cond ctx))
         (define-values (ift* ift-repr) (loop ift ctx))
         (define-values (iff* iff-repr) (loop iff ctx))
         (values `(if ,cond* ,ift* ,iff*) ift-repr)]
        [`(! ,props ... ,body)
         (define props* (props->dict props))
         (loop body
               (match (dict-ref props* ':precision #f)
                 [#f ctx]
                 [prec (struct-copy context ctx [repr (get-representation prec)])]))]
        [`(cast ,body)
         (define repr (context-repr ctx))
         (define-values (body* repr*) (loop body ctx))
         (if (equal? repr* repr) ; check if cast is redundant
             (values body* repr)
             (values (list (get-cast-impl repr* repr) body*) repr))]
        [`(,(? constant-operator? x))
         (define cnst (get-parametric-constant x (context-repr ctx)))
         (values (list cnst) (impl-info cnst 'otype))]
        [(list 'neg arg) ; non-standard but useful
         (define-values (arg* atype) (loop arg ctx))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (impl-info op* 'otype))]
        [`(,op ,args ...)
         (define-values (args* atypes) (for/lists (args* atypes) ([arg args]) (loop arg ctx)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (impl-info op* 'otype))]
        [(? variable?) (values expr (context-lookup ctx expr))]
        [(? number?)
         (define prec (representation-name (context-repr ctx)))
         (define num
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)]))
         (values (literal num prec) (context-repr ctx))])))
  expr*)

;; Translates from LImpl to an FPCore.
(define (prog->fpcore expr)
  (match expr
    [`(if ,cond ,ift ,iff) `(if ,(prog->fpcore cond) ,(prog->fpcore ift) ,(prog->fpcore iff))]
    [`(,(? cast-impl? impl) ,body)
     (define prec (representation-name (impl-info impl 'otype)))
     `(! :precision ,prec (cast ,(prog->fpcore body)))]
    [`(,impl) (impl->operator impl)]
    [`(,impl ,args ...)
     (define op (impl->operator impl))
     (define args* (map prog->fpcore args))
     (match (cons op args*)
       [`(neg ,arg) `(- ,arg)]
       [expr expr])]
    [(approx _ impl) (prog->fpcore impl)]
    [(? variable?) expr]
    [(? literal?)
     (match (literal-value expr)
       [-inf.0 '(- INFINITY)]
       [+inf.0 'INFINITY]
       [+nan.0 'NAN]
       [v (if (set-member? '(binary64 binary32) (literal-precision expr)) (exact->inexact v) v)])]))

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (expand-accelerators
   (match expr
     [`(if ,cond ,ift ,iff) `(if ,(prog->spec cond) ,(prog->spec ift) ,(prog->spec iff))]
     [`(,(? cast-impl? impl) ,body) `(,impl ,(prog->spec body))]
     [`(,impl ,args ...) `(,(impl->operator impl) ,@(map prog->spec args))]
     [(approx spec _) spec]
     [(? variable?) expr]
     [(? literal?) (literal-value expr)])))
