#lang racket

(require "../utils/errors.rkt"
         "types.rkt"
         "syntax.rkt")
(provide fpcore->prog
         prog->fpcore
         prog->spec
         spec->prog)

;; Herbie uses various IRs.
;; All IRs are S-expressions with symbolic operators, variables and numbers.
;;
;; ## FPCore (with a couple modifications) ##
;;
;; - input language, loopless, with bindings, lightly annotated
;; - operators denote real computations
;; - rounding context (in addition to variable context) decides format at node
;;
;;  <FPCore> ::= (FPCore (<var> ...) <props> ... <expr>)
;;           ::= (FPCore <name> (<var> ...) <props> ... <expr>)
;;  <expr>   ::= (let ([<var> <expr>] ...) <expr>)
;;           ::= (let* ([<var> <expr>] ...) <expr>)
;;           ::= (if <expr> <expr> <expr>
;;           ::= (! <props> ... <expr>)
;;           ::= (<op> <expr> ...)
;;           ::= <var>
;;           ::= <number>
;;
;; To propagate types through an FPCore, the types of the rounding context and
;; variables must be known. Variable types propagate up and rounding contexts
;; propagate down (modified only by !)
;;
;; ## ImplProg ##
;;
;; - internal language, floating-point programs
;; - rounding context from FPCore embedded in operators (called implementations)
;; - let expressions are inlined
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<impl> <expr> ...)
;;        ::= <var>
;;        ::= (literal <number> <repr-name>)
;;
;; Every (operator) implementation has a type signature taking inputs each
;; with a given number format producing an output of a possibly different
;; number format. In practice, most operator implemenetations have uniform
;; precision with only casts being multi-precision.
;;
;; ## Spec ##
;;
;; - internal language, (almost) real-number programs
;; - let expressions are inlined
;; - almost like FPCore exception
;;   (i) unary negation is `neg`
;;   (ii) casts are left as-is
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<op> <expr> ...)
;;        ::= <var>
;;        ::= <number>
;;
;; Every operator takes real numbers and produces a real number output.
;; Ideally, a specification would be an FPCore with its rounding
;; annotations completely stripped, but this conversion would be irreversible
;; for multi-precision expressions due to the removal of casts.
;; These issues exist for historical reasons and should be removed from Herbie.
;; Unfortunately, removing it will require refactoring core algorithms.
;;

;; Expression pre-processing for normalizing expressions.
;; Used for conversion from FPCore to other IRs.
(define (expand-expr expr)
  (let loop ([expr expr] [env '()])
    (match expr
      ; empty let/let* expression
      [`(,(or 'let 'let*) () ,body) (loop body env)]
      ; let* expression
      [`(let* ([,var ,val] ,rest ...) ,body) (loop `(let ([,var ,val]) (let* ,rest ,body)) env)]
      ; let expression
      [`(let ([,vars ,vals] ...) ,body)
       (define env*
         (for/fold ([env* env]) ([var (in-list vars)] [val (in-list vals)])
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
         (for/fold ([out #f]) ([term as*] [next (cdr as*)])
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
         (for/fold ([env* '()]) ([var (in-list vars)] [arg (in-list args)])
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
  (let loop ([props props] [dict '()])
    (match props
      [(list key val rest ...) (loop rest (dict-set dict key val))]
      [(list key) (error 'props->dict "unmatched key" key)]
      [(list) dict])))

;; Translates an FPCore to a specification.
(define (fpcore->spec expr)
  (let loop ([expr (expand-expr expr)])
    (match expr
      [`(FPCore ,name (,vars ...) ,props ... ,body) `(FPCore ,name ,vars ,@props ,(loop body))]
      [`(FPCore (,vars ...) ,props ... ,body) `(FPCore ,vars ,@props ,(loop body))]
      [`(if ,cond ,ift ,iff) `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [`(! ,_ ... ,body) (loop body)]
      [`(,op ,args ...) `(,op ,@(map loop args))]
      [_ expr])))

;; Translates an FPCore to an ImplProg.
(define (fpcore->prog prog ctx)
  (define-values (expr* _)
    (let loop ([expr (expand-expr prog)] [ctx ctx])
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
         (define ctx*
           (match (dict-ref (props->dict props) ':props #f)
             [#f ctx]
             [prec (struct-copy context ctx [repr (get-representation prec)])]))
         (loop body ctx*)]
        [`(cast ,body)
         (define repr (context-repr ctx))
         (define-values (body* repr*) (loop body ctx))
         (cond
           ; cast is redundant
           [(equal? repr repr*) (values body* repr)]
           [else
            (define cast (get-cast-impl repr* repr))
            (values (list cast body*) repr)])]
        [`(,(? constant-operator? x))
         (define cnst (get-parametric-constant x (context-repr ctx)))
         (values (list cnst) (impl-info cnst 'otype))]
        [(list 'neg arg)
         (define-values (arg* atype) (loop arg ctx))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (impl-info op* 'otype))]
        [`(,op ,args ...)
         (define-values (args* atypes) (for/lists (args* atypes) ([arg args]) (loop arg ctx)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (impl-info op* 'otype))]
        [(? variable?)
         (define vrepr (context-lookup ctx expr))
         (define repr (context-repr ctx))
         (cond
           [(equal? (representation-type vrepr) 'bool) (values expr vrepr)]
           [(equal? vrepr repr) (values expr repr)]
           [else
            (define cast (get-cast-impl vrepr repr))
            (unless cast
              (raise-herbie-missing-error "conversion does not exist: ~a -> ~a"
                                          (representation-name vrepr)
                                          (representation-name repr)))
            (values (list cast expr) repr)])]
        [(? number?)
         (define prec (representation-name (context-repr ctx)))
         (define num
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)]))
         (values (literal num prec) (context-repr ctx))]
        [(? boolean?) (values expr (get-representation 'bool))])))
  expr*)

;; Translates an ImplProg to an FPCore.
(define (prog->fpcore prog repr)
  (let loop ([expr prog])
    (match expr
      [`(if ,cond ,ift ,iff) `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [`(,(? cast-impl? impl) ,body)
       (match-define (list irepr) (impl-info impl 'itype))
       (define body* (prog->fpcore body irepr))
       (cond
         [(list? body*) `(cast (! :precision ,(representation-name irepr) ,body*))]
         [else body*])] ; constants and variables should not have casts
      [`(,impl) (impl->operator impl)]
      [`(,impl ,args ...)
       (define op (impl->operator impl))
       (define args* (map prog->fpcore args (impl-info impl 'itype)))
       (match (cons op args*)
         [`(neg ,arg) `(- ,arg)]
         [expr expr])]
      [(? variable?) expr]
      [(? literal?)
       (match (literal-value expr)
         [-inf.0 '(- INFINITY)]
         [+inf.0 'INFINITY]
         [+nan.0 'NAN]
         [v
          (if (set-member? '(binary64 binary32) (literal-precision expr)) (exact->inexact v) v)])])))

;; Translates an ImplProg to a Spec.
(define (prog->spec expr)
  (match expr
    [`(if ,cond ,ift ,iff) `(if ,(prog->spec cond) ,(prog->spec ift) ,(prog->spec iff))]
    [`(,(? cast-impl? impl) ,body) `(,impl ,(prog->spec body))]
    [`(,impl ,args ...)
     (define op (impl->operator impl))
     (define args* (map prog->spec args))
     `(,op ,@args*)]
    [(? variable?) expr]
    [(? literal?) (literal-value expr)]))

;; Translates a Spec to an FPCore.
;; Most of this is a copy of fpcore->prog but it's slightly different.
(define (spec->prog expr ctx)
  (define-values (expr* _)
    (let loop ([expr (expand-expr expr)] [ctx ctx])
      (match expr
        [`(if ,cond ,ift ,iff)
         (define-values (cond* cond-repr) (loop cond ctx))
         (define-values (ift* ift-repr) (loop ift ctx))
         (define-values (iff* iff-repr) (loop iff ctx))
         (values `(if ,cond* ,ift* ,iff*) ift-repr)]
        [`(! ,props ... ,body)
         (define ctx*
           (match (dict-ref (props->dict props) ':props #f)
             [#f ctx]
             [prec (struct-copy context ctx [repr (get-representation prec)])]))
         (loop body ctx*)]
        [`(cast ,body)
         (define repr (context-repr ctx))
         (define-values (body* repr*) (loop body ctx))
         (cond
           ; cast is redundant
           [(equal? repr repr*) (values body* repr)]
           [else
            (define cast (get-cast-impl repr* repr))
            (values (list cast body*) repr)])]
        [`(,(? constant-operator? x))
         (define cnst (get-parametric-constant x (context-repr ctx)))
         (values (list cnst) (impl-info cnst 'otype))]
        [(list 'neg arg)
         (define-values (arg* atype) (loop arg ctx))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (impl-info op* 'otype))]
        [`(,(? cast-impl? impl) ,body)
         ; this case is supported here but not by fpcore->prog
         (match-define (list irepr) (impl-info impl 'itype))
         (define-values (body* _) (loop body (struct-copy context ctx [repr irepr])))
         (values `(,impl ,body*) (impl-info impl 'otype))]
        [`(,op ,args ...)
         (define-values (args* atypes) (for/lists (args* atypes) ([arg args]) (loop arg ctx)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (impl-info op* 'otype))]
        [(? variable?)
         (define vrepr (context-lookup ctx expr))
         (define repr (context-repr ctx))
         (cond
           [(equal? (representation-type vrepr) 'bool) (values expr vrepr)]
           [(equal? vrepr repr) (values expr repr)]
           [else
            (define cast (get-cast-impl vrepr repr))
            (unless cast
              (raise-herbie-missing-error "conversion does not exist: ~a -> ~a"
                                          (representation-name vrepr)
                                          (representation-name repr)))
            (values (list cast expr) repr)])]
        [(? number?)
         (define prec (representation-name (context-repr ctx)))
         (define num
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)]))
         (values (literal num prec) (context-repr ctx))]
        [(? boolean?) (values expr (get-representation 'bool))])))
  expr*)
