#lang racket

(require "../errors.rkt" "types.rkt" "syntax.rkt")
(provide desugar-program resugar-program)

;; Herbie uses various IRs.
;; All IRs are S-expressions with symbolic operators, variables and numbers.
;; 
;; ## FPCore (with a couple modifications) ##
;;
;; - input language, loopless, bindings, lightly annotated
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
;;        ::= <number>
;;
;; Every (operator) implementation has a type signature taking inputs each
;; with a given number format producing an output of a possibly different
;; number format. In practice, most operator implemenetations have uniform
;; precision with only casts being multi-precision.
;;
;; ## SpecProg ##
;;
;; - internal language, real number programs
;; - no rounding information
;; - let expressions are inlined
;;
;; <expr> ::= (if <expr> <expr> <expr>)
;;        ::= (<op> <expr> ...)
;;        ::= <var>
;;        ::= <number>
;;
;; Every operator takes real numbers and produces a real number output.
;; This is essentially just FPCore with let expressions inlined and
;; rounding annotations stripped. Conversion to this IR is irreversible.
;;

;; Expression pre-processing for normalizing expressions.
;; Used for conversion from FPCore to other IRs.
(define (expand-expr expr)
  (let loop ([expr expr] [env '()])
    (match expr
      ; empty let/let* expression
      [`(,(or 'let 'let*) () ,body)
       (loop body)]
      ; let* expression
      [`(let* ([,var ,val] ,rest ...) ,body)
       (loop `(let ([,var ,val]) (let* ,rest ,body)) env)]
      ; let expression
      [`(let ([,vars ,vals] ...) ,body)
       (loop body (for/fold ([env* env]) ([var (in-list vars)] [val (in-list vals)])
                    (dict-set env* var (loop val env))))]
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
      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b)
       `(,op ,(loop `(,op ,@as) env) ,(loop b env))]
      ; expand comparison associativity
      [`(,(and (or '< '<= '> '>= '=) op) ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f]) ([term as*] [next (cdr as*)])
           (if out
               (list 'and out (list op term next))
               (list op term next))))
       (or out '(TRUE))]
      [`(!= ,as ...)
       (define as* (map (curryr loop env) as))
       (define out
         (for/fold ([out #f])
             ([term as*] [i (in-naturals)]
              #:when true
              [term2 as*] [j (in-naturals)]
              #:when (< i j))
           (if out
               (list 'and out (list '!= term term2))
               (list '!= term term2))))
       (or out '(TRUE))]
      ; function calls
      [(list (? (curry hash-has-key? (*functions*)) fname) args ...)
       (match-define (list vars _ body) (hash-ref (*functions*) fname))
       (loop body (for/fold ([env* '()]) ([var (in-list vars)] [arg (in-list args)])
                    (dict-set env* var (loop arg env))))]
      ; applications
      [`(,op ,args ...) `(,op ,@(map (curryr loop env) args))]
      ; constants
      [(? constant-operator?) (list expr)]
      ; variables
      [(? symbol?) (dict-ref env expr expr)]
      ; other
      [_ expr])))

;; Translates an FPCore to a specification.
(define (fpcore->spec expr)
  (let loop ([expr (expand-expr expr)])
    (match expr
      [`(FPCore ,name (,vars ...) ,props ... ,body)
       `(FPCore ,name ,vars ,@props ,(loop body))]
      [`(FPCore (,vars ...) ,props ... ,body)
       `(FPCore ,vars ,@props ,(loop body))]
      [`(if ,cond ,ift ,iff)
       `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [`(! ,_ ... ,body)
       (loop body)]
      [`(,op ,args ...)
       `(,op ,@(map loop args))]
      [_ expr])))

;; Translates an FPCore to an ImplProg.
(define (fpcore->prog expr ctx)
  (define-values (expr* _)
    (let loop ([expr (expand-expr expr)] [ctx ctx])
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
           (match (dict-ref props ':props)
             [prec (struct-copy context ctx [repr (get-representation prec)])]
             [#f ctx]))
         (loop body ctx*)]
        [`(cast ,body)
         (define repr (context-repr ctx))
         (define-values (body* repr*) (loop body ctx))
         (cond
           [(equal? repr repr*)
            ; cast is redundant
            (values body* repr)]
           [(equal? repr repr*)
            (define conv (get-repr-conv repr* repr))
            (values (list conv body*) repr)])]
        [`(,(? constant-operator? x))
         (define cnst (get-parametric-constant x (context-repr ctx)))
         (values (list cnst) (impl-info cnst 'otype))]
        [(list 'neg arg)
         (define-values (arg* atype) (loop arg ctx))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (impl-info op* 'otype))]
        [`(,op ,args ...)
         (define-values (args* atypes)
           (for/lists (args* atypes) ([arg args])
             (loop arg ctx)))
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
            (define conv (get-repr-conv vrepr repr))
            (unless conv
              (raise-herbie-missing-error "conversion does not exist: ~a -> ~a"
                                          (representation-name vrepr)
                                          (representation-name repr)))
            (values (list conv expr) repr)])]
        [(? number?)
         (define num
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)]))
         (values num (context-repr ctx))]
        [(? boolean?)
         (values expr (get-representation 'bool))])))
  expr*)

;; Translates an ImplProg to an FPCore.
(define (prog->fpcore prog repr)
  (let loop ([expr prog])
    (match expr
      [`(if ,cond ,ift ,iff)
       `(if ,(loop cond) ,(loop ift) ,(loop iff))]
      [`(,(? repr-conv? impl) ,body)
       (match-define (list irepr) (impl-info impl 'itype))
       (define body* (prog->fpcore body irepr))
       `(cast (! :precision ,(representation-name irepr)) ,body*)]
      [`(,impl)
       (impl->operator impl)]
      [`(,impl ,args ...)
       (define op (impl->operator impl))
       (define args*
         (for/list ([arg (in-list args)] [irepr (impl-info impl 'itype)])
           (prog->fpcore arg irepr)))
       (match (cons op args*)
         [`(neg ,arg) `(- ,arg)]
         [expr expr])]
      [(? variable?) expr]
      [(? number?)
       (match expr
         [-inf.0 '(- INFINITY)]
         [+inf.0 'INFINITY]
         [+nan.0 'NAN]
         [_ (if (set-member? '(binary64 binary32) (representation-name repr))
                (exact->inexact expr)
                expr)])])))
       
;; Translates an ImplProg to an FPCore
(define (prog->spec prog)
  (fpcore->spec (prog->fpcore prog)))

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric expr repr var-reprs full?)
  (define-values (expr* prec)
    (let loop ([expr expr] [repr repr]) ; easier to work with repr names
      ;; Run after unfold-let, so no need to track lets
      (match expr
        [(list 'if cond ift iff)
         (define-values (cond* _a) (loop cond repr))
         (define-values (ift* rtype) (loop ift repr))
         (define-values (iff* _b) (loop iff repr))
         (values (list 'if cond* ift* iff*) rtype)]
        [(list '! props ... body)
         (define props* (apply hash-set* (hash) props))
         (cond
           [(hash-has-key? props* ':precision)
            ; need to insert a cast
            (loop (list 'cast expr) repr)]
           [else (loop body repr)])]
        [(list 'cast (list '! ':precision iprec iexpr))
         (define irepr (get-representation iprec))
         (cond
          [(equal? repr irepr)  ; ignore
           (loop iexpr repr)]
          [else
           (define conv (get-repr-conv irepr repr))
           (define-values (iexpr* _) (loop iexpr irepr))
           (values (list conv iexpr*) repr)])]
        [(list 'cast body)    ; no-op cast (ignore)
         (loop body repr)]
        [(list (or 'neg '-) arg) ; unary minus
         (define-values (arg* atype) (loop arg repr))
         (define op* (get-parametric-operator 'neg atype))
         (values (list op* arg*) (impl-info op* 'otype))]
        [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
         (define irepr (first (impl-info op 'itype)))
         (define orepr (impl-info op 'otype))
         (define-values (body* rtype) (loop body irepr))
         (values (list op body*) orepr)]
        [(or (? constant-operator? x) (list x)) ; constant
         (define cnst (get-parametric-constant x repr))
         (define rtype (impl-info cnst 'otype))
         (values (list cnst) rtype)]
        [(list op args ...)
         (define-values (args* atypes)
           (for/lists (args* atypes) ([arg args])
             (loop arg repr)))
         ;; Match guaranteed to succeed because we ran type-check first
         (define op* (apply get-parametric-operator op atypes))
         (values (cons op* args*) (impl-info op* 'otype))]
        [(? number?) 
         (values
           (match expr
             [(or +inf.0 -inf.0 +nan.0) expr]
             [(? exact?) expr]
             [_ (inexact->exact expr)])
           repr)]
        [(? boolean?) (values expr (get-representation 'bool))]
        [(? variable?)
         (define vrepr (dict-ref var-reprs expr))
         (cond
          [(equal? (representation-type vrepr) 'bool) (values expr vrepr)]
          [(equal? vrepr repr) (values expr repr)]
          [else
           (define conv (get-repr-conv vrepr repr))
           (unless conv
             (raise-herbie-missing-error "Conversion does not exist: ~a -> ~a"
                (representation-name vrepr) (representation-name repr)))
           (values (list conv expr) repr)])]))) 
  expr*)

;; TODO(interface): This needs to be changed once the syntax checker is updated
;; and supports multiple precisions
(define (expand-parametric-reverse expr repr full?)
  (match expr
    [(list 'if cond ift iff)
     (define cond* (expand-parametric-reverse cond repr full?))
     (define ift* (expand-parametric-reverse ift repr full?))
     (define iff* (expand-parametric-reverse iff repr full?))
     (list 'if cond* ift* iff*)]
    [(list (? repr-conv? op) body) ; conversion (e.g. posit16->f64)
     (define repr* (first (impl-info op 'itype)))
     (define body* (expand-parametric-reverse body repr* full?))
     (cond
      [(not full?) `(,op ,body*)]
      [(list? body*) `(cast (! :precision ,(representation-name repr*) ,body*))]
      [else body*])] ; constants and variables should not have casts and precision changes
    [(list op)
     (define op* (impl->operator op))
     (if full? op* (list op*))]
    [(list op args ...)
     (define op* (impl->operator op))
     (define args*
       (for/list ([arg args] [repr (impl-info op 'itype)])
         (expand-parametric-reverse arg repr full?)))
     (if (and full? (equal? op* 'neg) (= (length args) 1)) ; if only unparameterizing, leave 'neg' alone
         (cons '- args*)
         (cons op* args*))]
    [(? number?)
     (if full?
         (match expr
           [-inf.0 (if full? '(- INFINITY) '(neg INFINITY))] ; not '(neg INFINITY) because this is post-resugaring
           [+inf.0 'INFINITY]
           [+nan.0 'NAN]
           [x
            ;; TODO: Why is this here?
            (if (set-member? '(binary64 binary32) (representation-name repr))
                 (exact->inexact x) ; convert to flonum if binary64 or binary32
                 x)])
         expr)]
    [(? variable?) expr]))

(define (desugar-program prog ctx #:full [full? #t])
  (define repr (context-repr ctx))
  (define var-reprs (map cons (context-vars ctx) (context-var-reprs ctx)))
  (if full?
      (fpcore->prog prog ctx)
      (expand-parametric prog repr var-reprs full?)))

(define (resugar-program prog repr #:full [full? #t])
  (if full?
      (prog->fpcore prog repr)
      (expand-parametric-reverse prog repr full?)))
