#lang racket

(require "../core/programs.rkt"
         "../utils/common.rkt"
         "syntax.rkt"
         "types.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FPCore -> LImpl

;; Translates from FPCore to an LImpl
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> FPCore

;; Returns the dictionary FPCore properties required to
;; round trip when converted to and from FPCore.
; (define (prog->req-props expr)
;   (match expr
;     [(? literal?) '()]
;     [(? variable?) '()]
;     [(list 'if cond ift iff) (prog->req-props ift)]
;     [(list (? impl-exists? impl) _ ...)
;      (match-define (list '! props ... _) (impl-info impl 'fpcore))
;      (props->dict props)]))

;; Instruction vector index
(struct index (v) #:prefab)

;; Translates a literal (LImpl) to an FPCore expr
(define (literal->fpcore x)
  (match x
    [(literal -inf.0 _) '(- INFINITY)]
    [(literal +inf.0 _) 'INFINITY]
    [(literal v (or 'binary64 'binary32)) (exact->inexact v)]
    [(literal v _) v]))

;; Translates from LImpl to an instruction vector where each
;; expression is evaluated under an explicit rounding context.
;; The output resembles A-normal form, so we will call the output "normal".
;; NOTE: This translation results in a verbose output but at least it's right
(define (prog->normal expr)
  (define exprs '())
  (define (push! impl node)
    (define id (length exprs))
    (set! exprs (cons (cons impl node) exprs))
    (index id))

  (define (munge expr)
    (match expr
      [(? literal?) (literal->fpcore expr)]
      [(? symbol?) expr]
      [(list 'if cond ift iff)
       (list 'if (munge cond) (munge ift) (munge iff))]
      [(list (? impl-exists? impl) args ...)
       (define args* (map munge args))
       (match-define (list _ vars _) (impl-info impl 'spec))
       (push! impl (replace-vars (map cons vars args*) (impl-info impl 'fpcore)))]))
  
  (define node (munge expr))
  (unless (index? node)
    (set! exprs (cons node exprs)))
  (list->vector (reverse exprs)))


; (define (prog->normal expr ctx)
;   (define vars (list->mutable-seteq (context-vars ctx)))
;   (define counter 0)
;   (define (gensym)
;     (set! counter (add1 counter))
;     (match (string->symbol (format "t~a" counter))
;       [(? (curry set-member? vars)) (gensym)]
;       [x
;        (set-add! vars x)
;        x]))

;; Translates from LImpl to an FPCore.
;; The implementation of this procedure is complicated since
;;  (1) every operator implementation requires certain (FPCore) rounding properties
;;  (2) rounding contexts have lexical scoping
;; FPCore can be precise, but that precision comes at the cost of complexity.
(define (prog->fpcore expr ctx)
  (eprintf "~a\n" expr)
  ; step 1: convert to an instruction vector where
  ; each expression is evaluated under explicit rounding contexts
  (define ivec (prog->normal expr))
  (define ivec-len (vector-length ivec))
  (define root (vector-ref ivec (sub1 ivec-len)))
  (eprintf "~a\n" ivec)

  ; step 2: condense nodes
  ; the value of a let binding may be inlined if converting back to LImpl
  ; results in the same 
  (define condensed (mutable-set))
  (define body
    (let loop ([node root])
      (match node
        [(? number?) node]
        [(? symbol?) node]
        [_ (error 'condense "unimplemented: ~a"node)])))

  (error 'prog->fpcore "unimplemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LImpl -> LSpec

;; Translates an LImpl to a LSpec.
(define (prog->spec expr)
  (match expr
    [(? literal?) (literal-value expr)]
    [(? variable?) expr]
    [`(if ,cond ,ift ,iff) `(if ,(prog->spec cond) ,(prog->spec ift) ,(prog->spec iff))]
    [`(,impl ,args ...)
     (match-define `(,_ (,vars ...) ,spec) (impl-info impl 'spec))
     (define env (map cons vars (map prog->spec args)))
     (replace-vars env spec)]))
