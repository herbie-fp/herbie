#lang racket

(require math/bigfloat rival)
(require "syntax/types.rkt" "syntax/syntax.rkt" "float.rkt" "interface.rkt"
         "timeline.rkt" "cost.rkt")

(module+ test (require rackunit))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables
         program-cost
         type-of repr-of
         expr-supports? expr-contains?
         location-hash
         location? expr?
         location-do location-get location-repr
         batch-eval-progs eval-prog eval-application
         free-variables replace-expression replace-vars
         apply-repr-change)

(define expr? (or/c list? symbol? value? real?))

(define location? (listof natural-number/c))

;; Programs are just lambda expressions

(define/contract (program-body prog)
  (-> expr? expr?)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define/contract (program-variables prog)
  (-> expr? (listof symbol?))
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  vars)

(define (program-cost prog)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  (expr-cost body))

;; Returns type name
;; Fast version does not recurse into functions applications
;; TODO(interface): Once we update the syntax checker to FPCore 1.1
;; standards, this will have to have more information passed in
(define (type-of expr repr env)
  (match expr
   [(? real?) (get-type 'real)]
   [(? (representation-repr? repr)) (representation-type repr)]
   [(? constant?) 
    (representation-type (get-representation (constant-info expr 'type)))]
   [(? variable?) (representation-type (dict-ref env expr))]
   [(list 'if cond ift iff) (type-of ift repr env)]
   [(list op args ...) ; repr-name -> repr -> type
    (representation-type (get-representation (operator-info op 'otype)))]))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr repr env)
  (match expr
   [(? real?) repr]
   [(? (representation-repr? repr)) repr]
   [(? constant?) (get-representation (constant-info expr 'type))]
   [(? variable?) (dict-ref env expr)]
   [(list 'if cond ift iff) (repr-of ift repr env)]
   [(list op args ...) (get-representation (operator-info op 'otype))]))

(define (expr-supports? expr field)
  (let loop ([expr expr])
    (match expr
      [(list op args ...)
       (and (operator-info op field) (andmap loop args))]
      [(? variable?) true]
      [(? constant?) (or (not (symbol? expr)) (constant-info expr field))])))

(define (expr-contains? expr pred)
  (let loop ([expr expr])
    (match expr
     [(list elems ...) (ormap loop elems)]
     [term (pred term)])))

;; Converting constants

(define/contract (location-hash prog)
  (-> expr? (hash/c expr? (listof location?)))
  (define hash (make-hash))
  (define (save expr loc)
    (hash-update! hash expr (curry cons loc) '()))

  (let loop ([expr prog] [loc '()])
    (match expr
      [(list (or 'lambda 'λ) (list vars ...) body)
       (loop body (cons 2 loc))]
      [(? constant?) (save expr (reverse loc))]
      [(? variable?) (save expr (reverse loc))]
      [(list op args ...)
       (save expr (reverse loc))
       (for ([idx (in-naturals 1)] [arg args])
         (loop arg (cons idx loc)))]))

  hash)

(define (free-variables prog)
  (match prog
    [(? constant?) '()]
    [(? variable?) (list prog)]
    [`(,op ,args ...)
     (remove-duplicates (append-map free-variables args))]))

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr)
     (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

(define/contract (location-do loc prog f)
  (-> location? expr? (-> expr? expr?) expr?)
  (cond
   [(null? loc)
    (f prog)]
   [(not (pair? prog))
    (error "Bad location: cannot enter " prog "any further.")]
   [#t
    ; Inlined loop for speed
    (let loop ([idx (car loc)] [lst prog])
      (if (= idx 0)
          (cons (location-do (cdr loc) (car lst) f) (cdr lst))
          (cons (car lst) (loop (- idx 1) (cdr lst)))))]))

(define (location-get loc prog)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define (location-repr loc prog repr var-reprs)
  (let loop ([expr prog] [repr repr] [loc loc])
    (cond
     [(null? loc)
      (get-representation
        (if (operator? expr)
            (operator-info expr 'otype)
            (repr-of expr repr var-reprs)))]
     [(not (pair? expr))
      (error "Bad location: cannot enter " expr "any further.")]
     [else
      (match expr
        [(list 'if cond ift iff)
         (loop (list-ref expr (car loc)) repr (cdr loc))]
        [(list (? operator? op) args ...)
         (define ireprs (cons repr (map get-representation (operator-info op 'itype))))
         (loop (list-ref expr (car loc)) (list-ref ireprs (car loc)) (cdr loc))]
        [(list (or 'λ 'lambda) (list vars ...) body)
         (loop (list-ref expr (car loc)) repr (cdr loc))])])))

(define (eval-prog prog mode repr)
  (define f (batch-eval-progs (list prog) mode repr))
  (λ args (vector-ref (apply f args) 0)))

(define (batch-eval-progs progs mode repr)
  ; Keep exact numbers exact
  ;; TODO(interface): Right now, real->precision and precision->real are
  ;; mixed up for bf and fl because there is a mismatch between the fpbench
  ;; input format for how we specify complex numbers (which is the format
  ;; the interface will ultimately use), and the 1.3 herbie input format
  ;; (which has no way of specifying complex numbers as input.) Once types
  ;; and representations are cleanly distinguished, we can get rid of the
  ;; additional check to see if the repr is complex.
  (define real->precision (match mode
    ['bf (λ (repr x) (->bf x repr))]
    ['fl (λ (repr x) (real->repr x repr))]
    ['ival (λ (repr x) (if (ival? x) x (mk-ival (->bf x repr))))]))
  
  (define vars (if (empty? progs) '() (program-variables (first progs))))
  (define var-reprs (map (curry dict-ref (*var-reprs*)) vars))

  ;; Expression cache
  (define exprs '())
  (define exprhash
    (make-hash
     (for/list ([var vars] [i (in-naturals)])
       (cons var i))))

  ; Counts
  (define size 0)
  (define exprc 0)
  (define varc (length vars))

  ;; Local cache of operator info
  (define cached-ops (make-hash))

  ;; Known representations
  (define bool-repr (get-representation 'bool))

  (define (munge prog repr)
    (set! size (+ 1 size))
    (define expr
      (match prog
       [(? real?) (list (const (real->precision repr prog)))]
       [(? constant?) (list (constant-info prog mode))]
       [(? variable?) prog]
       [`(if ,c ,t ,f)
        (list (operator-info 'if mode)
              (munge c bool-repr)
              (munge t repr)
              (munge f repr))]
       [(list op args ...)
        (define op-info
          (hash-ref! cached-ops op
                     (λ ()
                      (define atypes
                        (match (operator-info op 'itype)
                         [(? representation-name? a) (list #f (get-representation a))]
                         [(? list? as) (map get-representation as)]))
                      (cons (operator-info op mode) atypes))))
        (define atypes
          (if (cadr op-info) ; handle variadic operators
              (cdr op-info)
              (make-list (length args) (caddr op-info))))
        (cons (car op-info) (map munge args atypes))]
       [_ (raise-argument-error 'eval-prog "expr?" prog)]))
    (hash-ref! exprhash expr
              (λ ()
                (begin0 (+ exprc varc) ; store in cache, update exprs, exprc
                  (set! exprc (+ 1 exprc))
                  (set! exprs (cons expr exprs))))))

  (define progc (length progs))
  (define names
    (for/list ([prog progs])
      (munge (program-body prog) repr)))
  (define lt (+ exprc varc))

  (timeline-push! 'compiler (+ varc size) lt)
  (define exprvec (list->vector (reverse exprs)))
  (λ args
    (define v (make-vector lt))
    (for ([arg (in-list args)] [n (in-naturals)] [repr (in-list var-reprs)])
      (vector-set! v n (real->precision repr arg)))
    (for ([expr (in-vector exprvec)] [n (in-naturals varc)])
      (define tl
        (for/list ([arg (in-list (cdr expr))])
          (vector-ref v arg)))
      (vector-set! v n (apply (car expr) tl)))
    (for/vector ([n (in-list names)])
      (vector-ref v n))))

(module+ test
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(a b c)))
  (require math/bigfloat)
  (define tests
    #hash([(λ (a b c) (/.f64 (-.f64 (sqrt.f64 (-.f64 (*.f64 b b) (*.f64 a c))) b) a))
           . (-1.918792216976527e-259 8.469572834134629e-97 -7.41524568576933e-282)
           ])) ;(2.4174342574957107e-18 -1.4150052601637869e-40 -1.1686799408259549e+57)

  (define-simple-check (check-in-interval? iv pt)
    (match-define (ival lo hi) iv)
    (and (bf<= lo pt) (bf<= pt hi)))

  (for ([(e p) (in-hash tests)])
    (parameterize ([bf-precision 4000])
      ;; When we are in ival mode, we don't use repr, so pass in #f
      (define iv (apply (eval-prog e 'ival (get-representation 'binary64)) p))
      (define val (apply (eval-prog e 'bf (get-representation 'binary64)) p))
      (check-in-interval? iv val))))

;; This is a transcription of egg-herbie/src/math.rs, lines 97-149
(define (eval-application op . args)
  (define exact-value? (conjoin number? exact?))
  (match (cons (hash-ref parametric-operators-reverse op) args)
    [(list '+ (? exact-value? as) ...) (apply + as)]
    [(list '- (? exact-value? as) ...) (apply - as)]
    [(list '* (? exact-value? as) ...) (apply * as)]
    [(list '/ (? exact-value? num) (? exact-value? den))
     (and (not (zero? den)) (/ num den))]
    [(list 'neg (? exact-value? arg)) (- arg)]
    [(list 'pow  (? exact-value? a) (? exact-value? b))
     (cond [(and (zero? b) (not (zero? a))) 1]
           [(and (zero? a) (positive? b)) 0]
           [(and (not (zero? a)) (integer? b)) (expt a b)]
           [else #f])]
    [(list 'sqrt (? exact-value? a))
     (let ([s1 (sqrt (numerator a))] [s2 (sqrt (denominator a))])
       (and
        (real? s1) (real? s2)
        (exact? s1) (exact? s2)
        (/ s1 s2)))]
    [(list 'fabs (? exact-value? a)) (abs a)]
    [(list 'floor (? exact-value? a)) (floor a)]
    [(list 'ceil (? exact-value? a)) (ceiling a)]
    [(list 'round (? exact-value? a)) (round a)]
    ;; Added
    [(list 'log 1) 0]
    [(list 'cbrt 1) 1]
    [_ #f]))

(module+ test
  (check-equal? (eval-application '+.f64 1 1) 2)
  (check-equal? (eval-application '+.f64) 0)
  (check-equal? (eval-application '/.f64 1 0) #f) ; Not valid
  (check-equal? (eval-application 'cbrt.f64 1) 1)
  (check-equal? (eval-application 'log.f64 1) 0)
  (check-equal? (eval-application 'exp.f64 2) #f)) ; Not exact

(define/contract (replace-expression haystack needle needle*)
  (-> expr? expr? expr? expr?)
  (match haystack
   [(== needle) needle*]
   [(list (or 'lambda 'λ) (list vars ...) body)
    `(λ ,vars ,(replace-expression body needle needle*))]
   [(list op args ...)
    (cons op (map (curryr replace-expression needle needle*) args))]
   [x x]))

(module+ test
  (check-equal? (replace-expression '(λ (x) (- x (sin x))) 'x 1)
                '(λ (x) (- 1 (sin 1))))

  (check-equal?
   (replace-expression
    '(/ (cos (* 2 x)) (* (pow cos 2) (* (fabs (* sin x)) (fabs (* sin x)))))
    'cos
    '(/ 1 cos))
   '(/ (cos (* 2 x)) (* (pow (/ 1 cos) 2) (* (fabs (* sin x)) (fabs (* sin x)))))))


; Updates the repr of an expression if needed
(define (apply-repr-change-expr expr)
  (let loop ([expr expr] [prec #f])
    (match expr
     [(list (? repr-conv? op) body)
      (define iprec (first (operator-info op 'itype)))
      (define oprec (operator-info op 'otype))
      (define prec* (if prec prec oprec))
      (define body* (loop body iprec))
      (cond
       [(not body*) #f] ; propagate failed repr-change
       [else
        (define new-conv (get-repr-conv iprec prec*)) ; try to find a single conversion
        (if new-conv
            (list new-conv body*)
            (let ([second-conv (get-repr-conv oprec prec*)]) ; try a two-step conversion
              (and second-conv (list second-conv (list op body*)))))])]
     [(list (? rewrite-repr-op? rr) (list (? repr-conv? op) body))  ; repr change on a conversion
      (define iprec (first (operator-info op 'itype)))
      (define prec* (operator-info rr 'otype))
      (if (equal? prec* iprec)
          (if prec
              (loop body iprec) ; if the conversions are inverses and not the top
              (list op (loop body iprec)))
          (if prec
              (loop (list op body) prec*)
              (let* ([conv (get-repr-conv prec* (representation-name (*output-repr*)))]
                     [body* (loop body prec*)])
                (and conv body* (list conv body*)))))]
     [(list (? rewrite-repr-op? op) body)
      (define iprec (operator-info op 'otype))
      (define oprec (if prec prec (representation-name (*output-repr*))))
      (cond
       [(equal? iprec oprec)
        (loop body iprec)]
       [else
        (define conv (get-repr-conv iprec oprec))
        (define body* (loop body iprec))
        (and conv body* (list conv body*))])]
     [(list 'if con ift iff)
      (define prec* (if prec prec (representation-name (*output-repr*))))
      (define con*
        (let loop2 ([con con])
          (cond
           [(set-member? '(TRUE 'FALSE) con)
            con]
           [else
            (match-define (list op args ...) con)
            (define cprec (operator-info op 'itype))
            (cond
             [(equal? cprec 'bool)
              `(,op ,@(map loop2 args))]
             [else
              `(,op ,@(map (curryr loop cprec) args))])])))
      (define ift* (loop ift prec*))
      (define iff* (loop iff prec*))
      (and ift* iff* `(if ,con* ,ift* ,iff*))]
     [(list (? operator? op) args ...) 
      (define prec* (if prec prec (operator-info op 'otype)))
      (if (equal? (operator-info op 'otype) prec*)
          (let ([args* (map loop args (operator-info op 'itype))])
            (and (andmap identity args*) (cons op args*)))
          (let ([op* (apply get-parametric-operator 
                            (hash-ref parametric-operators-reverse op)
                            (make-list (length args) prec*))]
                [args* (map (curryr loop prec*) args)])
            (and op* (andmap identity args*) (cons op* args*))))]
     [(? variable?)
      (define var-prec (representation-name (dict-ref (*var-reprs*) expr)))
      (cond
       [(equal? var-prec prec) expr]
       [else ; insert a cast if the variable precision is not the same
        (define cast (get-repr-conv var-prec prec))
        (and cast (list cast expr))])]
     [(? (curry hash-has-key? parametric-constants-reverse))
      (define prec* (if prec prec (constant-info expr 'type)))
      (if (equal? (constant-info expr 'type) prec*) ; update constants if precision no longer matches
          expr
          (let* ([c-unparam (hash-ref parametric-constants-reverse expr expr)]
                 [c* (get-parametric-constant c-unparam prec)])
            (if c*
                c*
                (let ([conv (get-repr-conv (constant-info expr 'type) prec*)])
                  (and conv (list conv expr))))))] ; if constant does not exist in repr, add conversion
     [_ expr])))
      
(define (apply-repr-change prog)
  (match-define (list (and (or 'λ 'lambda) lam) (list args ...) body) prog)
  `(,lam ,args ,(apply-repr-change-expr body)))
