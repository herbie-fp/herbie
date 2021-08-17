#lang racket

(require math/bigfloat rival)
(require "syntax/types.rkt" "syntax/syntax.rkt" "float.rkt" "interface.rkt"
         "timeline.rkt")
(module+ test (require rackunit "load-plugin.rkt"))

(provide (all-from-out "syntax/syntax.rkt")
         program-body program-variables
         expr? expr-supports? expr-contains?
         type-of repr-of
         location-do location-get
         batch-eval-progs eval-prog eval-application
         free-variables replace-expression replace-vars
         apply-repr-change)

(define expr? (or/c list? symbol? value? real?))

;; Programs are just lambda expressions

(define/contract (program-body prog)
  (-> expr? expr?)
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  body)

(define/contract (program-variables prog)
  (-> expr? (listof symbol?))
  (match-define (list (or 'lambda 'λ 'FPCore) (list vars ...) body) prog)
  vars)

;; Returns type name
;; Fast version does not recurse into functions applications
;; TODO(interface): Once we update the syntax checker to FPCore 1.1
;; standards, this will have to have more information passed in
(define (type-of expr repr env)
  (match expr
   [(? number?) (get-type 'real)]
   [(? (representation-repr? repr)) (representation-type repr)]
   [(? variable?) (representation-type (dict-ref env expr))]
   [(list 'if cond ift iff) (type-of ift repr env)]
   [(list op args ...) ; repr-name -> repr -> type
    (representation-type (get-representation (operator-info op 'otype)))]))

;; Returns repr name
;; Fast version does not recurse into functions applications
(define (repr-of expr repr env)
  (match expr
   [(? number?) repr]
   [(? (representation-repr? repr)) repr]
   [(? variable?) (dict-ref env expr)]
   [(list 'if cond ift iff) (repr-of ift repr env)]
   [(list op args ...) (get-representation (operator-info op 'otype))]))

(define (expr-supports? expr field)
  (let loop ([expr expr])
    (match expr
      [(list 'if cond ift iff)
       (and (loop cond) (loop ift) (loop iff))]   ; if is special-cased and always supported
      [(list op args ...)
       (and (operator-info op field) (andmap loop args))]
      [(? variable?) true]
      [(? number?) true])))

(define (expr-contains? expr pred)
  (let loop ([expr expr])
    (match expr
     [(list elems ...) (ormap loop elems)]
     [term (pred term)])))

;; Converting constants

(define (free-variables prog)
  (match prog
    [(? number?) '()]
    [(? variable?) (list prog)]
    [`(,op ,args ...)
     (remove-duplicates (append-map free-variables args))]))

(define (replace-vars dict expr)
  (cond
    [(dict-has-key? dict expr) (dict-ref dict expr)]
    [(list? expr)
     (cons (replace-vars dict (car expr)) (map (curry replace-vars dict) (cdr expr)))]
    [#t expr]))

(define location? (listof natural-number/c))

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

(define/contract (location-get loc prog)
  (-> location? expr? expr?)
  ; Clever continuation usage to early-return
  (let/ec return
    (location-do loc prog return)))

(define (eval-prog prog mode repr)
  (define f (batch-eval-progs (list prog) mode repr))
  (λ args (vector-ref (apply f args) 0)))

(define (batch-eval-progs progs mode repr)
  (define real->precision
    (match mode
     ['fl (λ (x repr) (real->repr x repr))]
     ['bf (λ (x repr) (bf x))]
     ['ival (λ (x repr) (mk-ival (bf x)))]))

  (define arg->precision
    (match mode
     ['fl (λ (x repr) x)]
     ['bf (λ (x repr) (if (bigfloat? x) x ((representation-repr->bf repr) x)))]
     ['ival (λ (x repr) (if (ival? x) x (mk-ival ((representation-repr->bf repr) x))))]))

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

  ;; 'if' operator
  (define if-op
    (match mode
     [(or 'fl 'bf) (λ (c ift iff) (if c ift iff))]
     ['ival ival-if]))

  (define (munge prog repr)
    (set! size (+ 1 size))
    (define expr
      (match prog
       [(? number?) (list (const (real->precision prog repr)))]
       [(? variable?) prog]
       [`(if ,c ,t ,f)
        (list if-op
              (munge c bool-repr)
              (munge t repr)
              (munge f repr))]
       [(list op args ...)
        (match-define (list* fn variary? itypes)
          (hash-ref! cached-ops op
                     (λ ()
                      (define atypes
                        (match (operator-info op 'itype)
                         [(? representation-name? a)
                          (cons #t (get-representation a))]
                         [(? list? as)
                          (cons #f (map get-representation as))]))
                      (cons (operator-info op mode) atypes))))
        (define atypes
          (if variary? ; handle variadic operators
              (make-list (length args) itypes)
              itypes))
        (cons fn (map munge args atypes))]
       [_ (raise-argument-error 'eval-prog "Not a valid expression!" prog)]))
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
      (vector-set! v n (arg->precision arg repr)))
    (for ([expr (in-vector exprvec)] [n (in-naturals varc)])
      (define tl
        (for/list ([arg (in-list (cdr expr))])
          (vector-ref v arg)))
      (vector-set! v n (apply (car expr) tl)))
    (for/vector ([n (in-list names)])
      (vector-ref v n))))

(module+ test
  (*var-reprs* (map (curryr cons (get-representation 'binary64)) '(a b c)))
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
  (match (cons op args)
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
  (check-equal? (eval-application '+ 1 1) 2)
  (check-equal? (eval-application '+) 0)
  (check-equal? (eval-application '/ 1 0) #f) ; Not valid
  (check-equal? (eval-application 'cbrt 1) 1)
  (check-equal? (eval-application 'log 1) 0)
  (check-equal? (eval-application 'exp 2) #f)) ; Not exact

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
                            (make-list (length args) prec*)
                            #:fail-fast? #f)]
                [args* (map (curryr loop prec*) args)])
            (and op* (andmap identity args*) (cons op* args*))))]
     [(? variable?)
      (define var-prec (representation-name (dict-ref (*var-reprs*) expr)))
      (cond
       [(equal? var-prec prec) expr]
       [else ; insert a cast if the variable precision is not the same
        (define cast (get-repr-conv var-prec prec))
        (and cast (list cast expr))])]
     [_ expr])))

(define (apply-repr-change prog)
  (match prog
   [(list 'FPCore (list vars ...) body) `(FPCore ,vars ,(apply-repr-change-expr body))]
   [(list (or 'λ 'lambda) (list vars ...) body) `(λ ,vars ,(apply-repr-change-expr body))]
   [_ (apply-repr-change-expr prog)]))
