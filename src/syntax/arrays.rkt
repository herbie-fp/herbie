#lang racket

(require "../utils/errors.rkt"
         "platform.rkt"
         (only-in "platform-language.rkt"
                  create-operator-impl!
                  platform-register-implementation!
                  platform-register-representation!)
         "types.rkt")

(provide array-impl-name
         array-ref-impl-name
         ensure-array-impls!
         ensure-array-ref-impl!)

(define (impl-registered? name)
  (hash-has-key? (platform-implementations (*active-platform*)) name))

(define (repr-name->token name)
  (match name
    [(? symbol?) (symbol->string name)]
    [`(array ,slots ...) (format "array<~a>" (string-join (map repr-name->token slots) ":"))]
    [_ (raise-herbie-error "Cannot name representation ~a" name)]))

(define (array-token repr)
  (repr-name->token (representation-name repr)))

(define (array-impl-name repr)
  (string->symbol (array-token repr)))

(define (array-ref-impl-name repr idx)
  (string->symbol (format "ref.~a.~a" idx (array-token repr))))

(define (ensure-array-representation! repr)
  (define pform (*active-platform*))
  (unless (hash-has-key? (platform-representations pform) (representation-name repr))
    (platform-register-representation! pform
                                       #:repr repr
                                       #:cost
                                       (for/sum ([slot (in-list (array-representation-slots repr))])
                                                (platform-repr-cost pform slot)))))

(define (ensure-array-constructor! repr)
  (define name (array-impl-name repr))
  (unless (impl-registered? name)
    (define slots (array-representation-slots repr))
    (define vars
      (for/list ([i (in-range (length slots))])
        (string->symbol (format "x~a" i))))
    (define spec `(array ,@vars))
    (define cost (for/sum ([slot (in-list slots)]) (platform-repr-cost (*active-platform*) slot)))
    (platform-register-implementation!
     (*active-platform*)
     (create-operator-impl! name
                            (context vars repr slots)
                            #:spec spec
                            #:impl (procedure-reduce-arity (lambda args (list->vector args))
                                                           (length vars))
                            #:fpcore spec
                            #:cost cost)))
  name)

(define (ensure-array-accessor! repr idx)
  (define name (array-ref-impl-name repr idx))
  (unless (impl-registered? name)
    (define slots (array-representation-slots repr))
    (define spec `(ref t ,idx))
    (platform-register-implementation!
     (*active-platform*)
     (create-operator-impl! name
                            (context '(t) (list-ref slots idx) (list repr))
                            #:spec spec
                            #:impl (lambda (v) (vector-ref v idx))
                            #:fpcore spec
                            #:cost (platform-repr-cost (*active-platform*) (list-ref slots idx)))))
  name)

(define (ensure-array-impls! repr)
  (define ctor-name (array-impl-name repr))
  (cond
    [(impl-registered? ctor-name) ctor-name]
    [else
     (for ([slot (in-list (array-representation-slots repr))]
           #:when (array-representation? slot))
       (ensure-array-impls! slot))
     (ensure-array-representation! repr)
     (ensure-array-constructor! repr)
     (for ([idx (in-range (length (array-representation-slots repr)))])
       (ensure-array-accessor! repr idx))
     ctor-name]))

(define (ensure-array-ref-impl! repr idx)
  (ensure-array-impls! repr)
  (array-ref-impl-name repr idx))

;; Inverse of `repr-name->token`; #f when the token is not a well-formed name.
(define (token->repr-name tok)
  (define (split-slots body)
    (let loop ([i 0]
               [depth 0]
               [start 0]
               [out '()])
      (cond
        [(= i (string-length body)) (and (zero? depth) (reverse (cons (substring body start i) out)))]
        [else
         (define c (string-ref body i))
         (cond
           [(char=? c #\<) (loop (add1 i) (add1 depth) start out)]
           [(char=? c #\>) (and (positive? depth) (loop (add1 i) (sub1 depth) start out))]
           [(and (char=? c #\:) (zero? depth))
            (loop (add1 i) depth (add1 i) (cons (substring body start i) out))]
           [else (loop (add1 i) depth start out)])])))
  (define (wrapped? prefix)
    (and (string-prefix? tok (string-append prefix "<")) (string-suffix? tok ">")))
  (define (body prefix)
    (substring tok (add1 (string-length prefix)) (sub1 (string-length tok))))
  (define (slots-of prefix)
    (define parts (split-slots (body prefix)))
    (and parts
         (pair? parts)
         (not (member "" parts))
         (let ([names (map token->repr-name parts)]) (and (andmap values names) names))))
  (cond
    [(wrapped? "array")
     (define names (slots-of "array"))
     (and names `(array ,@names))]
    [(zero? (string-length tok)) #f]
    [(or (string-contains? tok "<") (string-contains? tok ">") (string-contains? tok ":")) #f]
    [else (string->symbol tok)]))

;; Symbols that cannot name an array impl, so a repeated miss costs one eq lookup.
(define non-array-names (make-weak-hasheq))

;; Splits a name into its array token and, for an accessor, its slot index.
(define (array-impl-name-parts name)
  (cond
    [(not (symbol? name)) (values #f #f)]
    [(hash-ref non-array-names name #f) (values #f #f)]
    [else
     (define str (symbol->string name))
     (define-values (tok idx)
       (cond
         [(string-prefix? str "array<") (values str #f)]
         [(string-prefix? str "ref.")
          (define rest (substring str 4))
          (define dot
            (for/first ([i (in-range (string-length rest))]
                        #:when (char=? (string-ref rest i) #\.))
              i))
          (define n (and dot (string->number (substring rest 0 dot))))
          (if (and dot (exact-nonnegative-integer? n))
              (values (substring rest (add1 dot)) n)
              (values #f #f))]
         [else (values #f #f)]))
     (cond
       [(and tok (string-prefix? tok "array<")) (values tok idx)]
       [else
        (hash-set! non-array-names name #t)
        (values #f #f)])]))

;; Resolves a name to the shape it denotes, or #f. Never mutates the platform.
(define (array-impl-name->repr name)
  (define-values (tok idx) (array-impl-name-parts name))
  (define repr-name (and tok (token->repr-name tok)))
  (define repr
    (and repr-name
         (repr-exists? repr-name)
         (let ([r (get-representation repr-name)])
           (and (array-representation? r)
                (for/and ([slot (in-list (array-representation-slots r))])
                  (not (equal? (representation-type slot) 'bool)))
                r))))
  (cond
    [(not repr) (values #f #f)]
    [(not idx) (values repr #f)]
    [(< idx (length (array-representation-slots repr))) (values repr idx)]
    [else (values #f #f)]))

;; Impl names encode their whole shape, so a missing array impl can be rebuilt
;; from its name alone.
(define (array-impl-name-resolvable? name)
  (define-values (repr _idx) (array-impl-name->repr name))
  (and repr #t))

(define (synthesize-array-impl-from-name name)
  (define-values (repr idx) (array-impl-name->repr name))
  (and repr
       (begin
         (ensure-array-impls! repr)
         #t)))

(set-array-impl-resolvable?! array-impl-name-resolvable?)
(set-array-impl-synthesizer! synthesize-array-impl-from-name)
