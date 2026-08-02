#lang racket

(require "../utils/errors.rkt"
         "platform.rkt"
         (only-in "platform-language.rkt"
                  create-operator-impl!
                  platform-register-implementation!
                  platform-register-representation!)
         "types.rkt")

(provide tuple-impl-name
         tuple-ref-impl-name
         ensure-tuple-impls!
         ensure-tuple-ref-impl!)

(define (impl-registered? name)
  (hash-has-key? (platform-implementations (*active-platform*)) name))

(define (repr-name->token name)
  (match name
    [(? symbol?) (symbol->string name)]
    [`(array ,elem ,len) (format "array<~a:~a>" (repr-name->token elem) len)]
    [`(tuple ,slots ...) (format "tuple<~a>" (string-join (map repr-name->token slots) ":"))]
    [_ (raise-herbie-error "Cannot name representation ~a" name)]))

(define (tuple-token repr)
  (repr-name->token (representation-name repr)))

(define (tuple-impl-name repr)
  (string->symbol (tuple-token repr)))

(define (tuple-ref-impl-name repr idx)
  (string->symbol (format "ref.~a.~a" idx (tuple-token repr))))

(define (ensure-tuple-representation! repr)
  (define pform (*active-platform*))
  (unless (hash-has-key? (platform-representations pform) (representation-name repr))
    (platform-register-representation! pform
                                       #:repr repr
                                       #:cost
                                       (for/sum ([slot (in-list (tuple-representation-slots repr))])
                                                (platform-repr-cost pform slot)))))

(define (ensure-tuple-constructor! repr)
  (define name (tuple-impl-name repr))
  (unless (impl-registered? name)
    (define slots (tuple-representation-slots repr))
    (define vars
      (for/list ([i (in-range (length slots))])
        (string->symbol (format "x~a" i))))
    (define spec `(tuple ,@vars))
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

(define (ensure-tuple-accessor! repr idx)
  (define name (tuple-ref-impl-name repr idx))
  (unless (impl-registered? name)
    (define slots (tuple-representation-slots repr))
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

(define (ensure-tuple-impls! repr)
  (define ctor-name (tuple-impl-name repr))
  (cond
    [(impl-registered? ctor-name) ctor-name]
    [else
     (for ([slot (in-list (tuple-representation-slots repr))]
           #:when (tuple-representation? slot))
       (ensure-tuple-impls! slot))
     (ensure-tuple-representation! repr)
     (ensure-tuple-constructor! repr)
     (for ([idx (in-range (length (tuple-representation-slots repr)))])
       (ensure-tuple-accessor! repr idx))
     ctor-name]))

(define (ensure-tuple-ref-impl! repr idx)
  (ensure-tuple-impls! repr)
  (tuple-ref-impl-name repr idx))

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
    [(wrapped? "tuple")
     (define names (slots-of "tuple"))
     (and names `(tuple ,@names))]
    [(wrapped? "array")
     (define names (slots-of "array"))
     (and names
          (= (length names) 2)
          (exact-positive-integer? (string->number (second (split-slots (body "array")))))
          `(array ,(first names) ,(string->number (second (split-slots (body "array"))))))]
    [(zero? (string-length tok)) #f]
    [(or (string-contains? tok "<") (string-contains? tok ">") (string-contains? tok ":")) #f]
    [else (string->symbol tok)]))

;; Symbols that cannot name a tuple impl, so a repeated miss costs one eq lookup.
(define non-tuple-names (make-weak-hasheq))

;; Splits a name into its tuple token and, for an accessor, its slot index.
(define (tuple-impl-name-parts name)
  (cond
    [(not (symbol? name)) (values #f #f)]
    [(hash-ref non-tuple-names name #f) (values #f #f)]
    [else
     (define str (symbol->string name))
     (define-values (tok idx)
       (cond
         [(string-prefix? str "tuple<") (values str #f)]
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
       [(and tok (string-prefix? tok "tuple<")) (values tok idx)]
       [else
        (hash-set! non-tuple-names name #t)
        (values #f #f)])]))

;; Resolves a name to the shape it denotes, or #f. Never mutates the platform.
(define (tuple-impl-name->repr name)
  (define-values (tok idx) (tuple-impl-name-parts name))
  (define repr-name (and tok (token->repr-name tok)))
  (define repr
    (and repr-name
         (repr-exists? repr-name)
         (let ([r (get-representation repr-name)])
           (and (tuple-representation? r)
                (for/and ([slot (in-list (tuple-representation-slots r))])
                  (not (equal? (representation-type slot) 'bool)))
                r))))
  (cond
    [(not repr) (values #f #f)]
    [(not idx) (values repr #f)]
    [(< idx (length (tuple-representation-slots repr))) (values repr idx)]
    [else (values #f #f)]))

;; Impl names encode their whole shape, so a missing tuple impl can be rebuilt
;; from its name alone.
(define (tuple-impl-name-resolvable? name)
  (define-values (repr _idx) (tuple-impl-name->repr name))
  (and repr #t))

(define (synthesize-tuple-impl-from-name name)
  (define-values (repr idx) (tuple-impl-name->repr name))
  (and repr
       (begin
         (ensure-tuple-impls! repr)
         #t)))

(set-tuple-impl-resolvable?! tuple-impl-name-resolvable?)
(set-tuple-impl-synthesizer! synthesize-tuple-impl-from-name)
