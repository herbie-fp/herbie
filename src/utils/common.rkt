#lang racket

(require racket/runtime-path
         math/base
         "../config.rkt")

(provide reap
         flip-lists
         drop-at
         find-duplicates
         partial-sums
         disjoint-set
         disjoint-set-find!
         disjoint-set-union!
         get-seed
         set-seed!
         quasisyntax
         dict
         sym-append
         format-time
         format-bits
         format-accuracy
         format-cost
         web-resource
         prop-dict/c
         props->dict
         dict->props
         (all-from-out "../config.rkt"))

(module+ test
  (require rackunit))

;; Various syntactic forms of convenience used in Herbie

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
                 (cons (λ () store)
                       (match-lambda*
                         [(list elt) (set! store (cons elt store))]
                         [(list) store]
                         [_ (error 'reap "invalid sow")])))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

(define (drop-at ls index)
  (define-values (front back) (split-at ls index))
  (append front (rest back)))

(define (flip-lists list-list)
  "Flip a list of rows into a list of columns"
  (apply map list list-list))

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9))))

(define (partial-sums vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (define new-psum (+ cur-psum el))
    (vector-set! res idx new-psum)
    new-psum)
  res)

(module+ test
  (check-equal? (partial-sums #(1 4 6 3 8)) #(1 5 11 14 22)))

(define (find-duplicates l)
  (map car (filter (compose pair? rest) (group-by identity l))))

;; Union-find

(define (disjoint-set s)
  (list->vector (range s)))

(define (disjoint-set-find! d x)
  (define p (vector-ref d x))
  (cond
    [(= p x) x]
    [else
     (define r (disjoint-set-find! d p))
     (vector-set! d x r)
     r]))

(define (disjoint-set-union! d x y)
  (vector-set! d y x))

;; Miscellaneous helper

(define the-seed #f)

(define (get-seed)
  (or the-seed (error "Seed is not set yet!")))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (set! the-seed seed)
  (if (vector? seed)
      (current-pseudo-random-generator (vector->pseudo-random-generator seed))
      (random-seed seed)))

;; Matching support for syntax objects.

;; Begin the match with a #`
;; Think of the #` as just like a ` match, same behavior
;; In fact, matching x with #`pat should do the same
;; as matching (syntax->datum x) with `pat
;; Inside the #`, you can use #, to bind not a value but a syntax object.

(define-match-expander quasisyntax
  (λ (stx)
    (syntax-case stx (unsyntax unquote)
      [(_ (unsyntax pat)) #'pat]
      [(_ (unquote pat)) #'(app syntax-e pat)]
      [(_ (pats ...))
       (let ([parts (for/list ([pat (syntax-e #'(pats ...))])
                      (syntax-case pat (unsyntax unquote ...)
                        [... pat]
                        [(unsyntax a) #'a]
                        [(unquote a) #'(app syntax-e a)]
                        [a #'(quasisyntax a)]))])
         #`(app syntax-e #,(datum->syntax stx (cons #'list parts))))]
      [(_ a) #'(app syntax-e 'a)])))

(define-match-expander dict
  (λ (stx)
    (syntax-case stx (quote)
      [(_) #'(? dict?)]
      [(dict 'x y rest ...)
       #'(and (dict rest ...) (? (curryr dict-has-key? 'x)) (app (curryr dict-ref 'x) y))])))

;; String formatting operations

(define (format-time ms #:min [min-unit 'millisecond] #:max [max-unit 'hour])
  (define min-unit*
    (match min-unit
      ['millisecond 0]
      ['second 1000]
      ['minute 60000]
      ['hour 3600000]))
  (cond
    [(or (eq? max-unit 'millisecond) (< (max ms min-unit*) 1000)) (format "~ams" (round ms))]
    [(or (eq? max-unit 'second) (< (max ms min-unit*) 60000))
     (format "~as" (/ (round (/ ms 100.0)) 10))]
    [(or (eq? max-unit 'minute) (< (max ms min-unit*) 3600000))
     (format "~amin" (/ (round (/ ms 6000.0)) 10))]
    [else (format "~ahr" (/ (round (/ ms 360000.0)) 10))]))

(module+ test
  (check-equal? (format-time 60000) "1.0min")
  (check-equal? (format-time 3600000) "1.0hr")
  (check-equal? (format-time 60000 #:min 'second) "1.0min")
  (check-equal? (format-time 60000 #:max 'millisecond) "60000ms")
  (check-equal? (format-time 500 #:min 'second #:max 'minute) "0.5s")
  (check-equal? (format-time 2000 #:min 'second #:max 'minute) "2.0s")
  (check-equal? (format-time 60000 #:min 'second #:max 'minute) "1.0min")
  (check-equal? (format-time 3600000 #:min 'second #:max 'minute) "60.0min")
  (check-equal? (format-time 7200000 #:min 'second #:max 'minute) "120.0min")
  (check-equal? (format-time 0 #:min 'hour) "0hr")
  (check-equal? (format-time 1800000 #:min 'hour) "0.5hr")
  (check-equal? (format-time 7200000 #:max 'millisecond) "7200000ms"))

(define (format-bits r #:sign [sign #f] #:unit [unit? #f])
  (define unit (if unit? "b" ""))
  (cond
    [(not r) ""]
    [(and (positive? r) sign) (format "+~a~a" (/ (round (* r 10)) 10) unit)]
    [else (format "~a~a" (/ (round (* r 10)) 10) unit)]))

(define (format-accuracy numerator denominator #:sign [sign #f] #:unit [unit ""])
  (cond
    [(and numerator (positive? denominator))
     (define percent (~r (- 100 (* (/ numerator denominator) 100)) #:precision '(= 1)))
     (if (and (positive? numerator) sign)
         (format "+~a~a" percent unit)
         (format "~a~a" percent unit))]
    [else ""]))

(define (format-cost r repr #:sign [sign #f])
  (cond
    [(not r) ""]
    [else
     (define val (~r (/ (round (* r 10)) 10) #:precision 2))
     (cond
       [(and (positive? r) sign) (format "+~a" val)]
       [else (format "~a" val)])]))

(define-runtime-path web-resource-path "../reports/resources/")

(define (web-resource [name #f])
  (if name
      (build-path web-resource-path name)
      web-resource-path))

;; Symbol generation

(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))

;; FPCore properties

(define prop-dict/c (listof (cons/c symbol? any/c)))

;; Prop list to dict
(define (props->dict props)
  (let loop ([props props]
             [dict '()])
    (match props
      [(list key val rest ...) (loop rest (dict-set dict key val))]
      [(list key) (error 'props->dict "unmatched key" key)]
      [(list) dict])))

(define (dict->props prop-dict)
  (apply append
         (for/list ([(k v) (in-dict prop-dict)])
           (list k v))))
