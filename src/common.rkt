#lang racket

(require racket/runtime-path math/base)
(require "config.rkt" "syntax/types.rkt")
(module+ test (require rackunit))

(provide reap
         flip-lists drop-at find-duplicates partial-sums
         argmins argmaxs set-disjoint?
         list-suffix? subsequence? list-ref* list-set*
         disjoint-set disjoint-set-find! disjoint-set-union!
         get-seed set-seed!
         quasisyntax dict sym-append
         format-time format-bits format-accuracy format-cost web-resource
         (all-from-out "config.rkt"))

;; Various syntactic forms of convenience used in Herbie

(define-syntax-rule (reap [sows ...] body ...)
  (let* ([sows (let ([store '()])
                 (cons
                  (λ () store)
                  (match-lambda*
                   [(list elt) (set! store (cons elt store))]
                   [(list) store]
                   [_ (error 'reap "invalid sow")])))] ...)
    (let ([sows (cdr sows)] ...)
      body ...)
    (values (reverse ((car sows))) ...)))

(define cast-single
  (let ([flsingle identity])
    (local-require racket/flonum)
    flsingle))

;; Utility list functions

(define (argmins f lst)
  (let loop ([lst lst] [best-score #f] [best-elts '()])
    (if (null? lst)
        (reverse best-elts)
        (let* ([elt (car lst)] [lst* (cdr lst)] [score (f elt)])
          (cond
           [(not best-score)
            (loop lst* score (list elt))]
           [(< score best-score)
            (loop lst* score (list elt))]
           [(> score best-score)
            (loop lst* best-score best-elts)]
           [(= score best-score)
            (loop lst* best-score (cons elt best-elts))])))))

(module+ test
  (check-equal? (argmins string-length '("a" "bb" "f" "ccc" "dd" "eee" "g"))
                '("a" "f" "g")))

(define (argmaxs f lst)
  (argmins (λ (x) (- (f x))) lst))

(module+ test
  (check-equal? (argmaxs string-length '("a" "bb" "f" "ccc" "dd" "eee" "g"))
                '("ccc" "eee")))

(define (drop-at ls index)
  (define-values (front back) (split-at ls index))
  (append front (rest back)))

(define (flip-lists list-list)
  "Flip a list of rows into a list of columns"
  (apply map list list-list))

(module+ test
  (check-equal? (flip-lists '((1 2 3) (4 5 6) (7 8 9)))
                '((1 4 7) (2 5 8) (3 6 9))))

(define (partial-sums vec)
  (define res (make-vector (vector-length vec)))
  (for/fold ([cur-psum 0]) ([(el idx) (in-indexed (in-vector vec))])
    (let ([new-psum (+ cur-psum el)])
      (vector-set! res idx new-psum)
      new-psum))
  res)

(module+ test
  (check-equal? (partial-sums #(1 4 6 3 8)) #(1 5 11 14 22)))

(define (find-duplicates l)
  (define found (mutable-set))
  (define duplicates '())
  (for ([x l])
    (when (set-member? found x)
      (set! duplicates (cons x duplicates)))
    (set-add! found x))
  (reverse duplicates))

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersect s2 s1)))

(module+ test
  (check-true (set-disjoint? '(a b c) '(e f g)))
  (check-true (set-disjoint? '() '()))
  (check-false (set-disjoint? '(a b c) '(a))))

(define (list-suffix? l r)
  (list-prefix? (reverse l) (reverse r)))

(module+ test
  (check-true (list-suffix? empty empty))
  (check-true (list-suffix? '(1 2) '(0 1 2)))
  (check-false (list-suffix? '(1 2 3) '(0 1 2)))
  (check-false (list-suffix? '(1 2 3) '())))

(define (subsequence? v l)
  (or
   (empty? v)
   (let ([v* (member (first v) l)])
     (and v* (subsequence? (rest v) v*)))))

(module+ test
  (define l (range 10))
  (check-true (subsequence? empty empty))
  (check-true (subsequence? empty l))
  (check-true (subsequence? '(1) l))
  (check-true (subsequence? '(1 2) l))
  (check-true (subsequence? '(1 3 5 7 9) l))
  (check-true (subsequence? '(1 2 5 8) l))
  (check-false (subsequence? '(x y) l))
  (check-false (subsequence? '(1 2 10) l)))

(define (list-ref* l p)
  (let loop ([l l] [p p] [i 0])
    (if (or (empty? p) (empty? l))
        empty
        (let* ([j (first p)]
               [k (- j i)]
               [l* (drop l k)])
          (cons (first l*) (loop (rest l*) (rest p) (+ i k 1)))))))

(module+ test
  (define m '(a b c d e f g))
  (check-equal? (list-ref* empty empty) empty)
  (check-equal? (list-ref* m empty) empty)
  (check-equal? (list-ref* m '(1)) '(b))
  (check-equal? (list-ref* m '(0 2 4 6)) '(a c e g))
  (check-equal? (list-ref* m '(0 2 3 5 6)) '(a c d f g)))

(define (list-set* l p v)
  (let loop ([l l] [p p] [v v] [i 0])
    (cond
      [(empty? l)
       empty]
      [(and (not (empty? p)) (equal? (first p) i))
       (cons (first v) (loop (rest l) (rest p) (rest v) (add1 i)))]
      [else
       (cons (first l) (loop (rest l) p v (add1 i)))])))

(module+ test
  (define n '(a b c d e f g))
  (check-equal? (list-set* empty empty empty) empty)
  (check-equal? (list-set* n empty empty) n)
  (check-equal? (list-set* n '(0) '(x)) '(x b c d e f g))
  (check-equal? (list-set* n '(1 2 5) '(x y z)) '(a x y d e z g)))

;; Union-find

(define (disjoint-set s)
  (list->vector (range s)))

(define (disjoint-set-find! d x)
  (define p (vector-ref d x))
  (if (= p x)
      x
      (let ([g (vector-ref d p)])
        (vector-set! d x g)
        (disjoint-set-find! d g))))

(define (disjoint-set-union! d x y) (vector-set! d y x))

;; Miscellaneous helper

(define the-seed #f)

(define (get-seed)
  (or the-seed (error "Seed is not set yet!")))

(define (set-seed! seed)
  "Reset the random number generator to a new seed"
  (set! the-seed seed)
  (if (vector? seed)
      (current-pseudo-random-generator
       (vector->pseudo-random-generator seed))
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
      [(_ (unsyntax pat))
       #'pat]
      [(_ (unquote pat))
       #'(app syntax-e pat)]
      [(_ (pats ...))
       (let ([parts
              (for/list ([pat (syntax-e #'(pats ...))])
                (syntax-case pat (unsyntax unquote ...)
                  [... pat]
                  [(unsyntax a) #'a]
                  [(unquote a) #'(app syntax-e a)]
                  [a #'(quasisyntax a)]))])
         #`(app syntax-e #,(datum->syntax stx (cons #'list parts))))]
      [(_ a)
       #'(app syntax-e 'a)])))

(define-match-expander dict
  (λ (stx)
    (syntax-case stx (quote)
      [(_)
       #'(? dict?)]
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
    [(or
      (eq? max-unit 'millisecond)
      (< (max ms min-unit*) 1000))
     (format "~ams" (round ms))]
    [(or
      (eq? max-unit 'second)
      (< (max ms min-unit*) 60000))
     (format "~as" (/ (round (/ ms 100.0)) 10))]
    [(or
      (eq? max-unit 'minute)
      (< (max ms min-unit*) 3600000))
     (format "~amin" (/ (round (/ ms 6000.0)) 10))]
    [else
     (format "~ahr" (/ (round (/ ms 360000.0)) 10))]))

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
   [(and (> r 0) sign) (format "+~a~a" (/ (round (* r 10)) 10) unit)]
   [else (format "~a~a" (/ (round (* r 10)) 10) unit)]))

(define (format-accuracy numerator denominator #:sign [sign #f] #:unit [unit ""])
  (if (and numerator (positive? denominator))
      (let ([percent (~r (- 100 (* (/ numerator denominator) 100)) #:precision '(= 1))])
        (if (and (> numerator 0) sign)
            (format "+~a~a" percent unit)
            (format "~a~a" percent unit)))
      ""))

(define (format-cost r repr #:sign [sign #f])  
  (cond 
    [(not r) ""]
    [else
      (define val (~r (/ (round (* r 10)) 10) #:precision 2))
      (cond
      [(and (> r 0) sign) (format "+~a" val)]
      [else (format "~a" val)])]))

(define-runtime-path web-resource-path "web/resources/")

(define (web-resource [name #f])
  (if name
      (build-path web-resource-path name)
      web-resource-path))

(define (sym-append . args)
  (string->symbol (apply string-append (map ~a args))))

