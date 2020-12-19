#lang racket

(require math/number-theory)
(require "../common.rkt")
(require "../function-definitions.rkt")
(require "../programs.rkt")
(require "reduce.rkt")

(provide approximate)

(define (approximate expr var #:transform [tform #f] #:iters [iters 5])
  (define expr* (simplify (replace-expression expr var ((car tform) var))))
  (match-define (cons offset coeffs) (taylor var expr*))

  (debug #:from 'approximate "Taking taylor expansion of" expr* "in" var "around" 0)
  
  (define i 0)
  (define terms '())

  (define (next [iter 0])
    (define coeff (simplify (coeffs i)))
    (set! i (+ i 1))
    (match coeff
     [0
      (if (< iter iters)
          (next (+ iter 1))
          (simplify (make-sum (reverse terms))))]
     [_
      (define term `(* ,coeff ,(make-monomial var (- i offset 1))))
      (define term* (simplify (replace-expression term var ((cdr tform) var))))
      (set! terms (cons term* terms))
      (simplify (make-sum (reverse terms)))]))

  next)

(define (make-sum terms)
  (match terms
   ['() 0]
   [`(,x) x]
   [`(,x ,xs ...)
    `(+ ,x ,(make-sum xs))]))

(define (make-prod terms)
  (match terms
   ['() 1]
   [`(,x) x]
   [`(,x ,xs ...)
    `(* ,x ,(make-prod xs))]))

(define (make-monomial var power)
  (cond
   [(equal? power 0)   1]
   [(equal? power 1)   var]
   [(equal? power -1) `(/ 1 ,var)]
   [(positive? power) `(pow ,var ,power)]
   [(negative? power) `(pow ,var ,power)]))

(define (make-term head vars expts)
  ; We do not want to output something like (* (sqr x) (sqr y)) -- we'd prefer (sqr (* x y))
  ; So we first extract the GCD of the exponents and put that exponentiation outside
  (let ([outside-expt (apply gcd expts)])
    (if (zero? outside-expt)
        head ; Only happens if expts has only zeros
        `(* ,head
            ,(make-monomial
              (make-prod
               (map make-monomial vars (map (curryr / outside-expt) expts)))
              outside-expt)))))

(define n-sum-to-cache (make-hash))

(define (n-sum-to n k)
  (hash-ref! n-sum-to-cache (cons n k)
             (λ ()
                (cond
                 [(= k 0) (list (build-list n (const 0)))]
                 [(= n 1) (list (list k))]
                 [(= n 0) '()]
                 [else
                  (apply append
                         (for/list ([i (in-range 0 (+ k 1))])
                           (map (curry cons i) (n-sum-to (- n 1) (- k i)))))]))))

(define (iterate-diagonal dim i)
  (let loop ([i i] [sum 0])
    (let ([seg (n-sum-to dim sum)])
      (if ((length seg) . <= . i)
          (loop (- i (length seg)) (+ sum 1))
          (list-ref seg i)))))

(define taylor-expansion-known
  '(+ - neg * / sqrt cbrt exp sin cos log pow))

(register-reset
 (λ ()
  (hash-clear! n-sum-to-cache)
  (hash-clear! logcache)
  (hash-set! logcache 1 '((1 -1 1)))))

(define (taylor var expr*)
  "Return a pair (e, n), such that expr ~= e var^n"
  (define expr
    (if (and (list? expr*) (not (set-member? taylor-expansion-known (car expr*))))
        ((get-expander taylor-expansion-known) expr*)
        expr*))
  (unless (equal? expr expr*)
    (debug #:from 'taylor "Rewrote expression to" expr))
  (match expr
    [(? (curry equal? var))
     (taylor-exact 0 1)]
    [(? constant?)
     (taylor-exact expr)]
    [(? variable?)
     (taylor-exact expr)]
    [`(+ ,args ...)
     (apply taylor-add (map (curry taylor var) args))]
    [`(neg ,arg)
     (taylor-negate ((curry taylor var) arg))]
    [`(- ,arg1 ,arg2)
     (taylor-add (taylor var arg1) (taylor-negate (taylor var arg2)))]
    [`(* ,left ,right)
     (taylor-mult (taylor var left) (taylor var right))]
    [`(/ 1 ,arg)
     (taylor-invert (taylor var arg))]
    [`(/ ,num ,den)
     (taylor-quotient (taylor var num) (taylor var den))]
    [`(sqrt ,arg)
     (taylor-sqrt (taylor var arg))]
    [`(cbrt ,arg)
     (taylor-cbrt (taylor var arg))]
    [`(exp ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (if (positive? (car arg*))
           (taylor-exact expr)
           (taylor-exp (zero-series arg*))))]
    [`(sin ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (cond
        [(positive? (car arg*))
         (taylor-exact expr)]
        [(= (car arg*) 0)
         ; Our taylor-sin function assumes that a0 is 0,
         ; because that way it is especially simple. We correct for this here
         ; We use the identity sin (x + y) = sin x cos y + cos x sin y
         (taylor-add
          (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0))) (taylor-cos (zero-series arg*)))
          (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0))) (taylor-sin (zero-series arg*))))]
        [else
         (taylor-sin (zero-series arg*))]))]
    [`(cos ,arg)
     (let ([arg* (normalize-series (taylor var arg))])
       (cond
        [(positive? (car arg*))
         (taylor-exact expr)]
        [(= (car arg*) 0)
         ; Our taylor-cos function assumes that a0 is 0,
         ; because that way it is especially simple. We correct for this here
         ; We use the identity cos (x + y) = cos x cos y - sin x sin y
         (taylor-add
          (taylor-mult (taylor-exact `(cos ,((cdr arg*) 0))) (taylor-cos (zero-series arg*)))
          (taylor-negate
           (taylor-mult (taylor-exact `(sin ,((cdr arg*) 0))) (taylor-sin (zero-series arg*)))))]
        [else
         (taylor-cos (zero-series arg*))]))]
    [`(log ,arg)
     (let* ([arg* (normalize-series (taylor var arg))]
            [rest (taylor-log (cdr arg*))])
       (if (zero? (car arg*))
           rest
           (cons 0
                 (λ (n)
                    (if (= n 0)
                        (simplify `(+ (* (neg ,(car arg*)) (log ,var))
                                      ,((cdr rest) 0)))
                        ((cdr rest) n))))))]
    [`(pow ,base ,(? exact-integer? power))
     (taylor-pow (normalize-series (taylor var base)) power)]
    [`(pow ,base ,power)
     (taylor var `(exp (* ,power (log ,base))))]
    [_
     (taylor-exact expr)]))

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

(define (taylor-exact . terms)
  (define items (list->vector (map simplify terms)))
  (cons 0
        (λ (n)
           (if (<= (length terms) n)
               0
               (vector-ref items n)))))

(define (first-nonzero-exp f)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (and (equal? (f n) 0) (< n 20))
        (loop (+ n 1))
        n)))

(define (align-series . serieses)
  (if (or (<= (length serieses) 1) (apply = (map car serieses)))
      serieses
      (let ([offset* (car (argmax car serieses))])
        (for/list ([series serieses])
          (let ([offset (car series)])
            (cons offset* (λ (n)
                             (if (< (+ n (- offset offset*)) 0)
                                 0
                                 ((cdr series) (+ n (- offset offset*)))))))))))

(define (taylor-add . terms)
  (match (apply align-series terms)
    [`((,offset . ,serieses) ...)
     (let ([hash (make-hash)])
       (cons (car offset)
             (λ (n)
               (hash-ref! hash n
                          (λ () (simplify (make-sum (for/list ([series serieses]) (series n)))))))))]))

(define (taylor-negate term)
  (cons (car term) (λ (n) (simplify (list 'neg ((cdr term) n))))))

(define (taylor-mult left right)
  (cons (+ (car left) (car right))
        (let ([hash (make-hash)])
          (lambda (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          (make-sum
                           (for/list ([i (range (+ n 1))])
                             (list '* ((cdr left) i) ((cdr right) (- n i))))))))))))

(define (normalize-series series)
  "Fixes up the series to have a non-zero zeroth term,
   allowing a possibly negative offset"
  (match series
    [(cons offset coeffs)
     (let ([slack (first-nonzero-exp coeffs)])
       (cons (- offset slack) (compose coeffs (curry + slack))))]))

(define ((zero-series series) n)
  (if (< n (- (car series))) 0 ((cdr series) (+ n (car series)))))

(define (taylor-invert term)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (normalize-series term)
    [(cons offset b)
     (let ([hash (make-hash)])
       (hash-set! hash 0 (simplify `(/ 1 ,(b 0))))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     `(neg (+ ,@(for/list ([i (range n)])
                                                `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))])
         (cons (- offset) f)))]))

(define (taylor-quotient num denom)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match-define (cons noff a) (normalize-series num))
  (match-define (cons doff b) (normalize-series denom))
  (define hash (make-hash))
  (hash-set! hash 0 (simplify `(/ ,(a 0) ,(b 0))))
  (define (f n)
    (hash-ref! hash n
               (λ ()
                 (simplify
                  `(- (/ ,(a n) ,(b 0))
                      (+ ,@(for/list ([i (range n)])
                             `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))
  (cons (- noff doff) f))

(define (taylor-sqrt num)
  (let* ([num* (normalize-series num)]
         [offset (car num*)]
         [offset* (if (even? offset) offset (+ offset 1))]
         [coeffs (cdr num*)]
         [coeffs* (if (even? offset) coeffs (λ (n) (if (= n 0) 0 (coeffs (- n 1)))))]
         [hash (make-hash)])
    (hash-set! hash 0 (simplify `(sqrt ,(coeffs* 0))))
    (hash-set! hash 1 (simplify `(/ ,(coeffs* 1) (* 2 (sqrt ,(coeffs* 0))))))
    (letrec ([f (λ (n)
                   (hash-ref! hash n
                              (λ ()
                                 (simplify
                                  (cond
                                   [(even? n)
                                    `(/ (- ,(coeffs* n) (pow ,(f (/ n 2)) 2)
                                           (+ ,@(for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                                  `(* 2 (* ,(f k) ,(f (- n k)))))))
                                        (* 2 ,(f 0)))]
                                   [(odd? n)
                                    `(/ (- ,(coeffs* n)
                                           (+ ,@(for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                                  `(* 2 (* ,(f k) ,(f (- n k)))))))
                                        (* 2 ,(f 0)))])))))])
      (cons (/ offset* 2) f))))

(define (taylor-cbrt num)
  (let* ([num* (normalize-series num)]
         [offset (car num*)]
         [offset* (- offset (modulo offset 3))]
         [coeffs (cdr num*)]
         [coeffs* (if (= (modulo offset 3) 0) coeffs (λ (n) (if (= n 0) 0 (coeffs (+ n (modulo offset 3))))))]
         [hash (make-hash)])
    (hash-set! hash 0 (simplify `(cbrt ,(coeffs* 0))))
    (hash-set! hash 1 (simplify `(/ ,(coeffs* 1) (* 3 (cbrt ,(coeffs* 0))))))
    (letrec ([f (λ (n)
                   (hash-ref! hash n
                              (λ ()
                                 (simplify
                                  `(/ (- ,(coeffs* n)
                                         (+ ,@(for/list ([j (in-range 1 n)] [k (in-range 1 n)] #:when (<= (+ j k) n))
                                                `(* 2 (* ,(f j) ,(f k) ,(f (- n j k)))))))
                                      (* 3 ,(f 0)))))))])
      (cons (/ offset* 3) f))))

(define (rle l)
  (for/list ([run (group-by identity l)])
    (cons (length run) (car run))))

(define (taylor-exp coeffs)
  (let* ([hash (make-hash)])
    (hash-set! hash 0 (simplify `(exp ,(coeffs 0))))
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          `(* (exp ,(coeffs 0))
                              (+
                               ,@(for/list ([p (map rle (all-partitions n))])
                                   `(*
                                     ,@(for/list ([(count num) (in-dict p)])
                                         `(/ (pow ,(coeffs num) ,count)
                                             ,(factorial count))))))))))))))

(define (taylor-sin coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 0)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          `(+
                            ,@(for/list ([p (map rle (all-partitions n))])
                                (if (= (modulo (apply + (map car p)) 2) 1)
                                    `(* ,(if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                                        ,@(for/list ([(count num) (in-dict p)])
                                            `(/ (pow ,(coeffs num) ,count)
                                                ,(factorial count))))
                                    0))))))))))

(define (taylor-pow coeffs n)
  (match n ;; Russian peasant multiplication
    [(? negative?) (taylor-pow (taylor-invert coeffs) (- n))]
    [0 (taylor-exact 1)]
    [1 coeffs]
    [(? even?)
     (define half (taylor-pow coeffs (/ n 2)))
     (taylor-mult half half)]
    [(? odd?)
     (define half (taylor-pow coeffs (/ (- n 1) 2)))
     (taylor-mult coeffs (taylor-mult half half))]))

(define (taylor-cos coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 1)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (simplify
                          `(+
                            ,@(for/list ([p (map rle (all-partitions n))])
                                (if (= (modulo (apply + (map car p)) 2) 0)
                                    `(* ,(if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                                        ,@(for/list ([(count num) (in-dict p)])
                                            `(/ (pow ,(coeffs num) ,count)
                                                ,(factorial count))))
                                    0))))))))))

;; This is a hyper-specialized symbolic differentiator for log(f(x))

(define initial-logtable '((1 -1 1)))

(define (list-setinc l i)
  (let loop ([i i] [l l] [rest '()])
    (if (= i 0)
        (if (null? (cdr l))
            (append (reverse rest) (list (- (car l) 1) 1))
            (append (reverse rest) (list* (- (car l) 1) (+ (cadr l) 1) (cddr l))))
        (loop (- i 1) (cdr l) (cons (car l) rest)))))

(define (loggenerate table)
  (apply append
         (for/list ([term table])
           (match term
             [`(,coeff ,ps ...)
              (filter identity
                      (for/list ([i (in-naturals)] [p ps])
                        (if (zero? p)
                            #f
                            `(,(* coeff p) ,@(list-setinc ps i)))))]))))

(define (lognormalize table)
  (filter (λ (entry) (not (= (car entry) 0)))
          (for/list ([entry (group-by cdr table)])
            (cons (apply + (map car entry))
                  (cdar entry)))))

(define (logstep table)
  (lognormalize (loggenerate table)))

(define logcache (make-hash (list (cons 1 '((1 -1 1))))))
(define logbiggest 1)

(define (logcompute i)
  (hash-ref! logcache i
             (λ ()
                (logstep (logcompute (- i 1))))))

(define (taylor-log coeffs)
  "coeffs is assumed to start with a nonzero term"
  (let ([hash (make-hash)])
    (hash-set! hash 0 (simplify `(log ,(coeffs 0))))
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (let* ([tmpl (logcompute n)])
                           (simplify
                            `(/
                              (+ ,@(for/list ([term tmpl])
                                     (match term
                                       [`(,coeff ,k ,ps ...)
                                        `(* ,coeff (/ (* ,@(for/list ([i (in-naturals 1)] [p ps])
                                                             (if (= p 0)
                                                                 1
                                                                 `(pow (* ,(factorial i) ,(coeffs i)) ,p))))
                                                      (pow ,(coeffs 0) ,(- k))))])))
                              ,(factorial n))))))))))

(module+ test
  (require rackunit)
  (check-pred exact-integer? (car (taylor 'x '(pow x 1.0)))))
