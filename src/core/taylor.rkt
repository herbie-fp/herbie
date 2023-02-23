#lang racket

(require math/number-theory)
(require "../common.rkt" "../programs.rkt" "reduce.rkt" "../syntax/syntax.rkt")
(provide approximate)

(define (approximate expr var #:transform [tform (cons identity identity)] #:iters [iters 5])
  (define expr* (simplify (replace-expression expr var ((car tform) var))))
  (match-define (cons offset coeffs) (taylor var expr*))

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
(define logcache (make-hash '((1 . ((1 -1 1))))))
(define series-cache (make-hash))

(register-reset
 (λ ()
  (set! n-sum-to-cache (make-hash))
  (set! logcache (make-hash '((1 . ((1 -1 1))))))
  (set! series-cache (make-hash))))

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

(define (taylor var expr)
  (define var-cache (hash-ref! series-cache var (λ () (make-hash))))
  (hash-ref! var-cache expr (λ () (taylor* var expr))))

(define (taylor* var expr)
  "Return a pair (e, n), such that expr ~= e var^n"
  (match expr
    [(? (curry equal? var))
     (taylor-exact 0 1)]
    [(? number?)
     (taylor-exact expr)]
    [(? variable?)
     (taylor-exact expr)]
    [`(,const)
     (taylor-exact expr)]
    [`(+ ,args ...)
     (apply taylor-add (map (curry taylor var) args))]
    [`(neg ,arg)
     (taylor-negate ((curry taylor var) arg))]
    [`(- ,arg1 ,arg2)
     (taylor var `(+ ,arg1 (neg ,arg2)))]
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
    [`(tan ,arg)
     (taylor var `(/ (sin ,arg) (cos ,arg)))]
    [`(log ,arg)
     (taylor-log var (taylor var arg))]
    [`(pow ,base ,(? exact-integer? power))
     (taylor-pow (normalize-series (taylor var base)) power)]
    [`(pow ,base 1/2)
     (taylor-sqrt (taylor var base))]
    [`(pow ,base 1/3)
     (taylor-cbrt (taylor var base))]
    [`(pow ,base ,power)
     (taylor var `(exp (* ,power (log ,base))))]
    [`(expm1 ,arg) (taylor var `(- (exp ,arg) 1))]
    [`(log1p ,arg) (taylor var `(log (+ 1 ,arg)))]
    [`(hypot ,a ,b)
     (define ta (taylor var a))
     (define tb (taylor var b))
     (taylor-sqrt (taylor-add (taylor-mult ta ta) (taylor-mult tb tb)))]
    [`(fma ,a ,b ,c) (taylor var `(+ (* ,a ,b) ,c))]
    [`(sinh ,arg)
     (define exparg (taylor var `(exp ,arg)))
     (taylor-mult (taylor-exact 1/2) (taylor-add exparg (taylor-negate (taylor-invert exparg))))]
    [`(cosh ,arg)
     (define exparg (taylor var `(exp ,arg)))
     (taylor-mult (taylor-exact 1/2) (taylor-add exparg (taylor-invert exparg)))]
    [`(tanh ,arg)
     (define exparg (taylor var `(exp ,arg)))
     (define expinv (taylor-invert exparg))
     (define x+ (taylor-add exparg expinv))
     (define x- (taylor-add exparg (taylor-negate expinv)))
     (taylor-quotient x- x+)]
    [`(asinh ,x)
     (define tx (taylor var x))
     (taylor-log var (taylor-add tx (taylor-sqrt (taylor-add (taylor-mult tx tx) (taylor-exact 1)))))]
    [`(acosh ,x)
     (define tx (taylor var x))
     (taylor-log var (taylor-add tx (taylor-sqrt (taylor-add (taylor-mult tx tx) (taylor-exact -1)))))]
    [`(atanh ,x)
     (define tx (taylor var x))
     (taylor-mult (taylor-exact 1/2)
                  (taylor-log var (taylor-quotient (taylor-add (taylor-exact 1) tx)
                                               (taylor-add (taylor-exact 1) (taylor-negate tx)))))]
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
         [f0 (simplify `(cbrt ,(coeffs* 0)))]
         [hash (make-hash)])
    (hash-set! hash 0 f0)
    (hash-set! hash 1 (simplify `(/ ,(coeffs* 1) (* 3 (cbrt (* ,f0 ,f0))))))
    (letrec ([f (λ (n)
                   (hash-ref! hash n
                              (λ ()
                                 (simplify
                                  `(/ (- ,(coeffs* n)
                                         ,@(for*/list ([terms (n-sum-to 3 n)]
                                                       #:unless (set-member? terms n))
                                             (match-define (list a b c) terms)
                                             `(* ,(f a) ,(f b) ,(f c))))
                                      (* 3 ,f0 ,f0))))))])
      (cons (/ offset* 3) f))))

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

(define (all-partitions n options)
  (match options
    ['() (if (= n 0) '(()) '())]
    [(cons k options*)
     (reap [sow]
       (for* ([i (in-range (/ (+ n 1) k))])
         (define head (cons i k))
         (if (= i 0)
             (map sow (all-partitions n options*))
             (for ([pt (all-partitions (- n (* k i)) options*)])
               (sow (cons head pt))))))]))

(define (taylor-exp coeffs)
  (let* ([hash (make-hash)])
    (hash-set! hash 0 (simplify `(exp ,(coeffs 0))))
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))] [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify
                          `(* (exp ,(coeffs 0))
                              (+
                               ,@(for/list ([p (all-partitions n (sort nums >))])
                                   `(*
                                     ,@(for/list ([(count num) (in-dict p)])
                                         `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                             ,(factorial count))))))))))))))

(define (taylor-sin coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 0)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))] [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify
                          `(+
                            ,@(for/list ([p (all-partitions n (sort nums >))])
                                (if (= (modulo (apply + (map car p)) 2) 1)
                                    `(* ,(if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                                        ,@(for/list ([(count num) (in-dict p)])
                                            `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                                ,(factorial count))))
                                    0))))))))))

(define (taylor-cos coeffs)
  (let ([hash (make-hash)])
    (hash-set! hash 0 1)
    (cons 0
          (λ (n)
            (hash-ref! hash n
                       (λ ()
                         (define coeffs* (list->vector (map coeffs (range 1 (+ n 1)))))
                         (define nums
                           (for/list ([i (in-range 1 (+ n 1))] [coeff (in-vector coeffs*)]
                                      #:unless (equal? coeff 0))
                             i))
                         (simplify
                          `(+
                            ,@(for/list ([p (all-partitions n (sort nums >))])
                                (if (= (modulo (apply + (map car p)) 2) 0)
                                    `(* ,(if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                                        ,@(for/list ([(count num) (in-dict p)])
                                            `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
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

(define logbiggest 1)

(define (logcompute i)
  (hash-ref! logcache i
             (λ ()
                (logstep (logcompute (- i 1))))))

(define (taylor-log var arg)
  (match-define (cons shift coeffs) (normalize-series arg))
  (define hash (make-hash))
  (hash-set! hash 0 (simplify `(log ,(coeffs 0))))

  (define (series n)
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
                      ,(factorial n)))))))

  (cons 0 (λ (n)
            (if (and (= n 0) (not (zero? shift)))
                (simplify `(+ (* (neg ,shift) (log ,var)) ,(series 0)))
                (series n)))))

(module+ test
  (require rackunit "../syntax/types.rkt" "../load-plugin.rkt")
  (check-pred exact-integer? (car (taylor 'x '(pow x 1.0)))))

(module+ test
  (define (coeffs expr #:n [n 7])
    (match-define fn (zero-series (taylor 'x expr)))
    (build-list n fn))

  (check-equal? (coeffs '(sin x)) '(0 1 0 -1/6 0 1/120 0))
  (check-equal? (coeffs '(cbrt (+ 1 x))) '(1 1/3 -1/9 5/81 -10/243 22/729 -154/6561))
  )
