#lang racket

(require math/number-theory)
(require "../utils/common.rkt"
         "../syntax/syntax.rkt"
         "batch.rkt"
         "dvector.rkt"
         "programs.rkt")

(provide approximate
         taylor-coefficients
         reduce
         add)

(define reduce (make-parameter #f))
(define add (make-parameter #f))

(define (reducer x)
  ((reduce) x))

(define (adder x)
  ((add) x))

(define (taylor-coefficients batch brfs vars transforms-to-try)
  (define expander (expand-taylor! batch))
  (define taylor-coeffs
    (for*/list ([var (in-list vars)]
                #:do [(define taylorer (taylor var batch))]
                [transform-type transforms-to-try])
      (match-define (list name f finv) transform-type)
      (define replacer (batch-replace-expression! batch var (f var)))
      (for/list ([brf (in-list brfs)])
        (taylorer (expander (reducer (replacer brf)))))))
  taylor-coeffs)

(define (approximate taylor-approxs
                     batch
                     var
                     #:transform [tform (cons identity identity)]
                     #:iters [iters 5])
  (define replacer (batch-replace-expression! batch var ((cdr tform) var)))
  (for/list ([ta taylor-approxs])
    (match-define (cons offset coeffs) ta)
    (define i 0)
    (define terms '())

    (define (next [iter 0])
      (define coeff (reducer (replacer (coeffs i))))
      (set! i (+ i 1))
      (match (deref coeff)
        [0
         (if (< iter iters)
             (next (+ iter 1))
             (reducer (make-horner ((cdr tform) var) (reverse terms))))]
        [_
         (set! terms (cons (cons coeff (- i offset 1)) terms))
         (reducer (make-horner ((cdr tform) var) (reverse terms)))]))
    next))

;; Our Taylor expander prefers sin, cos, exp, log, neg over trig, htrig, pow, and subtraction
(define (expand-taylor! input-batch)
  (batch-apply!
   input-batch
   (lambda (node)
     (match node
       [(list '- ref1 ref2) `(+ ,ref1 (neg ,ref2))]
       [(list 'pow base (app deref 1/2)) `(sqrt ,base)]
       [(list 'pow base (app deref 1/3)) `(cbrt ,base)]
       [(list 'pow base (app deref 2/3)) `(cbrt (* ,base ,base))]
       [(list 'pow base power)
        #:when (exact-integer? (deref power))
        `(pow ,base ,power)]
       [(list 'pow base power) `(exp (* ,power (log ,base)))]
       [(list 'tan arg) `(/ (sin ,arg) (cos ,arg))]
       [(list 'cosh arg) `(* 1/2 (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'sinh arg) `(* 1/2 (+ (exp ,arg) (/ -1 (exp ,arg))))]
       [(list 'tanh arg) `(/ (+ (exp ,arg) (neg (/ 1 (exp ,arg)))) (+ (exp ,arg) (/ 1 (exp ,arg))))]
       [(list 'asinh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) 1))))]
       [(list 'acosh arg) `(log (+ ,arg (sqrt (+ (* ,arg ,arg) -1))))]
       [(list 'atanh arg) `(* 1/2 (log (/ (+ 1 ,arg) (+ 1 (neg ,arg)))))]
       [_ node]))))

; Tests for expand-taylor
(module+ test
  (require rackunit)

  (define (test-expand-taylor expr)
    (define-values (batch brfs) (progs->batch (list expr)))
    (define brfs* (map (expand-taylor! batch) brfs))
    (car (batch->progs batch brfs*)))

  (check-equal? '(* 1/2 (log (/ (+ 1 x) (+ 1 (neg x))))) (test-expand-taylor '(atanh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) -1)))) (test-expand-taylor '(acosh x)))
  (check-equal? '(log (+ x (sqrt (+ (* x x) 1)))) (test-expand-taylor '(asinh x)))
  (check-equal? '(/ (+ (exp x) (neg (/ 1 (exp x)))) (+ (exp x) (/ 1 (exp x))))
                (test-expand-taylor '(tanh x)))
  (check-equal? '(* 1/2 (+ (exp x) (/ -1 (exp x)))) (test-expand-taylor '(sinh x)))
  (check-equal? '(+ 1 (neg (+ 2 (neg 3)))) (test-expand-taylor '(- 1 (- 2 3))))
  (check-equal? '(* 1/2 (+ (exp x) (/ 1 (exp x)))) (test-expand-taylor '(cosh x)))
  (check-equal? '(/ (sin x) (cos x)) (test-expand-taylor '(tan x)))
  (check-equal? '(+ 1 (neg (* 1/2 (+ (exp (/ (sin 3) (cos 3))) (/ 1 (exp (/ (sin 3) (cos 3))))))))
                (test-expand-taylor '(- 1 (cosh (tan 3)))))
  (check-equal? '(exp (* a (log x))) (test-expand-taylor '(pow x a)))
  (check-equal? '(+ x (sin a)) (test-expand-taylor '(+ x (sin a))))
  (check-equal? '(cbrt x) (test-expand-taylor '(pow x 1/3)))
  (check-equal? '(cbrt (* x x)) (test-expand-taylor '(pow x 2/3)))
  (check-equal? '(+ 100 (cbrt x)) (test-expand-taylor '(+ 100 (pow x 1/3))))
  (check-equal? `(+ 100 (cbrt (* x y))) (test-expand-taylor `(+ 100 (pow (* x y) 1/3))))
  (check-equal? `(+ y (cbrt x)) (test-expand-taylor `(+ y (pow x 1/3))))
  (check-equal? `(+ (cbrt x) y) (test-expand-taylor `(+ (pow x 1/3) y))))

(define (make-horner var terms [start 0])
  (match terms
    ['() (adder 0)]
    [(list (cons c n)) (adder `(* ,c ,(make-monomial var (- n start))))]
    [(list (cons c n) rest ...)
     (adder `(* ,(make-monomial var (- n start)) (+ ,c ,(make-horner var rest n))))]))

(define (make-sum terms)
  (match terms
    ['() 0]
    [`(,x) x]
    [`(,x ,xs ...) `(+ ,x ,(make-sum xs))]))

(define (make-monomial var power)
  (cond
    [(equal? power 0) 1]
    [(equal? power 1) var]
    [(equal? power -1) `(/ 1 ,var)]
    [(positive? power) `(pow ,var ,power)]
    [(negative? power) `(pow ,var ,power)]))

(define/reset n-sum-to-cache (make-hash))

(define/reset log-cache (make-hash '((1 . ((1 -1 1))))))

(define (n-sum-to n k)
  (hash-ref! (n-sum-to-cache)
             (cons n k)
             (λ ()
               (cond
                 [(= k 0) (list (make-list n 0))]
                 [(= n 1) (list (list k))]
                 [(= n 0) '()]
                 [else
                  (for*/list ([i (in-range 0 (+ k 1))]
                              [v (in-list (map (curry cons i) (n-sum-to (- n 1) (- k i))))])
                    v)]))))

(define (taylor var expr-batch)
  "Return a pair (e, n), such that expr ~= e var^n"
  (batch-recurse
   expr-batch
   (lambda (brf recurse)
     (define node (deref brf))
     (match node
       [(? (curry equal? var)) (taylor-exact (adder 0) (adder 1))]
       [(? number?) (taylor-exact brf)]
       [(? symbol?) (taylor-exact brf)]
       [`(,const) (taylor-exact brf)]
       [`(+ ,left ,right) (taylor-add (recurse left) (recurse right))]
       [`(neg ,arg) (taylor-negate (recurse arg))]
       [`(* ,left ,right) (taylor-mult (recurse left) (recurse right))]
       [`(/ ,num ,den)
        #:when (equal? (deref num) 1)
        (taylor-invert (recurse den))]
       [`(/ ,num ,den) (taylor-quotient (recurse num) (recurse den))]
       [`(sqrt ,arg) (taylor-sqrt var (recurse arg))]
       [`(cbrt ,arg) (taylor-cbrt var (recurse arg))]
       [`(exp ,arg)
        (define arg* (normalize-series (recurse arg)))
        (match-define (cons arg-offset _) arg*)
        (if (positive? arg-offset)
            (taylor-exact brf)
            (taylor-exp (zero-series arg*)))]
       [`(sin ,arg)
        (define arg* (normalize-series (recurse arg)))
        (match-define (cons arg-offset arg-coeffs) arg*)
        (cond
          [(positive? arg-offset) (taylor-exact brf)]
          [(= arg-offset 0)
           ; Our taylor-sin function assumes that a0 is 0,
           ; because that way it is especially simple. We correct for this here
           ; We use the identity sin (x + y) = sin x cos y + cos x sin y
           (taylor-add (taylor-mult (taylor-exact (adder `(sin ,(arg-coeffs 0))))
                                    (taylor-cos (zero-series arg*)))
                       (taylor-mult (taylor-exact (adder `(cos ,(arg-coeffs 0))))
                                    (taylor-sin (zero-series arg*))))]
          [else (taylor-sin (zero-series arg*))])]
       [`(cos ,arg)
        (define arg* (normalize-series (recurse arg)))
        (match-define (cons arg-offset arg-coeffs) arg*)
        (cond
          [(positive? arg-offset) (taylor-exact brf)]
          [(= arg-offset 0)
           ; Our taylor-cos function assumes that a0 is 0,
           ; because that way it is especially simple. We correct for this here
           ; We use the identity cos (x + y) = cos x cos y - sin x sin y
           (taylor-add (taylor-mult (taylor-exact (adder `(cos ,(arg-coeffs 0))))
                                    (taylor-cos (zero-series arg*)))
                       (taylor-negate (taylor-mult (taylor-exact (adder `(sin ,(arg-coeffs 0))))
                                                   (taylor-sin (zero-series arg*)))))]
          [else (taylor-cos (zero-series arg*))])]
       [`(log ,arg) (taylor-log var (recurse arg))]
       [`(pow ,base ,power)
        #:when (exact-integer? (deref power))
        (taylor-pow (normalize-series (recurse base)) (deref power))]
       [_ (taylor-exact brf)]))))

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

; (define term? (cons/c number? (-> number? batchref?)))

(define (taylor-exact . terms)
  ;(->* () #:rest (listof batchref?) term?)
  (define items (list->vector (map reducer terms)))
  (cons 0
        (λ (n)
          (if (<= (length terms) n)
              (adder 0)
              (vector-ref items n)))))

(define (first-nonzero-exp f)
  ;(-> (-> number? batchref?) number?)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (and (equal? (deref (f n)) 0) (< n 20))
        (loop (+ n 1))
        n)))

(define (align-series left right)
  ;(-> term? term? (values number? (-> number? batchref?) (-> number? batchref?)))
  (match-define (cons left-offset left-coeffs) left)
  (match-define (cons right-offset right-coeffs) right)
  (define target-offset (max left-offset right-offset))
  (define (shift offset coeffs)
    (define shift-offset (- offset target-offset))
    (λ (n)
      (define index (+ n shift-offset))
      (if (negative? index)
          (adder 0)
          (coeffs index))))
  (values target-offset (shift left-offset left-coeffs) (shift right-offset right-coeffs)))

(define-syntax-rule (make-series n offset cache n* body ...)
  (cons offset
        (λ (n)
          (when (>= n (dvector-length cache))
            (for ([n* (in-range (dvector-length cache) (add1 n))])
              (let ([value (reducer (adder (begin
                                             body ...)))])
                (dvector-set! cache n* value))))
          (dvector-ref cache n))))

(define (taylor-add left right)
  ;(-> term? term? term?)
  (define-values (offset left-coeffs right-coeffs) (align-series left right))
  (define cache (make-dvector 10))
  (make-series n offset cache n* `(+ ,(left-coeffs n*) ,(right-coeffs n*))))

(define (taylor-negate term)
  ;(-> term? term?)
  (match-define (cons offset coeffs) term)
  (define cache (make-dvector 10))
  (make-series n offset cache n* (list 'neg (coeffs n*))))

(define (taylor-mult left right)
  ;(-> term? term? term?)
  (match-define (cons left-offset left-coeffs) left)
  (match-define (cons right-offset right-coeffs) right)
  (define cache (make-dvector 10))
  (make-series n
               (+ left-offset right-offset)
               cache
               n*
               (make-sum (for/list ([i (range (+ n* 1))])
                           (list '* (left-coeffs i) (right-coeffs (- n* i)))))))

(define (normalize-series series)
  ;(-> term? term?)
  "Fixes up the series to have a non-zero zeroth term,
   allowing a possibly negative offset"
  (match-define (cons offset coeffs) series)
  (define slack (first-nonzero-exp coeffs))
  (cons (- offset slack) (compose coeffs (curry + slack))))

(define ((zero-series series) n)
  ;(-> (cons/c number? (-> number? batchref?)) (-> number? batchref?))
  (match-define (cons offset coeffs) series)
  (if (< n (- offset))
      (adder 0)
      (coeffs (+ n offset))))

(define (taylor-invert term)
  ;(-> term? term?)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match-define (cons offset b) (normalize-series term))
  (define cache (make-dvector 10))

  (make-series n
               (- offset)
               cache
               n*
               (if (zero? n*)
                   `(/ 1 ,(b 0))
                   `(neg (+ ,@(for/list ([i (range n*)])
                                `(* ,(dvector-ref cache i) (/ ,(b (- n* i)) ,(b 0)))))))))

(define (taylor-quotient num denom)
  ;(-> term? term? term?)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match-define (cons noff a) (normalize-series num))
  (match-define (cons doff b) (normalize-series denom))
  (define cache (make-dvector 10))
  (make-series n
               (- noff doff)
               cache
               n*
               (if (zero? n*)
                   `(/ ,(a 0) ,(b 0))
                   `(- (/ ,(a n*) ,(b 0))
                       (+ ,@(for/list ([i (range n*)])
                              `(* ,(dvector-ref cache i) (/ ,(b (- n* i)) ,(b 0)))))))))

(define (modulo-series var n series)
  ;(-> symbol? number? term? term?)
  (match-define (cons offset coeffs) (normalize-series series))
  (define offset* (+ offset (modulo (- offset) n)))
  (define cache (make-dvector 2)) ;; never called mor than twice
  (define (coeffs* i)
    (unless (and (> (dvector-capacity cache) i) (dvector-ref cache i))
      (define res
        (match i
          [0
           (adder (make-sum (for/list ([j (in-range (modulo offset n))])
                              `(* ,(coeffs j) (pow ,var ,(+ j (modulo (- offset) n)))))))]
          [_
           #:when (< i n)
           (adder 0)]
          [_ (coeffs (+ (- i n) (modulo offset n)))]))
      (dvector-set! cache i res))
    (dvector-ref cache i))
  (cons offset* (if (= offset offset*) coeffs coeffs*)))

(define (taylor-sqrt var num)
  ;(-> symbol? term? term?)
  (match-define (cons offset* coeffs*) (modulo-series var 2 num))
  (define cache (make-dvector 10))
  (make-series n
               (/ offset* 2)
               cache
               n*
               (cond
                 [(zero? n*) `(sqrt ,(coeffs* 0))]
                 [(= n* 1) `(/ ,(coeffs* 1) (* 2 (sqrt ,(coeffs* 0))))]
                 [(even? n*)
                  `(/ (- ,(coeffs* n*)
                         (pow ,(dvector-ref cache (/ n* 2)) 2)
                         (+ ,@(for/list ([k (in-naturals 1)]
                                         #:break (>= k (- n* k)))
                                `(* 2 (* ,(dvector-ref cache k) ,(dvector-ref cache (- n* k)))))))
                      (* 2 ,(dvector-ref cache 0)))]
                 [(odd? n*)
                  `(/ (- ,(coeffs* n*)
                         (+ ,@(for/list ([k (in-naturals 1)]
                                         #:break (>= k (- n* k)))
                                `(* 2 (* ,(dvector-ref cache k) ,(dvector-ref cache (- n* k)))))))
                      (* 2 ,(dvector-ref cache 0)))])))

(define (taylor-cbrt var num)
  ;(-> symbol? term? term?)
  (match-define (cons offset* coeffs*) (modulo-series var 3 num))
  (define cache (make-dvector 10))
  (make-series
   n
   (/ offset* 3)
   cache
   n*
   (cond
     [(zero? n*) `(cbrt ,(coeffs* 0))]
     [(= n* 1) `(/ ,(coeffs* 1) (* 3 (cbrt (* ,(dvector-ref cache 0) ,(dvector-ref cache 0)))))]
     [else
      `(/ (- ,(coeffs* n*)
             ,@(for*/list ([terms (n-sum-to 3 n*)]
                           #:unless (set-member? terms n*))
                 (match-define (list a b c) terms)
                 `(* ,(dvector-ref cache a) ,(dvector-ref cache b) ,(dvector-ref cache c))))
          (* 3 ,(dvector-ref cache 0) ,(dvector-ref cache 0)))])))

(define (taylor-pow coeffs n)
  ;(-> term? number? term?)
  (match n ;; Russian peasant multiplication
    [(? negative?) (taylor-pow (taylor-invert coeffs) (- n))]
    [0 (taylor-exact (adder 1))]
    [1 coeffs]
    [(? even?)
     (define half (taylor-pow coeffs (/ n 2)))
     (taylor-mult half half)]
    [(? odd?)
     (define half (taylor-pow coeffs (/ (- n 1) 2)))
     (taylor-mult coeffs (taylor-mult half half))]))

(define (all-partitions n options)
  (match options
    ['()
     (if (= n 0)
         '(())
         '())]
    [(cons k options*)
     (reap [sow]
           (for* ([i (in-range (/ (+ n 1) k))])
             (define head (cons i k))
             (if (= i 0)
                 (map sow (all-partitions n options*))
                 (for ([pt (all-partitions (- n (* k i)) options*)])
                   (sow (cons head pt))))))]))

(define (taylor-exp coeffs)
  ;(-> (-> number? batchref?) term?)
  (define cache (make-dvector 10))
  (make-series n
               0
               cache
               n*
               (if (zero? n)
                   `(exp ,(coeffs 0))
                   (let* ([coeffs* (list->vector (map coeffs (range 1 (+ n 1))))]
                          [nums (for/list ([i (in-range 1 (+ n 1))]
                                           [coeff (in-vector coeffs*)]
                                           #:unless (equal? (deref coeff) 0))
                                  i)])
                     `(* (exp ,(coeffs 0))
                         (+ ,@(for/list ([p (all-partitions n (sort nums >))])
                                `(* ,@(for/list ([(count num) (in-dict p)])
                                        `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                            ,(factorial count)))))))))))

(define (taylor-sin coeffs)
  ;(-> (-> number? batchref?) term?)
  (define cache (make-dvector 10))
  (make-series n
               0
               cache
               n*
               (if (zero? n*)
                   0
                   (let* ([coeffs* (list->vector (map coeffs (range 1 (+ n* 1))))]
                          [nums (for/list ([i (in-range 1 (+ n* 1))]
                                           [coeff (in-vector coeffs*)]
                                           #:unless (equal? (deref coeff) 0))
                                  i)])
                     `(+ ,@(for/list ([p (all-partitions n* (sort nums >))])
                             (if (= (modulo (apply + (map car p)) 2) 1)
                                 `(* ,(if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                                     ,@(for/list ([(count num) (in-dict p)])
                                         `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                             ,(factorial count))))
                                 0)))))))

(define (taylor-cos coeffs)
  ;(-> (-> number? batchref?) term?)
  (define cache (make-dvector 10))
  (make-series n
               0
               cache
               n*
               (if (zero? n*)
                   1
                   (let* ([coeffs* (list->vector (map coeffs (range 1 (+ n* 1))))]
                          [nums (for/list ([i (in-range 1 (+ n* 1))]
                                           [coeff (in-vector coeffs*)]
                                           #:unless (equal? (deref coeff) 0))
                                  i)])
                     `(+ ,@(for/list ([p (all-partitions n* (sort nums >))])
                             (if (= (modulo (apply + (map car p)) 2) 0)
                                 `(* ,(if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                                     ,@(for/list ([(count num) (in-dict p)])
                                         `(/ (pow ,(vector-ref coeffs* (- num 1)) ,count)
                                             ,(factorial count))))
                                 0)))))))

;; This is a hyper-specialized symbolic differentiator for log(f(x))

(define (list-setinc l i)
  (let loop ([i i]
             [l l]
             [rest '()])
    (if (= i 0)
        (if (null? (cdr l))
            (append (reverse rest) (list (- (car l) 1) 1))
            (append (reverse rest) (list* (- (car l) 1) (+ (cadr l) 1) (cddr l))))
        (loop (- i 1) (cdr l) (cons (car l) rest)))))

(define (loggenerate table)
  (apply append
         (for/list ([term table])
           (match-define `(,coeff ,ps ...) term)
           (filter identity
                   (for/list ([i (in-naturals)]
                              [p ps])
                     (if (zero? p)
                         #f
                         `(,(* coeff p) ,@(list-setinc ps i))))))))

(define (lognormalize table)
  (filter (λ (entry) (not (= (car entry) 0)))
          (for/list ([entry (group-by cdr table)])
            (cons (apply + (map car entry)) (cdar entry)))))

(define (logstep table)
  (lognormalize (loggenerate table)))

(define (logcompute i)
  (hash-ref! (log-cache) i (λ () (logstep (logcompute (- i 1))))))

(define (taylor-log var arg)
  ;(-> symbol? term? term?)
  (match-define (cons shift coeffs) (normalize-series arg))
  (define negate? (and (number? (deref (coeffs 0))) (not (positive? (deref (coeffs 0))))))
  (define (maybe-negate x)
    (if negate?
        `(neg ,x)
        x))

  (define series-cache (make-dvector 10 #f))
  (dvector-set! series-cache 0 (reducer (adder `(log ,(maybe-negate (coeffs 0))))))

  (define (series n)
    (unless (and (> (dvector-capacity series-cache) n) (dvector-ref series-cache n))
      (define tmpl (logcompute n))
      (define res
        (reducer (adder `(/ (+ ,@(for/list ([term tmpl])
                                   (match-define `(,coeff ,k ,ps ...) term)
                                   `(* ,coeff
                                       (/ (* ,@(for/list ([i (in-naturals 1)]
                                                          [p ps])
                                                 (if (= p 0)
                                                     1
                                                     `(pow (* ,(factorial i) ,(coeffs i)) ,p))))
                                          (exp (* ,(- k) ,(series 0)))))))
                            ,(factorial n)))))
      (dvector-set! series-cache n res))
    (dvector-ref series-cache n))

  (cons 0
        (λ (n)
          (if (and (= n 0) (not (zero? shift)))
              (reducer (adder `(+ (* (neg ,shift) (log ,(maybe-negate var))) ,(series 0))))
              (series n)))))

(module+ test
  (require rackunit)
  (define-values (batch brfs) (progs->batch (list '(pow x 1.0))))
  (parameterize ([reduce (batch-reduce batch)]
                 [add (λ (x) (batch-add! batch x))])
    (define brfs* (map (expand-taylor! batch) brfs))
    (define brf (car brfs*))
    (check-pred exact-integer? (car ((taylor 'x batch) brf)))))

(module+ test
  (require "batch-reduce.rkt")
  (define (coeffs expr #:n [n 7])
    (define-values (batch brfs) (progs->batch (list expr)))
    (parameterize ([reduce (batch-reduce batch)]
                   [add (λ (x) (batch-add! batch x))])
      (define brfs* (map (expand-taylor! batch) brfs))
      (define brf (car brfs*))
      (match-define fn (zero-series ((taylor 'x batch) brf)))
      (map batch-pull (build-list n fn))))
  (check-equal? (coeffs '(sin x)) '(0 1 0 -1/6 0 1/120 0))
  (check-equal? (coeffs '(sqrt (+ 1 x))) '(1 1/2 -1/8 1/16 -5/128 7/256 -21/1024))
  (check-equal? (coeffs '(cbrt (+ 1 x))) '(1 1/3 -1/9 5/81 -10/243 22/729 -154/6561))
  (check-equal? (coeffs '(sqrt x)) '((sqrt x) 0 0 0 0 0 0))
  (check-equal? (coeffs '(cbrt x)) '((cbrt x) 0 0 0 0 0 0))
  (check-equal? (coeffs '(cbrt (* x x))) '((pow x 2/3) 0 0 0 0 0 0)))
