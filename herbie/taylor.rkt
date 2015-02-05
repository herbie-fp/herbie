#lang racket

(require math/number-theory)
(require "common.rkt")
(require "programs.rkt")
(require "matcher.rkt")
(require "simplify/backup-simplify.rkt")
(require (rename-in "simplify/simplify.rkt"
		    [simplify simplify-alt]))

(provide approximate-0 approximate-inf)

(define (approximate-0 expr vars #:terms [terms 3] #:iters [iters 5]) ; TODO : constant
  (debug #:from 'approximate "Taking taylor expansion of" expr "in" vars "around" 0)

  (define taylor-cache (make-hash))

  (hash-set! taylor-cache '() (zero-series (taylor-0 (car vars) expr)))

  (define (take-taylor coeffs)
    (hash-ref! taylor-cache coeffs
               (λ ()
                  (let* ([oc (take-taylor (cdr coeffs))]
                         [expr* (oc (car coeffs))])
                    (if (= (length coeffs) (length vars))
                      expr*
                      (zero-series (taylor-0 (list-ref vars (length coeffs)) expr*)))))))

  (define (make-term coeffs)
    (simplify
     `(* ,(take-taylor coeffs)
         ,(let loop ([vars (reverse vars)] [coeffs coeffs])
            (if (null? vars)
                1
                (let ([var (car vars)]
                      [idx (car coeffs)])
                  `(* ,(make-monomial var idx) ,(loop (cdr vars) (cdr coeffs)))))))))

  (simplify
   (cons '+
         (let loop ([i 0] [res '()])
           (if (or (> i (* iters (length vars))) (>= (length res) terms))
               res
               (let ([coeffs (iterate-diagonal (length vars) i)])
                 (if (not (equal? (take-taylor coeffs) 0))
                     (loop (+ i 1) (cons (make-term coeffs) res))
                     (loop (+ i 1) res))))))))

; TODO: BUG: if variables appear in coefficients (rare...), this is incorrect
(define (approximate-inf expr vars #:terms [terms 3] #:iters [iters 5]) ; TODO : constant
  (debug #:from 'approximate "Taking taylor expansion of" expr "in" vars "around infinity")

  (define taylor-cache (make-hash))

  (hash-set! taylor-cache '() (zero-series (taylor-inf (car vars) expr)))

  (define (take-taylor coeffs)
    (hash-ref! taylor-cache coeffs
               (λ ()
                  (let* ([oc (take-taylor (cdr coeffs))]
                         [expr* (oc (car coeffs))])
                    (if (= (length coeffs) (length vars))
                      expr*
                      (zero-series (taylor-inf (list-ref vars (length coeffs)) expr*)))))))

  (define (make-term coeffs)
    (simplify
     `(* ,(take-taylor coeffs)
         ,(let loop ([vars (reverse vars)] [coeffs coeffs])
            (if (null? vars)
                1
                (let ([var (car vars)] [idx (car coeffs)])
                  `(/ ,(loop (cdr vars) (cdr coeffs)) ,(make-monomial var idx))))))))

  (simplify
   (cons '+
         (let loop ([i 0] [res '()])
           (if (or (> i (* iters (length vars))) (>= (length res) terms))
               res
               (let ([coeffs (iterate-diagonal (length vars) i)])
                 (if (not (equal? (take-taylor coeffs) 0))
                     (loop (+ i 1) (cons (make-term coeffs) res))
                     (loop (+ i 1) res))))))))

(define (make-sum terms)
  (match terms
   ['() 0]
   [`(,x) x]
   [`(,x ,xs ...)
    `(+ ,x ,(make-sum xs))]))

(define (make-monomial var pow)
  (cond
   [(equal? pow 0) 1]
   [(equal? pow 1) var]
   [(equal? pow 2) `(sqr ,var)]
   [(equal? pow -1) `(/ 1 ,var)]
   [(equal? pow -2) `(/ 1 (sqr ,var))]
   [(positive? pow) `(expt ,var ,pow)]
   [(negative? pow) `(expt ,var ,pow)]))

(define (make-expt var pow)
  (cond
   [(equal? pow 0) 1]
   [(equal? pow 1) var]
   [else
    (let* ([l (floor (/ pow 2))] [r (- pow l)])
      `(* ,(make-expt var l) ,(make-expt var r)))]))

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

(define (taylor-inf var expr)
  (let ([vars (free-variables expr)])
    (taylor-0
     var
     (pattern-substitute expr
                         (for/list ([var* vars])
                           (cons var* (if (eq? var var*) `(/ 1 ,var) var*)))))))

(define (taylor-0 var expr)
  "Return a pair (e, n), such that expr ~= e var^n"
  (debug #:from 'taylor "Taking taylor expansion of" expr "in" var)
  (match expr
    [(? (curry equal? var))
     (taylor-exact 0 1)]
    [(? constant?)
     (taylor-exact expr)]
    [(? variable?)
     (taylor-exact expr)]
    [`(abs ,arg)
     (taylor-exact expr)]
    [`(+ ,args ...)
     (apply taylor-add (map (curry taylor-0 var) args))]
    [`(- ,arg)
     (taylor-negate ((curry taylor-0 var) arg))]
    [`(- ,arg ,args ...)
     (apply taylor-add ((curry taylor-0 var) arg) (map (compose taylor-negate (curry taylor-0 var)) args))]
    [`(* ,left ,right)
     (taylor-mult (taylor-0 var left) (taylor-0 var right))]
    [`(/ ,arg)
     (taylor-invert (taylor-0 var arg))]
    [`(/ 1 ,arg)
     (taylor-invert (taylor-0 var arg))]
    [`(/ ,num ,den)
     (taylor-quotient (taylor-0 var num) (taylor-0 var den))]
    [`(if ,cond ,btrue ,bfalse)
     (taylor-exact expr)]
    [`(mod ,a ,b)
     (taylor-exact expr)]
    [`(sqr ,a)
     (let ([ta (taylor-0 var a)])
       (taylor-mult ta ta))]
    [`(sqrt ,arg)
     (taylor-sqrt (taylor-0 var arg))]
    [`(exp ,arg)
     (let ([arg* (normalize-series (taylor-0 var arg))])
       (if (positive? (car arg*))
           (taylor-exact expr)
           (taylor-exp (zero-series arg*))))]
    [`(sin ,arg)
     (let ([arg* (normalize-series (taylor-0 var arg))])
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
     (let ([arg* (normalize-series (taylor-0 var arg))])
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
     (let* ([arg* (normalize-series (taylor-0 var arg))]
            [rest (taylor-log (cdr arg*))])
       (if (zero? (car arg*))
           rest
           (cons 0
                 (λ (n)
                    (if (= n 0)
                        (simplify `(+ (* (- ,(car arg*)) (log ,var))
                                      ,((cdr rest) 0)))
                        ((cdr rest) n))))))]
    [`(expt ,(? (curry equal? var)) ,(? integer? pow))
     (cons (- pow) (λ (n) (if (= n 0) 1 0)))]
    [`(expt ,base ,pow)
     (taylor-0 var `(exp (* ,pow (log ,base))))]
    [`(tan ,arg)
     (taylor-0 var `(/ (sin ,arg) (cos ,arg)))]
    [`(cotan ,arg)
     (taylor-0 var `(/ (cos ,arg) (sin ,arg)))]
    [_
     (taylor-exact expr)]))

; A taylor series is represented by a function f : nat -> expr,
; representing the coefficients (the 1 / n! terms not included),
; and an integer offset to the exponent

(define (taylor-exact . terms)
  (cons 0
        (λ (n)
           (if (<= (length terms) n)
               0
               (simplify (list-ref terms n))))))

(define (first-nonzero-exp f)
  "Returns n, where (series n) != 0, but (series n) = 0 for all smaller n"
  (let loop ([n 0])
    (if (and (equal? (f n) 0) (< n 20))
        (loop (+ n 1))
        n)))

(define (align-series . serieses)
  (if (apply = (map car serieses))
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
     (cons (car offset)
           (λ (n) (simplify (cons '+ (for/list ([series serieses]) (series n))))))]))

(define (taylor-negate term)
  (cons (car term) (λ (n) (simplify (list '- ((cdr term) n))))))

(define (taylor-mult left right)
  (cons (+ (car left) (car right))
        (lambda (n)
          (simplify
           (cons '+
                 (for/list ([i (range (+ n 1))])
                   (list '* ((cdr left) i) ((cdr right) (- n i)))))))))

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
                                     `(- (+ ,@(for/list ([i (range n)])
                                                `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))])
         (cons (- offset) f)))]))

(define (taylor-quotient num denom)
  "This gets tricky, because the function might have a pole at 0.
   This happens if the inverted series doesn't have a constant term,
   so we extract that case out."
  (match (cons (normalize-series num) (normalize-series denom))
    [(cons (cons noff a) (cons doff b))
     (let ([hash (make-hash)])
       (hash-set! hash 0 (simplify `(/ ,(a 0) ,(b 0))))
       (letrec ([f (λ (n)
                      (hash-ref! hash n
                                 (λ ()
                                    (simplify
                                     `(- (/ ,(a n) ,(b 0))
                                         (+ ,@(for/list ([i (range n)])
                                                `(* ,(f i) (/ ,(b (- n i)) ,(b 0))))))))))]
                [offset (- noff doff)])
         (cons offset f)))]))

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
                                    `(/ (- ,(coeffs* n) (sqr ,(f (/ n 2)))
                                           (+ ,@(for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                                  `(* 2 (* ,(f k) ,(f (- n k)))))))
                                        (* 2 ,(f 0)))]
                                   [(odd? n)
                                    `(/ (- ,(coeffs* n)
                                           (+ ,@(for/list ([k (in-naturals 1)] #:break (>= k (- n k)))
                                                  `(* 2 (* ,(f k) ,(f (- n k)))))))
                                        (* 2 ,(f 0)))])))))])
      (cons (/ offset* 2) f))))

(define (rle l)
  (for/list ([run (multipartition l identity)])
    (cons (length run) (car run))))

(define (partition-list n)
  (define (aux n k)
    (cond
     [(= n 0) '(())]
     [(< n k) '()]
     [else
      (append (map (curry cons k) (aux (- n k) k))
              (aux n (+ k 1)))]))
  (map rle (aux n 1)))

(define (taylor-exp coeffs)
   (cons 0
         (λ (n)
            (if (= n 0)
                (simplify `(exp ,(coeffs 0)))
                (simplify
                 `(* (exp ,(coeffs 0))
                     (+
                      ,@(for/list ([p (partition-list n)])
                          `(*
                            ,@(for/list ([factor p])
                                `(/ (expt ,(coeffs (cdr factor)) ,(car factor))
                                    ,(factorial (car factor)))))))))))))

(define (taylor-sin coeffs)
  (cons 0
        (λ (n)
           (if (= n 0)
               0
               (simplify
                `(+
                  ,@(for/list ([p (partition-list n)])
                      (if (= (modulo (apply + (map car p)) 2) 1)
                          `(* ,(if (= (modulo (apply + (map car p)) 4) 1) 1 -1)
                              ,@(for/list ([factor p])
                                  `(/ (expt ,(coeffs (cdr factor)) ,(car factor))
                                      ,(factorial (car factor)))))
                          0))))))))

(define (taylor-cos coeffs)
  (cons 0
        (λ (n)
           (if (= n 0)
               1
               (simplify
                `(+
                  ,@(for/list ([p (partition-list n)])
                      (if (= (modulo (apply + (map car p)) 2) 0)
                          `(* ,(if (= (modulo (apply + (map car p)) 4) 0) 1 -1)
                              ,@(for/list ([factor p])
                                  `(/ (expt ,(coeffs (cdr factor)) ,(car factor))
                                      ,(factorial (car factor)))))
                          0))))))))

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
          (for/list ([entry (multipartition table cdr)])
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
  (cons 0
        (λ (n)
           (if (= n 0)
               (simplify `(log ,(coeffs 0)))
               (let* ([tmpl (logcompute n)])
                 (simplify
                  `(/
                    (+ ,@(for/list ([term tmpl])
                           (match term
                             [`(,coeff ,k ,ps ...)
                              `(* ,coeff (/ (* ,@(for/list ([i (in-naturals 1)] [p ps])
                                                   (if (= p 0)
                                                       1
                                                       `(expt (* ,(factorial i) ,(coeffs i)) ,p))))
                                            (expt ,(coeffs 0) ,(- k))))])))
                    ,(factorial n))))))))
