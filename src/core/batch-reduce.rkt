#lang racket

(require "batch.rkt"
         "../utils/common.rkt"
         "programs.rkt")

(provide batch-reduce)

(define global-batch (make-parameter #f))
(define eval-application (make-parameter #f))
(define reduce-node (make-parameter #f))
(define reduce-evaluation (make-parameter #f))
(define reduce-inverses (make-parameter #f))
(define gather-additive-terms (make-parameter #f))
(define gather-multiplicative-terms (make-parameter #f))

;; Covered by tests
;; This is a transcription of egg-herbie/src/math.rs, lines 97-149

;; To evaluate recursively - change deref to recurse
(define (batch-eval-application batch)
  (define exact-value? (conjoin number? exact?))
  (batch-recurse batch
                 (λ (brf recurse)
                   (define res
                     (match (deref brf)
                       [(? exact-value? val) val]
                       [(list '+ (app deref (? exact-value? as)) ...) (apply + as)]
                       [(list '- (app deref (? exact-value? as)) ...) (apply - as)]
                       [(list '* (app deref (? exact-value? as)) ...) (apply * as)]
                       [(list '/ (app deref (? exact-value? num)) (app deref (? exact-value? den)))
                        (and (not (zero? den)) (/ num den))]
                       [(list 'neg (app deref (? exact-value? arg))) (- arg)]
                       [(list 'pow (app deref (? exact-value? a)) (app deref (? exact-value? b)))
                        (cond
                          [(and (zero? b) (not (zero? a))) 1]
                          [(and (zero? a) (positive? b)) 0]
                          [(and (not (zero? a)) (integer? b)) (expt a b)]
                          [(= a -1) (if (even? (numerator b)) 1 -1)]
                          [(= a 1) 1]
                          [else #f])]
                       [(list 'sqrt (app deref (? exact-value? a)))
                        (define s1 (sqrt (numerator a)))
                        (define s2 (sqrt (denominator a)))
                        (and (real? s1) (real? s2) (exact? s1) (exact? s2) (/ s1 s2))]
                       [(list 'cbrt (app deref (? exact-value? a)))
                        (define inexact-num (inexact->exact (expt (abs (numerator a)) 1/3)))
                        (define inexact-den (inexact->exact (expt (abs (denominator a)) 1/3)))
                        (and (real? inexact-num)
                             (real? inexact-den)
                             (= (expt inexact-num 3) (abs (numerator a)))
                             (= (expt inexact-den 3) (abs (denominator a)))
                             (* (sgn a) (/ inexact-num inexact-den)))]
                       [(list 'fabs (app deref (? exact-value? a))) (abs a)]
                       [(list 'floor (app deref (? exact-value? a))) (floor a)]
                       [(list 'ceil (app deref (? exact-value? a))) (ceiling a)]
                       [(list 'round (app deref (? exact-value? a))) (round a)]
                       ;; Added
                       [(list 'exp (app deref 0)) 1]
                       [(list 'log (app deref 1)) 0]
                       [_ #f]))
                   res)))

(define (batch-reduce batch)
  (define eval-application* (batch-eval-application batch))
  (define reduce-node* (batch-reduce-node batch))
  (define reduce-evaluation* (batch-reduce-evaluation batch))
  (define reduce-inverses* (batch-reduce-inverses batch))
  (define gather-additive-terms* (batch-gather-additive-terms batch))
  (define gather-multiplicative-terms* (batch-gather-multiplicative-terms batch))
  (batch-recurse batch
                 (λ (brf recurse)
                   (define res
                     (parameterize ([global-batch batch]
                                    [eval-application eval-application*]
                                    [reduce-node reduce-node*]
                                    [reduce-evaluation reduce-evaluation*]
                                    [reduce-inverses reduce-inverses*]
                                    [gather-additive-terms gather-additive-terms*]
                                    [gather-multiplicative-terms gather-multiplicative-terms*])
                       (define node (deref brf))
                       (match node
                         [(? number?) brf]
                         [(? symbol?) brf]
                         [`(,op ,args ...)
                          (define args* (map recurse args))
                          (define brf* (batch-add! batch (list* op args*)))
                          (define val ((eval-application) brf*))
                          (or val ((reduce-node) brf*))])))
                   (printf "reduce* ~a -> ~a\n" (batch-pull brf) (batch-pull res))
                   res)))

;; Covered by tests
(define (batch-reduce-evaluation batch)
  (batch-apply!
   batch
   (λ (node)
     (match node
       [(list 'sin (app deref 0)) 0]
       [(list 'cos (app deref 0)) 1]
       [(list 'sin (app deref (list 'PI))) 0]
       [(list 'cos (app deref (list 'PI))) -1]
       [(list 'exp (app deref 1)) '(E)]
       [(list 'tan (app deref 0)) 0]
       [(list 'sinh (app deref 0)) 0]
       [(list 'log (app deref (list 'E))) 1]
       [(list 'exp (app deref 0)) 1]
       [(list 'tan (app deref (list 'PI))) 0]
       [(list 'cosh (app deref 0)) 1]
       [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 6)))) '(/ (sqrt 3) 2)]
       [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 3)))) '(sqrt 3)]
       [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 4)))) 1]
       [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 2)))) 0]
       [(list 'tan (app deref (list '/ (app deref '(PI)) (app deref 6)))) '(/ 1 (sqrt 3))]
       [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 3)))) '(/ (sqrt 3) 2)]
       [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 6)))) 1/2]
       [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 4)))) '(/ (sqrt 2) 2)]
       [(list 'sin (app deref (list '/ (app deref '(PI)) (app deref 2)))) 1]
       [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 3)))) 1/2]
       [(list 'cos (app deref (list '/ (app deref '(PI)) (app deref 4)))) '(/ (sqrt 2) 2)]
       [_ node]))))

;; Covered by tests
(define (batch-reduce-inverses batch)
  (batch-apply! batch
                (λ (node)
                  (match node
                    [(list 'tanh (app deref (list 'atanh x))) x]
                    [(list 'cosh (app deref (list 'acosh x))) x]
                    [(list 'sinh (app deref (list 'asinh x))) x]
                    [(list 'acos (app deref (list 'cos x))) x]
                    [(list 'asin (app deref (list 'sin x))) x]
                    [(list 'atan (app deref (list 'tan x))) x]
                    [(list 'tan (app deref (list 'atan x))) x]
                    [(list 'cos (app deref (list 'acos x))) x]
                    [(list 'sin (app deref (list 'asin x))) x]
                    [(list 'pow x (app deref 1)) x]
                    [(list 'log (app deref (list 'exp x))) x]
                    [(list 'exp (app deref (list 'log x))) x]
                    [(list 'cbrt (app deref (list 'pow x (app deref 3)))) x]
                    [(list 'pow (app deref (list 'cbrt x)) (app deref 3)) x]
                    [_ node]))))

(define (batch-reduce-node batch)
  (batch-recurse
   batch
   (λ (brf recurse)
     (define node (deref brf))
     (define brf* ((reduce-evaluation) brf))
     (define res
       (match (deref brf*)
         [(? number?) brf] ; that's kinda weird, why we do not return a reduced evaluation
         [(? symbol?) brf]
         [(or `(+ ,_ ...) `(- ,_ ...) `(neg ,_))
          (make-addition-node (combine-aterms ((gather-additive-terms) brf)))]
         [(or `(* ,_ ...)
              `(/ ,_ ...)
              `(cbrt ,_)
              `(pow ,_ ,(app deref (? (conjoin rational? (negate even-denominator?))))))
          (make-multiplication-node (combine-mterms ((gather-multiplicative-terms)
                                                     brf)))] ; also weird here
         [`(exp (* ,c (log ,x)))
          (define rewrite (batch-add! batch `(pow ,x ,c)))
          (recurse rewrite)]
         [else ((reduce-inverses) brf)]))
     (printf "reduce-node ~a -> ~a\n" (batch-pull brf) (batch-pull res))
     res)))

(define (negate-term term)
  (cons (batch-push! (global-batch) (- (deref (car term)))) (cdr term)))

(define (batch-gather-additive-terms batch)
  (batch-recurse
   batch
   (λ (brf recurse)
     (define res
       (match (deref brf)
         [(? number?) `((,brf ,(batch-push! batch 1)))]
         [(? symbol?) `((,(batch-push! batch 1) ,brf))]
         [`(+ ,args ...) (append-map recurse args)]
         [`(neg ,arg) (map negate-term (recurse arg))]
         [`(- ,arg ,args ...) (append (recurse arg) (map negate-term (append-map recurse args)))]
         ; Prevent fall-through to the next case
         [`(/ ,arg) `((,(batch-push! batch 1) ,brf))]
         [`(/ ,arg ,args ...)
          (for/list ([term (recurse arg)])
            (list* (car term)
                   ((reduce-node) (batch-add! batch (list* '/ (cadr term) args)))
                   (cons brf (cddr term))))]
         [`(pow ,arg ,(app deref 1)) `((,(batch-push! batch 1) ,(batch-push! batch 1)))]
         [else `((,(batch-push! batch 1) ,brf))]))
     (printf "batch-gather-additive-terms ~a -> ~a\n" (batch-pull brf) (~a-terms res))
     res)))

(define (even-denominator? x)
  (even? (denominator x)))

(define (batch-gather-multiplicative-terms batch)
  (batch-recurse
   batch
   (λ (brf recurse)
     (define res
       (match (deref brf)
         [(? number?) (list brf)]
         ['NAN (list (batch-push! batch 'NAN))]
         [(? symbol?) `(,(batch-push! batch 1) (,(batch-push! batch 1) . ,brf))]
         [`(neg ,arg)
          (define terms (recurse arg))
          (if (eq? (deref (car terms)) 'NAN)
              (list (batch-push! batch 'NAN))
              (negate-term terms))]
         [`(* ,args ...)
          (define terms (map recurse args))
          (if (ormap (curry eq? 'NAN) (map (compose deref car) terms))
              (list (batch-push! batch 'NAN))
              (cons (batch-push! batch (apply * (map (compose deref car) terms)))
                    (apply append (map cdr terms))))]
         [`(/ ,arg)
          (define terms (recurse arg))
          (cons (if (member (deref (car terms)) '(0 NAN))
                    (batch-push! batch 'NAN)
                    (batch-push! batch (/ (deref (car terms)))))
                (map negate-term (cdr terms)))]
         [`(/ ,arg ,args ...)
          (define num (recurse arg))
          (define dens (map recurse args))
          (cons (if (or (eq? (deref (car num)) 'NAN)
                        (ormap (compose (curryr member '(0 NAN)) deref car) dens))
                    (batch-push! batch 'NAN)
                    (batch-push! batch (apply / (deref (car num)) (map (compose deref car) dens))))
                (append (cdr num) (map negate-term (append-map cdr dens))))]
         [`(cbrt ,arg)
          (define terms (recurse arg))
          (define exact-cbrt
            (match (deref (car terms))
              ['NAN (batch-push! batch 'NAN)]
              [x ((eval-application) (batch-add! batch (list* 'cbrt (car terms))))]))
          (if exact-cbrt
              (cons exact-cbrt
                    (for/list ([term (cdr terms)])
                      (cons (batch-push! batch (/ (deref (car term)) 3)) (cdr term))))
              (list* (batch-push! batch 1)
                     (cons (batch-push! batch 1) (batch-add! batch `(cbrt ,(car terms))))
                     (for/list ([term (cdr terms)])
                       (cons (batch-push! batch (/ (deref (car term)) 3)) (cdr term)))))]
         [`(pow ,arg ,(app deref 0)) (list (batch-push! batch 1))]
         [`(pow ,arg ,(app deref (? (conjoin rational? (negate even-denominator?)) a)))
          (define terms (recurse arg))
          (define exact-pow
            (match (deref (car terms))
              ['NAN (batch-push! batch 'NAN)]
              [x
               (printf "!!!!!~a" (batch-pull (batch-add! batch (list* 'pow (car terms) a))))
               ((eval-application) (batch-add! batch (list* 'pow (car terms) a)))]))
          (if exact-pow
              (cons exact-pow
                    (for/list ([term (cdr terms)])
                      (cons (batch-push! batch (* a (deref (car term)))) (cdr term))))
              (list* (batch-push! batch 1)
                     (cons (batch-push! batch a) (car terms))
                     (for/list ([term (cdr terms)])
                       (cons (batch-push! batch (* a (deref (car term)))) (cdr term)))))]
         [else `(,(batch-push! batch 1) (,(batch-push! batch 1) . ,brf))]))
     (printf "batch-gather-multiplicative-terms ~a -> ~a\n" (batch-pull brf) (~a-terms res))
     res)))

(define (~a-terms terms)
  (map (λ (x)
         (match x
           [(cons a '()) (cons (batch-pull a) '())]
           [(cons a (list b)) (cons (batch-pull a) (list (batch-pull b)))]
           [(? list?) (map batch-pull x)]
           [(cons a b) (cons (batch-pull a) (batch-pull b))]
           [_ (batch-pull x)]))
       terms))

(define (combine-aterms terms)
  (define h (make-hash))
  (for ([term terms])
    (define sum (hash-ref! h (cadr term) 0))
    (hash-set! h (cadr term) (+ (deref (car term)) sum)))
  (define res
    (sort (reap [sow]
                (for ([(k v) (in-hash h)]
                      #:when (not (= v 0)))
                  (sow (cons (batch-push! (global-batch) v) k))))
          expr<?
          #:key cdr))
  (printf "combine-aterms ~a -> ~a\n" (~a-terms terms) (~a-terms res))
  res)

(define (combine-mterms terms)
  (define res
    (cons (car terms)
          (let ([h (make-hash)])
            (for ([term (cdr terms)])
              (define sum (hash-ref! h (cdr term) 0))
              (hash-set! h (cdr term) (+ (deref (car term)) sum)))
            (sort (reap [sow]
                        (for ([(k v) (in-hash h)]
                              #:unless (= v 0))
                          (sow (cons (batch-push! (global-batch) v) k))))
                  expr<?
                  #:key cdr))))
  (printf "combine-mterms ~a -> ~a\n" (~a-terms terms) (~a-terms res))
  res)

;;---------------------------------------------------------------------------------------------

(define (aterm->expr term)
  (match term
    [`(,(app deref 1) . ,x) x]
    [`(,x . ,(app deref 1)) x]
    [`(,(app deref -1) . ,x) (batch-add! (global-batch) `(neg ,x))]
    [`(,coeff . ,x) (batch-add! (global-batch) `(* ,coeff ,x))]))

(define (make-addition-node terms)
  (define-values (pos neg)
    (partition (λ (x) (and (real? (deref (car x))) (positive? (deref (car x))))) terms))
  (define res
    (cond
      [(and (null? pos) (null? neg)) (batch-push! (global-batch) 0)]
      [(null? pos) (batch-add! (global-batch) `(neg ,(make-addition-node* (map negate-term neg))))]
      [(null? neg) (make-addition-node* pos)]
      [else
       (batch-add! (global-batch)
                   `(- ,(make-addition-node* pos) ,(make-addition-node* (map negate-term neg))))]))
  (printf "make-addition-node ~a -> ~a\n" (~a-terms terms) (batch-pull res))
  res)

;; TODO : Use (- x y) when it is simpler
(define (make-addition-node* terms)
  (match terms
    ['() (batch-push! (global-batch) 0)]
    [`(,term) (aterm->expr term)]
    [`(,term ,terms ...)
     (batch-add! (global-batch) `(+ ,(aterm->expr term) ,(make-addition-node terms)))]))

(define (make-multiplication-node term)
  (match (cons (car term) (make-multiplication-subnode (cdr term)))
    [(cons (app deref 'NAN) e) (list (batch-push! (global-batch) 'NAN))]
    [(cons (app deref 0) e) (batch-push! (global-batch) 0)]
    [(cons (app deref 1) '()) (batch-push! (global-batch) 1)]
    [(cons (app deref 1) e) e]
    [(cons a (app deref 1)) a]
    [(cons a (app deref (list '/ (app deref 1) denom))) (batch-add! (global-batch) `(/ ,a ,denom))]
    [(cons a '()) a]
    [(cons a e) (batch-add! (global-batch) `(* ,a ,e))]))

(define (make-multiplication-subnode terms)
  (make-multiplication-subsubsubnode
   (list (cons (batch-push! (global-batch) 1)
               (mterm->expr (cons (batch-push! (global-batch) 1)
                                  (make-multiplication-subsubnode terms)))))))

(define (make-multiplication-subsubnode terms)
  (define-values (pos neg) (partition (compose positive? deref car) terms))
  (cond
    [(and (null? pos) (null? neg)) (batch-push! (global-batch) 1)]
    [(null? pos)
     (batch-add! (global-batch) `(/ 1 ,(make-multiplication-subsubsubnode (map negate-term neg))))]
    [(null? neg) (make-multiplication-subsubsubnode pos)]
    [else
     (batch-add! (global-batch)
                 `(/ ,(make-multiplication-subsubsubnode pos)
                     ,(make-multiplication-subsubsubnode (map negate-term neg))))]))

(define (make-multiplication-subsubsubnode terms)
  (match terms
    ['() (batch-push! (global-batch) 1)]
    [`(,term) (mterm->expr term)]
    [`(,term ,terms ...)
     (batch-add! (global-batch)
                 `(* ,(mterm->expr term) ,(make-multiplication-subsubsubnode terms)))]))

(define (mterm->expr term)
  (match term
    [(cons (app deref 1) x) x]
    [(cons (app deref -1) x) (batch-add! (global-batch) `(/ 1 ,x))]
    [(cons (app deref 1/2) x) (batch-add! (global-batch) `(sqrt ,x))]
    [(cons (app deref -1/2) x) (batch-add! (global-batch) `(/ 1 (sqrt ,x)))]
    [(cons (app deref 1/3) x) (batch-add! (global-batch) `(cbrt ,x))]
    [(cons (app deref -1/3) x) (batch-add! (global-batch) `(/ 1 (cbrt ,x)))]
    [(cons power x) (batch-add! (global-batch) `(pow ,x ,power))]))

(module+ test
  (require rackunit)
  (define batch (batch-empty))
  (define evaluator (batch-eval-application batch))
  (define (evaluator-results expr)
    (evaluator (batch-add! batch expr)))

  ;; Checks for batch-eval-application
  ;(check-equal? (evaluator-results '(+ 1 (cbrt 8))) 3)
  ;(check-equal? (evaluator-results '(exp (/ 0 100))) 1)
  (check-equal? (evaluator-results '(+ 1 1)) 2)
  (check-equal? (evaluator-results '(+)) 0)
  (check-equal? (evaluator-results '(/ 1 0)) #f) ; Not valid
  (check-equal? (evaluator-results '(cbrt 1)) 1)
  (check-equal? (evaluator-results '(log 1)) 0)
  (check-equal? (evaluator-results '(exp 2)) #f) ; Not exact

  ;; Checks for batch-reduce-evaluation
  (define reducer (batch-reduce-evaluation batch))
  (define (reducer-results expr)
    (batch-pull (reducer (batch-add! batch expr))))
  (check-equal? (reducer-results '(cos (/ (PI) 6))) '(/ (sqrt 3) 2))
  (check-equal? (reducer-results '(sin (/ (PI) 4))) '(/ (sqrt 2) 2))
  (check-equal? (reducer-results '(cos (PI))) -1)
  (check-equal? (reducer-results '(exp 1)) '(E))

  (define inverse-reducer (batch-reduce-inverses batch))
  (define (inverse-reducer-results expr)
    (batch-pull (inverse-reducer (batch-add! batch expr))))
  (check-equal? (inverse-reducer-results '(cosh (acosh x))) 'x)
  (check-equal? (inverse-reducer-results '(tanh (atanh x))) 'x)
  (check-equal? (inverse-reducer-results '(sinh (asinh x))) 'x)
  (check-equal? (inverse-reducer-results '(acos (cos x))) 'x)
  (check-equal? (inverse-reducer-results '(asin (sin x))) 'x)
  (check-equal? (inverse-reducer-results '(asin (sin x))) 'x)
  (check-equal? (inverse-reducer-results '(atan (tan x))) 'x)
  (check-equal? (inverse-reducer-results '(tan (atan x))) 'x)
  (check-equal? (inverse-reducer-results '(cos (acos x))) 'x)
  (check-equal? (inverse-reducer-results '(sin (asin x))) 'x)
  (check-equal? (inverse-reducer-results '(pow x 1)) 'x)
  (check-equal? (inverse-reducer-results '(log (exp x))) 'x)
  (check-equal? (inverse-reducer-results '(exp (log x))) 'x)
  (check-equal? (inverse-reducer-results '(cbrt (pow x 3))) 'x)
  (check-equal? (inverse-reducer-results '(pow (cbrt x) 3)) 'x))

(module+ main
  (define batch (batch-empty))
  (define reducer (batch-reduce batch))
  (batch-pull (reducer (batch-add! batch '(- (* (+ x 1) (+ x 1)) 1))))
  (batch-pull (reducer (batch-add! batch '(+ (/ 1 (neg x)) (/ 1 (neg x))))))
  (batch-pull (reducer (batch-add! batch '(- (* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) 1))))
  (batch-pull (reducer (batch-add! batch '(* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)))))
  (batch-pull (reducer (batch-add! batch '(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x))))))
  ;(- (* (+ x 1) (+ x 1)) 1) -> (- (pow (+ 1 x) 2) 1)
  ;(+ (/ 1 (neg x)) (/ 1 (neg x))) -> (neg (* 2 (/ 1 x)))
  ;(- (* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) 1) -> (- (pow (- 1 (/ 1 x)) 2) 1)
  ;(* (+ (/ 1 (neg x)) 1) (+ (/ 1 (neg x)) 1)) -> (pow (- 1 (/ 1 x)) 2)
  ;(+ (* (/ 1 x) (/ 1 x)) (+ (/ 1 x) (/ 1 x))) -> (+ (* 2 (/ 1 x)) (/ 1 (pow x 2)))
  )
