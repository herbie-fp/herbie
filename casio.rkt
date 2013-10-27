#lang racket

(require racket/match)
(require racket/flonum)

(define prog1 '(/ (- (exp x) 1.0d0) x))
(define prog2 '(/ (- (exp x) 1.0d0) (log (exp x))))

(define (eval-double var prog val)
  (let ([fn (eval `(lambda (,var) ,prog))])
    (fn (real->double-flonum val))))

(define (eval-single var prog val)
  (let ([fn (eval `(lambda (,var) ,prog))])
    (fn (real->single-flonum val))))

(define (error/one var prog val exact)
  (flabs (fl/ (fl- exact (eval-single var prog val)) exact)))

(define (random-flonum)
  (expt 2 (- (* 253 (random)) 126)))

(define (max-error var prog pts exacts)
  (apply max
         (filter (lambda (x) (not (or (infinite? x) (nan? x))))
                 (map (lambda (x exact) (error/one var prog x exact)) pts exacts))))

(define (map-unmap-through fn lst)
  (let loop ([start '()] [end lst] [out '()])
    (cond
      [(null? end) out]
      [(not (list? end)) (list end)]
      [#t
       (let ([focus (car end)]
             [end (cdr end)])
         (loop
          (cons focus start)
          end
          (append
           (map (lambda (x) (append (reverse start) (cons x end)))
                (fn focus))
           out)))])))

(define-syntax recursive-match
  (lambda (stx)
    (syntax-case stx ()
      [(_ value [pattern expansion] ...)
       #'(letrec
             [(matcher (lambda (x)
                         (append
                          (match x
                            [pattern (list expansion)] [_ '()]) ...)))
              (walker (lambda (x)
                        (append
                         (matcher x)
                         (map-unmap-through
                          matcher
                          x))))]
             (walker value))])))

(define (alternatives var expr)
  (recursive-match expr
    ;[`(list - ,x ,x) 0]
    ;[`(+ ,a (+ ,b ,c)) `(+ (+ ,a ,b) ,c)]
    [(or (? (lambda (x) (eq? x var)) x) (? list? x)) `(exp (log ,x))]
    [(or (? (lambda (x) (eq? x var)) x) (? list? x)) `(log (exp ,x))]))
    ;[`(/ (+ ,x (sqrt ,y)) ,c) `(/ (- (expt ,x 2) ,y) (* ,c (- ,x (sqrt ,y))))]))

(define (merge-lists a b)
  "Merge two sorted lists a and b into one sorted list, sorting by cdr"
  (let loop ([a a] [b b] [res '()])
    (cond
      [(null? a) (append (reverse res) b)]
      [(null? b) (append (reverse res) a)]
      [(equal? (car a) (car b))
       (loop (cdr a) (cdr b) (cons (car a) res))]
      [(< (cdar a) (cdar b))
       (loop (cdr a) b (cons (car a) res))]
      [(>= (cdar a) (cdar b))
       (loop a (cdr b) (cons (car b) res))])))

(define (heuristic-search var start generate evaluate iterations)
  (let [(options (list (cons start (evaluate var start))))
        (done '())
        (step (lambda (options done)
                  (if (null? options)
                      (values options done)
                      ; We generate alternatives from "parent" and then annotate each with its value
                      (let ([parent (caar options)])
                        (values
                         (merge-lists
                          (cdr options)
                          (sort
                           (map (lambda (child) (cons child (evaluate var child)))
                                (generate var parent))
                           (lambda (x y) (< (cdr x) (cdr y)))))
                         (cons (car options) done))))))]
    (let loop ([options options] [done done])
      (if (or (null? options) (>= (length done) iterations))
          (values options done)
          (call-with-values
           (lambda () (step options done))
           loop)))))

(define (heuristic-improve var prog iterations)
  (let* ([pts (build-list 100 (lambda (x) (random-flonum)))]
         [exacts (map (lambda (x) (eval-double var prog x)) pts)])
    (heuristic-search var prog alternatives
                      (lambda (var prog) (max-error var prog pts exacts))
                      iterations)))