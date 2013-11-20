#lang racket

(require casio/main)
(require rackunit)

(define (unfold-let expr)
  (match expr
    [`(let ,vars ,body)
     (let loop ([vars vars] [body body])
       (if (null? vars)
           body
           (let ([var (caar vars)] [val (cadar vars)])
             (loop (map (replace-var var val) (cdr vars))
                   ((replace-var var val) body)))))]
    [`(,head ,args ...)
     (cons head (map unfold-let args))]
    [x
     x]))

(define ((replace-var var val) expr)
  (cond
   [(eq? expr var) val]
   [(list? expr)
    (cons (car expr) (map (replace-var var val) (cdr expr)))]
   [#t
    expr]))

(define (bench-results name inerr outerr)
  (printf "~a orders: ~s\n"
          (/ (round (* (- (/ (log (/ outerr (max inerr 1e-16)))
                             (log 10))) 10)) 10)
          name))

(define-binary-check (check-member (lambda (x y) (member y x)) elt lst))

(define-syntax (casio-test stx)
  (syntax-case stx ()
    [(_ vars name input output)
     #`(let* ([prog '(lambda vars input)]
              [alts (map alternative-program
                         (heuristic-execute prog 5))])
         (with-check-info (['start 'input] ['goal 'output])
           (check-member (map program-body alts) 'output name)))]))

(define-syntax (casio-bench stx)
  (syntax-case stx ()
    [(_ vars name input)
     #`(let* ([pts (make-points (length 'vars))]
              [prog '(lambda vars input)]
              [exacts (make-exacts prog pts)]
              [output (alternative-program
                      (car (sort (heuristic-execute prog 5)
                                 #:key alternative-score list<)))])
         (let-values ([(prog-score prog-specials)
                       (max-error prog pts exacts)]
                      [(goal-score goal-specials)
                       (max-error output pts exacts)])
           (bench-results name prog-score goal-score)))]))

(provide casio-test casio-bench)
