#lang racket

;; Periodicity analysis
;
; Given a mathematical formula, determines which components are periodic,
; which variables they are periodic in, and the associated periods.
;
; The basic process is abstract interpretation, where each subexpression
; is classified, for every variable, as one of:
;  - Constant (and the value)
;  - Linear (and the coefficient)
;  - Periodic (and the period)
;  - Other
;
; Known periodic functions, like sin or cotan, transform linear
; expressions into periodic expressions. Periods are then properly
; bubbled up the expression tree.

(require racket/match)
(require casio/common)
(require casio/programs)

(struct annotation (expr loc type coeffs) #:transparent)

(define (constant? a) (eq? (annotation-type a) 'constant))
(define (linear? a)   (eq? (annotation-type a) 'linear))
(define (periodic? a) (eq? (annotation-type a) 'periodic))
(define coeffs annotation-coeffs)

(define (alist-merge merge . as)
  (define (merge-2 a b)
    (cond
     [(null? a) b]
     [(null? b) a]
     [(symbol=? (caar a) (caar b))
      (cons (cons (caar a) (merge (cdar a) (cdar b)))
            (merge-2 (cdr a) (cdr b)))]
     [(symbol<? (caar a) (caar b))
      (cons (car a) (merge-2 (cdr a) b))]
     [(symbol<? (caar b) (caar a))
      (cons (car b) (merge-2 a (cdr b)))]
     [else
      (error "Something horrible has happened" a b)]))
  (foldl merge-2 '() as))

(define (alist-map f al)
  (for/list ([rec al])
    (cons (car rec) (f (cdr rec)))))

(define (default-combine expr loc special)
  (cond
   [special special]
   [(andmap constant? (cdr expr))
    (annotation expr loc 'constant
                (safe-eval (cons (car expr) (map coeffs (cdr expr)))))]
   [(andmap (λ (x) (or (periodic? x) (constant? x))) (cdr expr))
    (annotation expr loc 'periodic
                (apply alist-merge lcm
                       (map coeffs (filter periodic? (cdr expr)))))]
   [else
    (annotation expr loc 'other #f)]))

(define (periodicity prog)
  (define vars (program-variables prog))
  
  (location-induct
   prog

   #:constant
   (λ (c loc)
      (annotation c loc 'constant c))

   #:variable
   (λ (x loc)
      (annotation x loc 'linear `((,x . 1))))

   #:primitive
   (λ (expr loc)
      (define out (curry annotation expr loc))

      ; Default-combine handles function-generic things
      ; The match below handles special cases for various functions
      (default-combine expr loc
        (match expr
          [`(+ ,a ,b)
           (cond
            [(and (constant? a) (linear? b))
             (out 'linear (coeffs b))]
            [(and (linear? a) (constant? b))
             (out 'linear (coeffs a))]
            [(and (linear? a) (linear? b))
             (out 'linear (alist-merge + (coeffs a) (coeffs b)))]
            [else #f])]
          [`(- ,a)
           (cond
            [(linear? a)
             (out 'linear (alist-map (coeffs a) -))])]
          [`(- ,a ,b)
           (cond
            [(and (constant? a) (linear? b))
             (out 'linear (coeffs b))]
            [(and (linear? a) (constant? b))
             (out 'linear (coeffs a))]
            [(and (linear? a) (linear? b))
             (out 'linear (alist-merge - (coeffs a) (coeffs b)))]
            [else #f])]

          [`(* ,a ,b)
           (cond
            [(and (linear? a) (constant? b))
             (out 'linear (alist-map (curry * (coeffs b)) (coeffs a)))]
            [(and (constant? a) (linear? b))
             (out 'linear (alist-map (curry * (coeffs a)) (coeffs b)))]
            [else #f])]
          [`(/ ,a ,b)
           (cond
            [(and (linear? a) (constant? b))
             (if (= 0 (coeffs b))
                 (out 'constant +nan.0)
                 (out 'linear (alist-map (curryr / (coeffs b)) (coeffs a))))]
            [else #f])]

          ; Periodic functions record their period
          ;         AS A MULTIPLE OF PI
          ; This prevents problems from round-off
          [`(sin ,a)
           (cond
            [(linear? a)
             (out 'periodic (alist-map / (coeffs a)))]
            [else #f])]
          [`(cos ,a)
           (cond
            [(linear? a)
             (out 'periodic (alist-map / (coeffs a)))]
            [else #f])]
          [`(tan ,a)
           (cond
            [(linear? a)
             (out 'periodic (alist-map / (coeffs a)))]
            [else #f])]
          [`(cotan ,a)
           (cond
            [(linear? a)
             (out 'periodic (alist-map / (coeffs a)))]
            [else #f])]

          [_ #f])))))
