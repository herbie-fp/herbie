#lang racket

(require math/flonum)
(require math/bigfloat)
(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/redgreen)
(require casio/simplify)

(provide analyze-local-error)

(struct annotation (children exact-value local-error loc) #:transparent)

(define (real-op->bigfloat-op op) (list-ref (hash-ref operations op) mode:bf)) 
(define (real-op->float-op op) (list-ref (hash-ref operations op) mode:fl))

(define (transpose l)
  (apply map list l))

(define (analyze-expressions prog)
  (define varmap
    (for/list ([inps (*points*)])
      (map cons (program-variables prog) inps)))

  (define (repeat c)
    (map (λ (x) c) (*points*)))

  (location-induct
   prog

   #:constant
   (λ (c loc)
      (annotation '() (repeat (->bf c)) (repeat 1) loc))

   #:variable
   (λ (v loc)
      (let* ([vals (for/list ([vmap varmap]) (cdr (assoc v vmap)))])
        (annotation '() (map ->bf vals) (repeat 1) loc)))

   #:primitive
   (λ (expr loc)
     (if (eq? (car expr) 'if)
	 (let ([value (annotation-exact-value (caddr expr))])
	   (annotation (cddr expr) value (repeat 1) loc))
	 (let* ([exact-op (real-op->bigfloat-op (car expr))]
		[approx-op (real-op->float-op (car expr))]
		[exact-inputs
		 (transpose (map annotation-exact-value (cdr expr)))]
		[approx-inputs (map (curry map ->flonum) exact-inputs)]
		[exact-ans (map (curry apply exact-op) exact-inputs)]
		[approx-ans (map (curry apply approx-op) approx-inputs)]
		[local-error
                 (for/list ([exact exact-ans] [approx approx-ans])
                   (+ 1 (abs (flonums-between (->flonum exact) approx))))])
	   (annotation (cdr expr) exact-ans local-error loc))))

   #:toplevel
   (λ (expr loc)
      (program-body expr))))

(define (analyze-local-error altn)
  (define (search-annot found annot)
    (found annot)
    (map (curry search-annot found) (annotation-children annot)))

  (map annotation-loc
       (filter (λ (alt) (ormap (λ (err) (< 1 err)) (annotation-local-error alt)))
               (take-up-to
                (sort
                 (reap [sow]
                       (search-annot sow (analyze-expressions (alt-program altn))))
                 > #:key (compose errors-score annotation-local-error))
                3))))
