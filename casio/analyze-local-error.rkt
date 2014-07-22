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

(struct annotation (expr exact-value approx-value local-error total-error loc) #:transparent)

(define (real-op->bigfloat-op op)
  (list-ref (hash-ref operations op) mode:bf))

(define (real-op->float-op op)
  (list-ref (hash-ref operations op) mode:fl))

(define (transpose l)
  (apply map list l))

(define (analyze-expressions prog inputs)
  (define varmap
    (for/list ([inps inputs])
      (map cons (program-variables prog) inps)))

  (define (repeat c)
    (map (λ (x) c) inputs))

  (location-induct
   prog

   #:constant
   (λ (c loc)
      (let* ([exact (repeat (->bf c))] [approx (repeat (->flonum c))] [error (repeat 1)])
        (annotation c exact approx error error loc)))

   #:variable
   (λ (v loc)
      (let* ([var (for/list ([vmap varmap]) (cdr (assoc v vmap)))]
             [exact (map ->bf var)] [approx (map ->flonum var)])
        (annotation v exact approx (repeat 1) (repeat 1) loc)))

   #:primitive
   (λ (expr loc)
     (if (eq? (car expr) 'if)
	 (let ([value (annotation-exact-value (caddr expr))])
	   (annotation expr value value (make-list (length value) 0)
		       (map max
			    (annotation-total-error (caddr expr))
			    (annotation-total-error (cadddr expr)))
		       loc))
	 (let* ([exact-op (real-op->bigfloat-op (car expr))]
		[approx-op (real-op->float-op (car expr))]
		[exact-inputs
		 (transpose (map annotation-exact-value (cdr expr)))]
		[semiapprox-inputs (map (curry map ->flonum) exact-inputs)]
		[approx-inputs
		 (transpose (map annotation-approx-value (cdr expr)))]
		[exact-ans (map (curry apply exact-op) exact-inputs)]
		[semiapprox-ans
		 (map (curry apply approx-op) semiapprox-inputs)]
		[approx-ans
		 (map (curry apply approx-op) approx-inputs)]
		[local-error
		 (map (compose add1 flulp-error) (map ->flonum exact-ans) (map ->flonum semiapprox-ans))]
		[cumulative-error
		 (map (compose add1 flulp-error) (map ->flonum exact-ans) (map ->flonum approx-ans))])
	   (annotation expr exact-ans approx-ans local-error cumulative-error loc))))))

(define (find-interesting-locations annot-prog)
  (define (search-expression found expr)
    (when (list? expr)
      (if (eq? (car expr) 'if)
	  (map (curry search-annot found) (cddr expr))
	  (map (curry search-annot found) (cdr expr)))))

  (define (search-annot found annot)
    (when (ormap (λ (x) (> x 1)) (annotation-local-error annot))
      (found (list
              (annotation-loc annot)
              (annotation-local-error annot))))
    (search-expression found (annotation-expr annot)))

  (reap [sow]
        (search-annot sow (program-body annot-prog))))


(define (analyze-local-error altn)
  (map car
       (take-up-to
        (reverse
         (sort
          (find-interesting-locations
           (analyze-expressions (alt-program altn) (*points*)))
          <
          #:key (compose errors-score second)))
        3)))
