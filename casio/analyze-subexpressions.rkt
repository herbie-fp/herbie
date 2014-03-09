#lang racket

(require racket/match)
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
      (let* ([exact (repeat (bf c))] [approx (repeat ((*precision*) c))]
             [error (repeat (relative-error (->flonum exact) approx))])
        (annotation c exact approx error error loc)))

   #:variable
   (λ (v loc)
      (let* ([var (for/list ([vmap varmap]) (cdr (assoc v vmap)))]
             [exact (map bf var)] [approx (map (*precision*) var)]
             [error (map relative-error (map ->flonum exact) approx)])
        (annotation v exact approx error error loc)))

   #:primitive
   (λ (expr loc)
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
              (map relative-error (map ->flonum exact-ans) semiapprox-ans)]
             [cumulative-error
              (map relative-error (map ->flonum exact-ans) approx-ans)])
        (annotation expr exact-ans approx-ans local-error cumulative-error loc)))))

(define (interesting-error? l)
  (ormap (curry < 0) l))

(define (summarize-error l)
  (apply + (filter ordinary-float? l)))

(define (find-interesting-locations annot-prog)
  (define (search-expression found expr)
    (when (list? expr)
      (map (curry search-annot found) (cdr expr))))

  (define (search-annot found annot)
    (when (interesting-error? (annotation-local-error annot))
      (found (cons
              (annotation-loc annot)
              (summarize-error (annotation-local-error annot)))))
    (search-expression found (annotation-expr annot)))

  (reap [sow]
    (search-annot sow (program-body annot-prog))))

(define (analyze-local-error altn)
  (find-interesting-locations
   (analyze-expressions (alt-program altn) (*points*))))

(define (strip-off-top loc)
  (let ([loc* (reverse loc)])
    (match loc*
      [`(car cdr car . ,rest)
       (values (reverse (cons 'car rest))
               (reverse (list* 'car 'cdr 'cdr 'car rest)))]
      [`(car cdr cdr car . ,rest)
       (values (reverse (cons 'car rest))
               (reverse (list* 'car 'cdr 'car rest)))]
      [#t
       (error "Wat?")])))
