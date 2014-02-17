#lang racket

(require math/bigfloat)
(require casio/common)
(require casio/points)
(require casio/programs)
(require casio/alternative)
(require casio/redgreen)

(provide improve-by-analysis)

(struct annotation (expr exact-value approx-value local-error total-error loc) #:transparent)

(define (real-op->bigfloat-op op)
  (list-ref (hash-ref operations op) mode:bf))

(define (real-op->float-op op)
  (list-ref (hash-ref operations op) mode:fl))

(define (analyze-expressions prog inputs)
  (define varmap (map cons (program-variables prog) inputs))

  (location-induct
   prog

   #:constant
   (λ (c loc)
      (let* ([exact (bf c)] [approx ((*precision*) c)]
             [error (relative-error (->flonum exact) approx)])
        (annotation c exact approx error error loc)))

   #:variable
   (λ (v loc)
      (let* ([var (cdr (assoc v varmap))]
             [exact (bf var)] [approx ((*precision*) var)]
             [error (relative-error (->flonum exact) approx)])
        (annotation v exact approx error error loc)))

   #:primitive
   (λ (expr loc)
      (let* ([exact-op (real-op->bigfloat-op (car expr))]
             [approx-op (real-op->float-op (car expr))]
             [exact-inputs (map annotation-exact-value (cdr expr))]
             [semiapprox-inputs (map ->flonum exact-inputs)]
             [approx-inputs (map annotation-approx-value (cdr expr))]
             [exact-ans (apply exact-op exact-inputs)]
             [semiapprox-ans (apply approx-op semiapprox-inputs)]
             [approx-ans (apply approx-op approx-inputs)]
             [local-error (relative-error (->flonum exact-ans)
                                          semiapprox-ans)]
             [cumulative-error (relative-error (->flonum exact-ans)
                                               approx-ans)])
        (annotation expr exact-ans approx-ans local-error cumulative-error loc)))))

(define (find-most-local-error annot-prog)
  (define (search-expression expr)
    (if (list? expr)
        (let continue ([expr (cdr expr)] [err 0] [loc #f])
          (cond
           [(null? expr) (values err loc)]
           [#t
            (let-values ([(err* loc*) (search-annot (car expr))])
              (if (> err* err)
                  (continue (cdr expr) err* loc*)
                  (continue (cdr expr) err loc)))]))
        (values 0 #f)))

  (define (search-annot annot)
    (let-values ([(err* loc*)
                  (search-expression (annotation-expr annot))])
      ; Why >=? Because all else equal, we're more interested in small subexpressions
      (if (>= err* (annotation-local-error annot))
          (values err* loc*)
          (values (annotation-local-error annot) (annotation-loc annot)))))

  (let-values ([(err loc) (search-annot (program-body annot-prog))])
    (if (= err 0) #f loc)))

(define (pick-bad-input alt)
  (let* ([lst (enumerate (alt-errors alt) (*points*) (*exacts*))]
         ; We filter out very large errors.
         ; Those usually result in overfitting.
         ; TODO: Eliminate this ad-hoc solution.
         [lst* (filter (λ (x) (< (cadr x) 1)) lst)])
    (argmax cadr (if (null? lst*) lst lst*))))

(define (step alt input)
  (let* ([annot (analyze-expressions (alt-program alt) input)]
         [loc (find-most-local-error annot)])
    (if loc
        (alt-rewrite-expression alt #:destruct #t #:root loc)
        '())))

(define (alt-error-at alt idx)
  (list-ref (alt-errors alt) idx))

(define (alt<-at? idx alt1 alt2)
  "Compare two alternatives.
   Compares first by a lattice order on points, then by program cost."
  (eq? (list-ref (errors-compare (alt-errors alt1) (alt-errors alt2))
                 idx)
       '<))

(define (improve-by-analysis alt0 iters)
  (define input (pick-bad-input alt0))

  (let loop ([altn alt0] [left iters])
    (cond
     [(= left 0) altn]
     [(green? altn) altn]
     [#t
      (let* ([alts (step altn (caddr input))])
        (if (null? alts)
            altn
            (loop (argmin (curryr alt-error-at (car input)) alts)
                  (- iters 1))))])))
