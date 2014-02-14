#lang racket

(provide improve-by-analysis)

(struct annotation (expr exact-value approx-value local-error total-error) #:transparent)

(define (analyze-expressions prog inputs)
  (define varmap (map cons (program-variables prog) inputs))

  (define (real-op->bigfloat-op op)
    (list-ref (hash-ref operations op) mode:bf))

  (define (real-op->float-op op)
    (list-ref (hash-ref operations op) mode:fl))


  (program-induct
   prog

   #:constant
   (λ (c)
      (let* ([exact (bf c)] [approx (*precision* c)]
             [error (relative-error (->flonum exact) approx)])
        (annotation c exact approx error error)))

   #:variable
   (λ (v)
      (let* ([var (cdr (assoc v varmap))]
             [exact (bf var)] [approx (*precision* var)]
             [error (relative-error (->flonum exact) approx)])
        (annotation v exact approx error error)))

   #:primitive
   (λ (expr)
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
        (annotation expr exact-ans approx-ans local-error cumulative-error)))))

(define (find-most-local-error annot-prog)
  (define (search-expression expr loc-tail)
    (if (list? expr)
        (let continue ([expr (cdr expr)] [err 0] [loc loc-tail] [idx 1])
          (cond
           [(null? expr) (values err loc)]
           [#t
            (let-values ([(err* loc*) (search-annot (car expr) (cons idx loc-tail))])
              (if (> err* err)
                  (continue (cdr expr) err* loc* (+ idx 1))
                  (continue (cdr expr) err loc (+ idx 1))))]))
        (values 0 #f)))

  (define (search-annot annot loc)
    (let-values ([(err* loc*) (search-expression (annotation-expr annot) loc)])
      ; Why >=? Because all else equal, we're more interested in small subexpressions
      (if (>= err* (annotation-local-error annot))
          (values err* loc*)
          (values (annotation-local-error annot) loc))))

  (let-values ([(err loc) (search-annot (program-body annot-prog) '(2))])
    (if (= err 0) #f (reverse loc))))

(define (rewrite-at-location prog loc)
  (define vars (program-variables prog))

  (define (recursor exp loc)
    (if (null? loc)
        (rewrite-expression vars exp)
        (let ([front (take exp (car loc))] [tail (drop exp (+ (car loc) 1))]
              [middle (recursor (list-ref exp (car loc)) (cdr loc))])
          (for/list ([opt middle])
            (append front (cons opt tail))))))

  (if loc
      (recursor prog loc)
      (list prog)))

(define (improve-by-analysis prog iters points exacts)
  (define (pick-input prog)
    (argmax cadr (filter (λ (x) (< (cadr x) 1))
                  (enumerate (alternative-errors prog) points exacts))))

  (define (step prog input)
    (let ([annot (analyze-expressions (alternative-program prog) input)])
      (map make-alternative (rewrite-at-location (alternative-program prog)
                                                 (find-most-local-error annot)))))

  (define (make-alternative prog)
    (let ([errs (errors prog points exacts)])
      (alternative prog errs (program-cost prog) '())))

  (define start-prog (make-alternative prog))

  (let loop ([good-prog start-prog] [test-prog start-prog] [left iters]
             [input (pick-input start-prog)])
    (println "; Trying " (alternative-program test-prog) " at " (caddr input))
    (if (= left 0)
        good-prog
        (let* ([alts (step test-prog (caddr input))]
               [alts* (sort alts (curry alternative<-at? (car input)))]
               [new-prog (car alts*)])
          (cond
           [(null? alts*)
            good-prog]
           [(< (relative-error ((eval-prog (alternative-program new-prog) mode:fl) (caddr input))
                               (cadddr input) )
               (relative-error ((eval-prog (alternative-program good-prog) mode:fl) (caddr input))
                               (cadddr input)))
            (loop new-prog new-prog iters (pick-input new-prog))]
           [#t
            (loop good-prog new-prog (- left 1) input)])))))
