#lang racket

; Test compile optimizations
(module+ test
  (require rackunit
           "machine.rkt"
           "../main.rkt")
  ; This function is needed to unwrap constant procedure which fails tests otherwise
  ; (const (ival 0.bf 0.bf)) != (const (ival 0.bf 0.bf))
  (define (drop-ival-const instrs)
    (for/vector ([instr (in-vector instrs)])
      (match instr
        [`(,const) (const)]
        [_ instr])))

  (define discs (list flonum-discretization))
  (define vars '(x y z))

  (define (check-rival-optimization expr target-expr)
    (define optimized-instrs
      (drop-ival-const (parameterize ([*rival-use-shorthands* #t])
                         (rival-machine-instructions (rival-compile (list expr) vars discs)))))
    (define target-instrs
      (drop-ival-const (parameterize ([*rival-use-shorthands* #f])
                         (rival-machine-instructions (rival-compile (list target-expr) vars discs)))))
    (check-equal? optimized-instrs target-instrs))

  (check-rival-optimization `(* (log (exp x)) y) `(* x y))
  (check-rival-optimization `(* (exp (log x)) y) `(* (then (assert (> x 0)) x) y))
  (check-rival-optimization `(fma x y z) `(+ (* x y) z))
  (check-rival-optimization `(- (exp x) 1) `(expm1 x))
  (check-rival-optimization `(- 1 (exp x)) `(neg (expm1 x)))
  (check-rival-optimization `(log (+ 1 x)) `(log1p x))
  (check-rival-optimization `(log (+ x 1)) `(log1p x))
  (check-rival-optimization `(sqrt (+ (* x x) (* y y))) `(hypot x y))
  (check-rival-optimization `(sqrt (+ (* x x) 1)) `(hypot x 1))
  (check-rival-optimization `(sqrt (+ 1 (* x x))) `(hypot 1 x))
  (check-rival-optimization `(pow x 2) `(pow2 x))
  (check-rival-optimization `(pow x 1/3) `(cbrt x))
  (check-rival-optimization `(pow x 1/2) `(sqrt x))
  (check-rival-optimization `(pow 2 x) `(exp2 x)))

; Check hints from rival-analyze
(module+ test
  (require rackunit
           math/bigfloat
           "../main.rkt")
  (define number-of-random-hyperrects 100)
  (define number-of-random-pts-per-rect 100)
  (define rect (list (bf -100) (bf 100)))
  (bf-precision 53)

  ; Check whether outputs are the same for the hint and without hint executions
  (define (rival-check-hint machine hint pt)
    (define hint-result
      (with-handlers ([exn:rival:invalid? (λ (e) 'invalid)]
                      [exn:rival:unsamplable? (λ (e) 'unsamplable)])
        (rival-apply machine pt hint)))
    (define hint-instr-count (vector-length (rival-profile machine 'executions)))

    (define no-hint-result
      (with-handlers ([exn:rival:invalid? (λ (e) 'invalid)]
                      [exn:rival:unsamplable? (λ (e) 'unsamplable)])
        (rival-apply machine pt)))
    (define no-hint-instr-count (vector-length (rival-profile machine 'executions)))

    (check-equal? hint-result no-hint-result)
    (values no-hint-instr-count hint-instr-count))

  ; Random sampling hyperrects given a general range as [rect-lo, rect-hi]
  (define (sample-hyperrect-within-bounds rect-lo rect-hi varc)
    (for/vector ([_ (in-range varc)])
      (define xlo-range-length (bf- rect-hi rect-lo))
      (define xlo (bf+ (bf* (bfrandom) xlo-range-length) rect-lo))
      (define xhi-range-length (bf- rect-hi xlo))
      (define xhi (bf+ (bf* (bfrandom) xhi-range-length) xlo))
      (check-true (and (bf> rect-hi xhi) (bf> xlo rect-lo) (bf> xhi xlo))
                  "Hyperrect is out of bounds")
      (ival xlo xhi)))

  ; Sample points with respect to the input hyperrect
  (define (sample-pts hyperrect)
    (for/vector ([rect (in-vector hyperrect)])
      (define range-length (bf- (ival-hi rect) (ival-lo rect)))
      (define pt (bf+ (bf* (bfrandom) range-length) (ival-lo rect)))
      (check-true (and (bf> pt (ival-lo rect)) (bf< pt (ival-hi rect)))
                  "Sampled point is out of hyperrect range")
      pt))

  ; Testing hint on an expression for 'number-of-random-hyperrects' hyperrects by
  ;     'number-of-random-pts-per-rect' points each
  (define (hints-random-checks machine rect-lo rect-hi varc)
    (define number-of-instructions-total
      (* number-of-random-hyperrects (vector-length (rival-machine-instructions machine))))

    (define hint-cnt 0)
    (define no-hint-cnt 0)
    (for ([n (in-range number-of-random-hyperrects)])
      (define hyperrect (sample-hyperrect-within-bounds rect-lo rect-hi varc))
      (match-define (list res hint converged?) (rival-analyze-with-hints machine hyperrect))

      ; A little hack, the analyze below uses hint from the previous run
      ; The analyze results must be equal. If not, something wrong has happened
      (match-define (list res* hint* converged?*) (rival-analyze-with-hints machine hyperrect hint))

      (with-check-info (['hyperrect hyperrect] ['hint hint])
                       (check-equal? hint hint*)
                       (check-equal? res res*)
                       (check-equal? converged? converged?*))

      (for ([_ (in-range number-of-random-pts-per-rect)])
        (define pt (sample-pts hyperrect))
        (define-values (no-hint-cnt* hint-cnt*)
          (with-check-info (['pt pt] ['hint hint]) (rival-check-hint machine hint pt)))
        (set! hint-cnt (+ hint-cnt hint-cnt*))
        (set! no-hint-cnt (+ no-hint-cnt no-hint-cnt*))))
    (define skipped-percentage (* (/ hint-cnt no-hint-cnt) 100))
    skipped-percentage)

  (define (expressions2d-check expressions)
    (define discs (list boolean-discretization flonum-discretization))
    (define vars '(x y))
    (define varc (length vars))
    (define machine (rival-compile expressions vars discs))
    (define skipped-instr
      (with-check-info (['expressions expressions])
                       (hints-random-checks machine (first rect) (second rect) varc)))
    (check-true (< skipped-instr 99)
                (format "Almost no instructions got skipped by hint at ~a" expressions)))

  (expressions2d-check (list '(assert (> (+ (log x) (log y)) (- (log x) (log y))))
                             '(+ (if (> (/ (log x) (log y)) (* (log x) (log y)))
                                     (fmax (* (log x) (log y)) (+ (log x) (log y)))
                                     (fmin (* (log x) (log y)) (+ (log x) (log y))))
                                 (if (> (+ (log x) (log y)) (* (log x) (log y)))
                                     (fmax (/ (log x) (log y)) (- (log x) (log y)))
                                     (fmin (/ (log x) (log y)) (- (log x) (log y)))))))

  (expressions2d-check
   (list '(TRUE)
         '(fmax (fmin (fmax (* x y) (+ x y)) (+ (fmax x (* 2 y)) (fmin y (* x 2))))
                (fmax (fmin (* x y) (+ x y)) (+ (fmin x (* 2 y)) (fmax y (* x 2)))))))

  (expressions2d-check (list '(TRUE)
                             '(if (> (exp x) (+ 10 (log y)))
                                  (if (> (fmax (* x y) (+ x y)) 4)
                                      (cos (fmax x y))
                                      (cos (fmin x y)))
                                  (if (< (pow 2 x) (- (exp x) 10))
                                      (* PI x)
                                      (fmax x (- (cos y) (+ 10 (log y))))))))

  ; Test checks hint on assert where an error can be observed
  (expressions2d-check (list '(assert (> (+ (log x) (log y)) (- (log x) (log y))))
                             '(+ (cos x) (cos y))))

  ; Test checks hint on fmax where an error can be observed
  (expressions2d-check (list '(TRUE) '(fmax (log x) y)))

  ; Test checks hints on comparison operators
  (expressions2d-check
   (list '(and (and (> (log x) y) (or (== (exp x) (exp y)) (> (cos x) (cos y)))) (<= (log y) (log x)))
         '(if (or (or (< (log x) y) (and (!= (exp x) (exp y)) (< (cos x) (cos y))))
                  (>= (log y) (log x)))
              x
              y))))

(module+ test
  (require rackunit
           "machine.rkt"
           "../main.rkt")
  ; Test that checks correctness of early exit!
  (define expr '(fmin (exp (pow 1e100 1e200)) (- (cos (+ 1 1e200)) (cos 1e200))))
  (define machine (rival-compile (list expr) '() (list flonum-discretization)))
  (define out (vector-ref (rival-apply machine (vector)) 0))
  (check-equal? out 0.19018843355136827)

  ; Test that checks correctness of path reducing!
  (define machine2
    (rival-compile (list '(+ (fmin (- (cos (+ 1 1e200)) (cos 1e200)) -3)
                             (- (cos (+ 1 1e200)) (cos 1e200))))
                   '()
                   (list flonum-discretization)))
  (check-equal? (vector-ref (rival-apply machine2 (vector)) 0) -2.809811566448632))
