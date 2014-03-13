#lang racket

(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/brute-force)
(require casio/redgreen)
(require casio/analyze-subexpressions)

(define *analyses* (list analyze-local-error))
(define *strategies* (list brute-force-search))

(define (analyze altn)
  (apply append
         (for/list ([anal *analyses*])
           (for/list ([know (anal altn)])
             (list* anal altn know)))))

(define (make-steps altns knows)
  (append
   (for/list ([altn altns])
     (list alt-rewrite-tree altn '(cdr cdr car)))
   (for/list ([know knows])
     (cond
      [(eq? (car know) analyze-local-error)
       (list alt-rewrite-expression (second know) #t '(cdr cdr car))]
      [else (error "Unknown analysis" (car know))]))))

(define (apply-step step)
  (cond
    [(eq? (car step) alt-rewrite-tree)
     (alt-rewrite-tree (second step) #:root (third step))]
    [(eq? (car step) alt-rewrite-expression)
     (alt-rewrite-expression (second step) #:destruct (third step)
                             #:root (fourth step))]
    [else (error "Unknown step type" (car step))]))

(define (step<? s1 s2)
  (cond
   [(and (eq? (car s1) alt-rewrite-expression)
         (eq? (car s2) alt-rewrite-tree))
    #t]
   [(and (eq? (car s1) alt-rewrite-tree)
         (eq? (car s2) alt-rewrite-expression))
    #f]
   [(and (eq? (car s1) alt-rewrite-expression)
         (eq? (car s2) alt-rewrite-expression))
    (< (length (fourth s1)) (length (fourth s2)))]
   [(and (eq? (car s1) alt-rewrite-tree)
         (eq? (car s2) alt-rewrite-tree))
    (> (length (third s1)) (length (third s2)))]
   [else (error "Cannot compare steps" s1 s2)]))

(define (choose-alt alts)
  (car (sort alts alternative<?)))

(define (choose-step steps)
  (let ([steps* (sort steps step<?)])
    (values (car steps*) (cdr steps*))))

(define (initial altn)
  (let ([know0 (analyze altn)])
    (values (list altn) know0 (make-steps (list altn) know0))))

(define combine (compose remove-duplicates append))

(define (improve prog max-iters)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    ; Save original program
    (let*-values ([(orig) (make-alt prog)]
                  [(alts0 know0 steps0) (initial orig)])
      (let loop ([alts alts0] [knows know0] [steps steps0]
                 [iters max-iters])
	(if (or (null? steps) (<= iters 0))
            (choose-alt alts)
            (let*-values ([(step steps) (choose-step steps)]
                          [(alt*) (apply-step step)]
                          [(know*) (apply append (map analyze alt*))]
                          [(step*) (make-steps alt* know*)])
              (debug step max-iters #:from 'improve)
              (loop (combine alt* alts) (combine know* knows)
                    (combine step* steps) (- iters 1))))))))

;; For usage at the REPL, we define a few helper functions.
;;
;; PROGRAM-A and PROGRAM-B are two example programs to test.
;; (explore prog iters) returns a list of alternatives found
;; (improve prog iters) prints the found alternatives
(define (print-improve prog max-iters)
  (let-values ([(end start) (improve prog max-iters)])
    (println "Started at: " start)
    (println "Ended at:   " end)
    (println "Improvement by an average of "
	     (/ (apply + (filter ordinary-float?
				 (errors-difference (alt-errors start) (alt-errors end))))
		       (log 2))
	     " bits of precision")))

(define program-a '(λ (x) (/ (- (exp x) 1) x)))
(define program-b '(λ (x) (- (sqrt (+ x 1)) (sqrt x))))

;(define (plot-alternatives prog iterations)
;  "Return a spectrum plot of the alternatives found."
;  (let* ([alts (explore prog iterations)]
;         [logs (map (lambda (x) (- (/ (log (alternative-error x)) (log 10)))) alts)]
;         [rands (for/list ([i (range (length logs))]) (random))])
;    (display "Found program with score ")
;    (display (alternative-score (car alts)))
;    (newline)
;    (pretty-print (alternative-program (car alts)))
;    (parameterize ([plot-width 800] [plot-height 100]
;                   [plot-x-label #f] [plot-y-label #f])
;      (plot (points (map vector logs rands))))))

(provide improve *strategies*)
