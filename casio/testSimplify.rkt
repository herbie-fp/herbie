#lang racket

(require casio/simplify)

(provide test-simplify-time)

(define vars
  '(x y z w t))

(define ops
  '(- + / *))

(define (rand-var)
  (list-ref vars (random (length vars))))

(define (rand-op)
  (list-ref ops (random (length ops))))

(define (rand-term)
  `(,(rand-op) ,(rand-var) ,(rand-var)))

(define transformations
  (list (lambda (x) `(/ ,x 1))
	(lambda (x) `(sqrt (square ,x)))
	(lambda (x) `(square (sqrt ,x)))
	(lambda (x) `(log (exp ,x)))
	(lambda (x) (let ([cancel-term (rand-term)]) `(* ,x (/ ,cancel-term ,cancel-term))))
	(lambda (x) (let ([cancel-term (rand-term)]) `(/ ,x (/ ,cancel-term ,cancel-term))))
	(lambda (x) (let ([cancel-term (rand-term)]) `(+ ,x (- ,cancel-term ,cancel-term))))
	(lambda (x) (let ([cancel-term (rand-term)]) `(- ,x (- ,cancel-term ,cancel-term))))))

(define (rand-trans)
  (list-ref transformations (random (length transformations))))

(define (rand-bool)
  (= 0 (random 2)))

(define (at-random-loc expr f)
  (let ([stop? (rand-bool)])
    (cond [(or (symbol? expr) (real? expr) stop?)
	   (f expr)]
	  [(= 3 (length expr))
	   (let ([left? (rand-bool)])
	     (if left?
		 (list (car expr) (at-random-loc (cadr expr) f) (caddr expr))
		 (list (car expr) (cadr expr) (at-random-loc (caddr expr) f))))]
	  [#t (list (car expr) (at-random-loc (cadr expr) f))])))

(define (gen-rand-prog iters)
  (let loop ([cur-prog '1] [steps iters])
    (if (= 0 steps)
	cur-prog
	(loop (at-random-loc cur-prog (rand-trans)) (- steps 1)))))

(define (get-test-progs iters)
  (map gen-rand-prog
       (map (lambda (x) (+ 2 (random 4)))
	    (make-list iters '()))))

(define (test-simplify-time iters)
  (let* ([test-cases (get-test-progs iters)]
         [simplified-progs (time (map (lambda (x) (cons x (simplify-expression x)))
				       test-cases))]
	 [correct? (andmap (lambda (x) (number? (cdr x)))
			   simplified-progs)])
    (when (not correct?) (let ([fail-progs (filter (lambda (x) (not (number? (cdr x)))) simplified-progs)])
			   (map (lambda (prog) (println "Incorrectly simplified " (car prog) " to " (cdr prog)))
				fail-progs)))
	correct?))
