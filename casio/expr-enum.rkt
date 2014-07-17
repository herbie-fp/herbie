#lang racket

(require casio/common)
(require casio/simplify)

(struct oprtr (symbol n-args))

(define *constants* `(1 0))
(define *operators* (list (oprtr '+ 2) (oprtr '- 2) (oprtr '- 1)
			  (oprtr '* 2) (oprtr '/ 2) (oprtr '/ 1)
			  (oprtr 'sqr 1) (oprtr 'sqrt 1) (oprtr 'exp 1)
			  (oprtr 'log 1) (oprtr 'sin 1) (oprtr 'cos 1)
			  (oprtr 'tan 1) (oprtr 'cotan 1)))

(define (gen-expr n vars)
  (if (= n 0)
      (append vars *constants*)
      (remove-duplicates (map simplify-expression*
			      (apply append
				     (for/list ([op *operators*])
				       (map (curry cons (oprtr-symbol op))
					    (apply list-product
						   (make-list (oprtr-n-args op)
							      (gen-expr (sub1 n) vars))))))))))
