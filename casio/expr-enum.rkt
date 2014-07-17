#lang racket

(require casio/common)

(struct oprtr (symbol n-args))

(define *constants* `(1 0))
(define *operators* (list (oprtr '+ 2) (oprtr '- 2) (oprtr '- 1)
			  (oprtr '* 2) (oprtr '/ 2) (oprtr '/ 1)
			  (oprtr 'sqr 1) (oprtr 'sqrt 1) (oprtr 'exp 1)
			  (oprtr 'log 1) (oprtr 'sin 1) (oprtr 'cos 1)
			  (oprtr 'tan 1) (oprtr 'cotan 1)))

(define (reduce-common expr)
  (match expr
    [`(* 1 ,a) a]
    [`(* ,a 1) a]
    [`(* 0 ,a) 0]
    [`(* ,a 0) 0]
    [`(/ ,a 0) +nan.0]
    [`(/ 0 ,a) a]
    [`(/ ,a 1) a]
    [`(/ 1 ,a) `(/ ,a)]
    [`(/ 0) +nan.0]
    [`(/ 1) 1]
    [`(+ 0 ,a) a]
    [`(+ ,a 0) a]
    [`(- ,a 0) a]
    [`(- 0 ,a) `(- ,a)]
    [`(- 0) 0]
    [`(sqrt 0) 0]
    [`(sqrt 1) 1]
    [`(sqr 0) 0]
    [`(sqr 1) 1]
    [`(exp 0) 1]
    [`(log 1) 0]
    [`(log 0) +nan.0]
    [`(cos 0) 1]
    [`(sin 0) 0]
    [`(tan 0) 0]
    [`(cotan 0) +nan.0]
    [`(- ,a ,a) 0]
    [`(/ ,a ,a) 1]
    [`(* ,a ,b) (if (expr<? a b)
		    `(* ,a ,b)
		    `(* ,b ,a))]
    [`(+ ,a ,b) (if (expr<? a b)
		    `(+ ,a ,b)
		    `(+ ,b ,a))]
    [_ expr]))

(define (filter-exprs exprs)
  (filter (λ (expr) (not (and (number? expr) (nan? expr))))
	  (remove-duplicates exprs)))

(define (expr<? expr1 expr2)
  (cond [(and (number? expr1) (number? expr2))
	 (< expr1 expr2)]
	[(number? expr1) #t]
	[(number? expr2) #f]
	[(and (symbol? expr1) (symbol? expr2))
	 (symbol<? expr1 expr2)]
	[(symbol? expr1) #t]
	[(symbol? expr2) #f]
	[(and (list? expr1) (list? expr2))
	 (expr<? (cadr expr1) (cadr expr2))]
	[#t (error "Something in this expression isn't a symbol, number, or list!" expr1 expr2)]))

(define (gen-expr n vars)
  (if (= n 0)
      (append vars *constants*)
      (filter-exprs (map reduce-common
			 (let ([sub-gens (gen-expr (sub1 n) vars)])
			   (apply append
				  (for/list ([op *operators*])
				    (map (curry cons (oprtr-symbol op))
					 (apply list-product
						(make-list (oprtr-n-args op)
							   sub-gens))))))))))
