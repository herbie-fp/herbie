#lang racket

(require racket/runtime-path)
(require "egraph-conversion.rkt" "../timeline.rkt"
	    "../syntax/types.rkt" "../points.rkt")
(require (for-syntax syntax/parse))
(module+ test (require rackunit))

(provide run-egglog)

(define-runtime-path egglog-binary
  "egg-smol/target/release/egg-smol")

(define egg-iters 100)
(define ground-truth-iters 10)
(define compute-accuracy-iters 10)
(define egg-node-limit 200000)
(define egg-match-limit 1000)
(define egg-if-match-limit 100000)
(define HIGH-COST 100000000)
;; Number of points from the point context to take
(define egg-num-sample 4)


(define (add-to-ruleset ruleset commands)
  	(for/list ([command commands])
          		(match command
                          [`(rule ,args ...)
                           `(rule ,@args :ruleset ,ruleset)]
													[`(rewrite ,args ...)
													 `(rewrite ,@args :ruleset ,ruleset)]
                          [else command])))


;; The egglog rules follow error-preserving semantics
;; If a program in the egraph errors, then it's okay to rewrite it to
;; something that errors for exactly the same input points
;; It is not okay to rewrite it to something that errors on fewer points

;; Operations that error on some point of the domain include:
;; Div, Pow, Log, Log1p, Sqrt, Tan, asin, acos, atan2

(define op-type (make-hash))

;; operators that do not error given their children don't error
(define safe-ops
	(list->set `(Add Sub Mul Fma If Less LessEq Greater GreaterEq Eq NotEq
		And Or)))

(define bool-ops
  `(TRUE FALSE))
(for ([op bool-ops])
  (hash-set! op-type op (list 'bool)))
(define num-ops
  `(PI E INFINITY NAN))
(for ([op num-ops])
  (hash-set! op-type op (list 'num)))
(define *-ops
  (append num-ops bool-ops))


(define bool-bool-ops
  `(Not))
(for ([op bool-bool-ops])
  (hash-set! op-type op (list 'bool 'bool)))
(define num-num-ops
  `(Neg Sqrt Cbrt Fabs Ceil Floor Round Log Exp
        	   Sin Cos Tan Asin Acos Atan Sinh Cosh Tanh Asinh Acosh Atanh Expm1 Log1p))
(for ([op num-num-ops])
  (hash-set! op-type op (list 'num 'num)))
(define *-*-ops
  (append bool-bool-ops num-num-ops))


(define num-num-bool-ops
  (list 'Less 'LessEq 'Greater 'GreaterEq 'Eq 'NotEq))
(for ([op num-num-bool-ops])
  (hash-set! op-type op (list 'num 'num 'bool)))
(define num-num-num-ops
  `(Add Sub Mul Div Pow Atan2 Hypot Copysign Fmod))
(for ([op num-num-num-ops])
  (hash-set! op-type op (list 'num 'num 'num)))
(define bool-bool-bool-ops
  `(And Or))
(for ([op bool-bool-bool-ops])
  (hash-set! op-type op (list 'bool 'bool 'bool)))
(define *-*-*-ops
  (append num-num-bool-ops num-num-num-ops
		bool-bool-bool-ops))

(define bool-num-num-num-ops
  `(If))
(for ([op bool-num-num-num-ops])
  (hash-set! op-type op (list 'bool 'num 'num 'num)))
(define num-num-num-num-ops
  `(Fma))
(for ([op num-num-num-num-ops])
  (hash-set! op-type op (list 'num 'num 'num 'num)))
(define *-*-*-*-ops
  (append num-num-num-num-ops bool-num-num-num-ops))

(define all-ops (append *-*-*-*-ops *-*-*-ops *-*-ops *-ops))
(define all-ops-except-if
	(filter (lambda (x) (not (equal? x 'If)))
									all-ops))

(define (type op n)
  (list-ref (hash-ref op-type op) n))

(define (return-type op)
	(last (hash-ref op-type op)))

(define (arity op)
	(- (length (hash-ref op-type op))
	   1))

(define (rep Op name)
	  (build-list (arity Op)
		  (lambda (v) (ivar name v))))

(define (tval op n)
  (match (list-ref (hash-ref op-type op) n)
    ['num 'ival]
    ['bool 'bval]))

(define (physical-repr type)
  (match type
	  ['num 'f64]
		['bool 'bool]))

(define (some op)
	(append-type 'Some op (arity op)))

(define (none op)
	(append-type
		'None op (arity op)))

(define (append-symbols . args)
	(string->symbol
		(apply string-append (map symbol->string args))))

(define (ivar variable n)
	(string->symbol
	  (string-append (symbol->string variable)
		               (format "~a" n))))

(define (ival-op op)
  (string->symbol
   (string-append "ival-" (symbol->string op))))

(define (ret-type op)
	(last (hash-ref op-type op)))

;; Appends the resulting type to the front of the name
(define (physical-op op)
  (string->symbol
   (string-append (symbol->string
	                  (physical-repr (ret-type op)))
	                "-" (symbol->string op))))

(define (append-type symbol op n)
  (string->symbol
   (string-append (symbol->string symbol) "-" (symbol->string (type op n)))))

(define-syntax (expand-for-list stx)
  (syntax-parse stx
    [(_expand-for-list list var:id body)
	#'(for/list ([ele list])
	    (let ([var ele])
		 body))]))


(define header
  `((set-option node_limit ,egg-node-limit)
		(set-option match_limit ,egg-match-limit)
    (datatype HerbieType (Type String))
    (datatype Math
		    ; Ground terms
		    (Num HerbieType Rational)
		    (Var HerbieType String)
		    ; comparison
		    ,@(expand-for-list *-*-*-*-ops Op
						   `(,Op HerbieType Math Math Math))
		    ,@(expand-for-list *-*-*-ops Op
						   `(,Op HerbieType Math Math))
		    ,@(expand-for-list *-*-ops Op
						   `(,Op HerbieType Math))
		    ,@(expand-for-list *-ops Op
						   `(,Op HerbieType)))
    ;; shorthands- must be added to the exclude list of extraction
    (define r-zero (rational "0" "1"))
    (define r-one (rational "1" "1"))
    (define r-two (rational "2" "1"))
    (define r-three (rational "3" "1"))
    (define r-four (rational "4" "1"))
    (define r-neg-one (rational "-1" "1"))
		(define r-half (rational "1" "2"))
		(define r-third (rational "1" "3"))
		))
(define egglog-consts (apply set `(ty r-zero r-one r-two r-three r-four r-neg-one r-half r-third)))

;; TODO: rewrite analysis to use interval type
(define analysis
  	(add-to-ruleset 'analysis
                        `((add-ruleset analysis)
                          		(function hi (Math) Rational :merge (min old new))
    (function lo (Math) Rational :merge (max old new))

    ;; Compute ground truth for a program for a particular input point indexed by the i64
		;; When the index is -1, computes a general interval arithmetic analysis
    (function ival (Math i64) Interval :merge (intersect old new))
    (function true-float (Math i64) f64 :merge (assert-eq old new))
    (function bval (Math i64) BooleanInterval :merge (ival-Or old new))
    (function true-bool (Math i64) bool :merge (assert-eq old new))
    (relation point (i64))


    ;; universe is a hack so we can quantify over it
    (relation universe (Math HerbieType))
    (relation non-zero (Math))
		;; TODO make non-error analysis better using intervals
		(relation non-error (Math))
    (relation non-negative (Math))
    (relation positive (Math))

    ;; First, constant folding!
    ;; We don't need an explicit constant folding analysis, we can just union
    ;; with nums when we can
    (rewrite (Add ty (Num ty a) (Num ty b))
		   (Num ty (+ a b)))
    (rewrite (Sub ty (Num ty a) (Num ty b))
		   (Num ty (- a b)))
    (rewrite (Mul ty (Num ty a) (Num ty b))
		   (Num ty (* a b)))
    (rewrite (Div ty (Num ty a) (Num ty b))
		   (Num ty (/ a b)))
    (rewrite (Pow ty (Num ty a) (Num ty b))
		   (Num ty (pow a b)))
    (rewrite (Neg ty (Num ty a)) (Num ty (neg a)))
    (rewrite (Sqrt ty (Num ty a))
		   (Num ty (sqrt a)))
    ;; TODO unimplemented
    ;; (rewrite (Cbrt (Num ty a)) (Num ty res) :when ((= res (cbrt a))))
    (rewrite (Fabs ty (Num ty a)) (Num ty (abs a)))
    (rewrite (Ceil ty (Num ty a)) (Num ty (ceil a)))
    (rewrite (Floor ty (Num ty a)) (Num ty (floor a)))
    (rewrite (Round ty (Num ty a)) (Num ty (round a)))
    (rewrite (Log ty (Num ty a))
		   (Num ty (log a)))
    ;; To check if something is zero, we check that zero is not contained in the
    ;; interval.
    (rule ((= l (lo e)) (> l r-zero)) ((non-zero e)))
    (rule ((= h (hi e)) (< h r-zero)) ((non-zero e)))
    (rule ((= l (lo e)) (>= l r-zero)) ((non-negative e)))
    (rule ((= l (lo e)) (> l r-zero)) ((positive e)))
    (rule ((= e (Num ty ve)))
		((set (lo e) ve) (set (hi e) ve)))

    ;; UNIVERSE
    (rule ((= t (Num ty a))) ((universe t ty)))
    (rule ((= t (Var ty a))) ((universe t ty)))
    (rule ((= t (PI ty))) ((universe t ty)))
    (rule ((= t (E ty))) ((universe t ty)))
    (rule ((= t (Add ty a b))) ((universe t ty)))
    (rule ((= t (Sub ty a b))) ((universe t ty)))
    (rule ((= t (Mul ty a b))) ((universe t ty)))
    (rule ((= t (Div ty a b))) ((universe t ty)))
    (rule ((= t (Pow ty a b))) ((universe t ty)))
    (rule ((= t (Neg ty a))) ((universe t ty)))
    (rule ((= t (Sqrt ty a))) ((universe t ty)))
    (rule ((= t (Cbrt ty a))) ((universe t ty)))
    (rule ((= t (Fabs ty a))) ((universe t ty)))
    (rule ((= t (Ceil ty a))) ((universe t ty)))
    (rule ((= t (Floor ty a))) ((universe t ty)))
    (rule ((= t (Round ty a))) ((universe t ty)))
    (rule ((= t (Log ty a))) ((universe t ty)))
    (rule ((= t (Exp ty a))) ((universe t ty)))
    (rule ((= t (If ty a b c))) ((universe t ty)))
    (rule ((= t (Fma ty a b c))) ((universe t ty)))
    (rule ((= t (Sin ty a))) ((universe t ty)))
    (rule ((= t (Cos ty a))) ((universe t ty)))
    (rule ((= t (Tan ty a))) ((universe t ty)))
    (rule ((= t (Atan ty a))) ((universe t ty)))
    (rule ((= t (Atan2 ty a b))) ((universe t ty)))
    (rule ((= t (Asin ty a))) ((universe t ty)))
    (rule ((= t (Acos ty a))) ((universe t ty)))
    (rule ((= t (Hypot ty a b))) ((universe t ty)))
    (rule ((= t (Expm1 ty a))) ((universe t ty)))
    (rule ((= t (Log1p ty a))) ((universe t ty)))


		;; leverage interval analysis
		(rule ((ival-NonError 
							(ival term -1)))
					((non-error term)))
		(rule ((ival-Positive 
							(ival term -1)))
					((positive term)))
		(rule ((ival-NonNegative
							(ival term -1)))
					((non-negative term)))
		(rule ((ival-NonZero
							(ival term -1)))
					((non-zero term)))

		;; constant fold nan by using interval analysis
		(rule ((ival-GuaranteeError
							(ival term -1))
					 (universe term ty))
					((set (NAN ty) term)))

    ;; soundness check
    (rule ((= (Num ty n) (Num ty m)) (!= n m))
          		((panic "Unsoundness detected!"))))))

(define (if-permute-rule Op index)
	`(rule
		((= top
				(,Op ty 
						 ,@(for/list ([j (in-range (arity Op))])
									(if (equal? j index)
											`(If ty cond then else)
											(ivar 'var j))))))
		((set 
      	(If ty cond
						(,Op ty
							,@(for/list ([j (in-range (arity Op))])
									(if (equal? j index)
											`then
											(ivar 'var j)))
						)
						top)
			top))))

(define simplify-if
	(add-to-ruleset 'simplify-if
	`((add-ruleset simplify-if)

		(rewrite
			(If ty cond
				then
				then)
			then)
		(rewrite
				(If ty cond
					 (If ty cond then else1)
					 else2)
				(If ty cond then else2))
		(rewrite
				(If ty cond
					 then1
					 (If ty cond then2 else))
				(If ty cond then1 else)))))


(define if-permute
	(add-to-ruleset 'if-permute
		`((add-ruleset if-permute)
			,@(apply append
					(expand-for-list
						all-ops-except-if
						Op
						(for/list ([i (in-range (arity Op))])
							(if-permute-rule Op i))))
		)))

(define rewrites
  	(add-to-ruleset 'rewrites
                        `((add-ruleset rewrites)
    ;; injectivity rules don't seem to do anything
    #;((rule ((= t1 (Add ty a b)) (= t2 (Add ty a c))
						 (= t1 t2))
		((union b c)))
    (rule ((= t1 (Mul ty a b)) (= t2 (Mul ty a c))
						 (= t1 t2)
						 (non-zero a))
		((union b c)))
    (rule ((= t1 (Div ty a b)) (= t2 (Div ty a c))
						 (= t1 t2)
						 (non-zero a))
		((union b c)))
    (rule ((= t1 (Div ty a b)) (= t2 (Div ty c b))
						 (= t1 t2))
		((union a c)))
    (rule ((= t1 (Sqrt ty a)) (= t2 (Sqrt ty b)) (= t1 t2))
		((union a b)))
    (rule ((= t1 (Cbrt ty a)) (= t2 (Cbrt ty b)) (= t1 t2))
		((union a b))))
    ;; Commutativity
    (rewrite (Add ty a b) (Add ty b a))
    (rewrite (Mul ty a b) (Mul ty b a))
    ;; Associativity
    (rewrite (Add ty a (Add ty b c))
		   (Add ty (Add ty a b) c))
    (rewrite (Add ty (Add ty a b) c)
		   (Add ty a (Add ty b c)))
    (rewrite (Add ty a (Sub ty b c))
		   (Sub ty (Add ty a b) c))
    (rewrite (Add ty (Sub ty a b) c)
		   (Sub ty a (Sub ty b c)))
    (rewrite (Sub ty a (Add ty b c))
		   (Sub ty (Sub ty a b) c))
    (rewrite (Sub ty (Add ty a b) c)
		   (Add ty a (Sub ty b c)))
    (rewrite (Sub ty (Sub ty a b) c)
		   (Sub ty a (Add ty b c)))
    (rewrite (Sub ty a (Sub ty b c))
		   (Add ty (Sub ty a b) c))
    (rewrite (Mul ty a (Mul ty b c))
		   (Mul ty (Mul ty a b) c))
    (rewrite (Mul ty (Mul ty a b) c)
		   (Mul ty a (Mul ty b c)))
    (rewrite (Mul ty a (Div ty b c)) ;; not defined when c is zero
		   (Div ty (Mul ty a b) c))
    (rewrite (Mul ty (Div ty a b) c) ;; not defined when b is zero
		   (Div ty (Mul ty a c) b))
    (rewrite (Div ty a (Mul ty b c)) ;; not defined when b or c is zero
		   (Div ty (Div ty a b) c))
    (rewrite (Div ty (Mul ty b c) a) ;; not defined when a is zero
		   (Div ty b (Div ty a c))
		   :when
		   ((non-zero c)))
    (rewrite (Div ty a (Div ty b c)) ;; not defined when b or c is zero
		   (Mul ty (Div ty a b) c)
		   :precisely-when
		   ((non-zero c)))
    (rewrite (Div ty (Div ty b c) a) ;; not defined when a or c is zero
		   (Div ty b (Mul ty a c)))

    ;; Counting
    (rewrite (Add ty x x) (Mul ty (Num ty r-two) x))
    ;; Distributivity
    (rewrite (Mul ty a (Add ty b c))
		   (Add ty (Mul ty a b) (Mul ty a c)))
    (rewrite (Mul ty a (Add ty b c))
		   (Add ty (Mul ty b a) (Mul ty c a)))
    (rewrite (Add ty (Mul ty a b) (Mul ty a c))
		   (Mul ty a (Add ty b c)))
    (rewrite (Sub ty (Mul ty a b) (Mul ty a c))
		   (Mul ty a (Sub ty b c)))
    (rewrite (Add ty (Mul ty b a) (Mul ty c a))
		   (Mul ty a (Add ty b c)))
    (rewrite (Sub ty (Mul ty b a) (Mul ty c a))
		   (Mul ty a (Sub ty b c)))
    (rewrite (Add ty (Mul ty b a) a)
		   (Mul ty (Add ty b (Num ty r-one)) a))
    (rewrite (Add ty a (Mul ty c a))
		   (Mul ty (Add ty c (Num ty r-one)) a))
    (rewrite (Neg ty (Mul ty a b)) (Mul ty (Neg ty a) b))
    (rewrite (Neg ty (Mul ty a b)) (Mul ty a (Neg ty b)))
    (rewrite (Mul ty (Neg ty a) b) (Neg ty (Mul ty a b)))
    (rewrite (Mul ty a (Neg ty b)) (Neg ty (Mul ty a b)))
    (rewrite (Neg ty (Add ty a b))
		   (Add ty (Neg ty a) (Neg ty b)))
    (rewrite (Add ty (Neg ty a) (Neg ty b))
		   (Neg ty (Add ty a b)))
    (rewrite (Div ty (Neg ty a) b) ;; not defined when b is zero
		   (Neg ty (Div ty a b)))
    (rewrite (Neg ty (Div ty a b)) ;; not defined when b is zero
		   (Div ty (Neg ty a) b))
    (rewrite (Sub ty a (Mul ty (Neg ty b) c))
		   (Add ty a (Mul ty b c)))
    (rewrite (Sub ty a (Mul ty b c))
		   (Add ty a (Mul ty (Neg ty b) c)))
    ;; Difference of squares
    (rewrite (Mul ty (Mul ty a b) (Mul ty a b))
		   (Mul ty (Mul ty a a) (Mul ty b b)))
    (rewrite (Mul ty (Mul ty a a) (Mul ty b b))
		   (Mul ty (Mul ty a b) (Mul ty a b)))
    (rewrite (Sub ty (Mul ty a a) (Mul ty b b))
		   (Mul ty (Add ty a b) (Sub ty a b)))
    (rewrite (Sub ty (Mul ty a a) (Num ty r-one))
		   (Mul ty
			   (Add ty a (Num ty r-one))
			   (Sub ty a (Num ty r-one))))
    (rewrite (Add ty (Mul ty a a) (Neg ty (Num ty r-one)))
		   (Mul ty
			   (Add ty a (Num ty r-one))
			   (Sub ty a (Num ty r-one))))
    (rule
			((= e (Pow ty a b)) (= loa (lo a)) (> loa r-zero)) ;; always defined if a > 0
			((set (Pow ty a b)
		 		(Mul ty
			 		(Pow ty a (Div ty b (Num ty r-two)))
			 		(Pow ty a (Div ty b (Num ty r-two))))))) ;; so rhs always defined
    (rewrite (Mul ty (Pow ty a b) (Pow ty a b))
		   (Pow ty a (Mul ty (Num ty r-two) b)) ;; could be more defined for fractional b
		   :when ((non-negative a))) ;; always defined when a non-neg
    (rewrite (Pow ty a (Num ty r-three))
		   (Mul ty a (Mul ty a a)))
    (rewrite (Pow ty a (Num ty r-four))
		   (Mul ty
			   (Pow ty a (Num ty r-two))
			   (Pow ty a (Num ty r-two))))

    ;; Identity
    (rewrite
			(Div ty (Num ty r-one) (Div ty (Num ty r-one) x)) ;; not defined when x is zero
			x
			:precisely-when ((non-zero x)))
    (rewrite (Mul ty x (Div ty (Num ty r-one) x)) ;; not defined when x is zero
		   (Num ty r-one)
		   :when ((non-zero x)
			        (non-error x)
			 ))
    (rewrite (Mul ty (Div ty (Num ty r-one) x) x) ;; not defined when x is zero
		   (Num ty r-one)
		   :when ((non-zero x)
			 				(non-error x)
			 ))
    (rewrite (Sub ty x x) (Num ty r-zero) :when ((non-error x)))
    (rewrite (Div ty x x) ;; not defined for x=0
		   (Num ty r-one)
		   :when ((non-zero x)
			        (non-error x)))
    (rewrite (Div ty (Num ty r-zero) x)
		   (Num ty r-zero)
		   :when ((non-zero x)
			 				(non-error x)
			 )) ;; not defined for x=0
    (rewrite (Mul ty (Num ty r-zero) x) (Num ty r-zero)
						 :when ((non-error x)))
    (rewrite (Mul ty x (Num ty r-zero)) (Num ty r-zero)
						:when ((non-error x)))
    (rewrite (Add ty (Num ty r-zero) x) x)
    (rewrite (Add ty x (Num ty r-zero)) x)
    (rewrite (Sub ty (Num ty r-zero) x) (Neg ty x))
    (rewrite (Sub ty x (Num ty r-zero)) x)
    (rewrite (Neg ty (Neg ty x)) x)
    (rewrite (Mul ty (Num ty r-one) x) x)
    (rewrite (Mul ty x (Num ty r-one)) x)
    (rewrite (Div ty x (Num ty r-one)) x) ;; always defined
    (rewrite (Mul ty (Num ty r-neg-one) x) (Neg ty x))
    (rewrite (Sub ty a b) (Add ty a (Neg ty b)))
    (rewrite (Sub ty a b) (Neg ty (Sub ty b a)))
    (rewrite (Add ty a (Neg ty b)) (Sub ty a b))
    (rewrite (Neg ty x) (Sub ty (Num ty r-zero) x))
    (rewrite (Neg ty x) (Mul ty (Num ty r-neg-one) x))
    (rewrite (Div ty x y) ;; not defined for y=0
		   (Mul ty x (Div ty (Num ty r-one) y)))
    (rewrite (Mul ty x (Div ty (Num ty r-one) y)) ;; not defined for y=0
		   (Div ty x y))
    (rewrite (Div ty x y) ;; not defined when y is zero
		   (Div ty (Num ty r-one) (Div ty y x))
		   :when
		   ((non-zero x) (non-zero y)))
    (rule ((universe t ty))
		((union t (Mul ty (Num ty r-one) t))))
    ;; Fractions
    (rewrite (Div ty (Sub ty a b) c) ;; not defined when c zero
		   (Sub ty (Div ty a c) (Div ty b c)))
    (rewrite (Div ty (Mul ty a b) (Mul ty c d)) ;; not defined when c or d is zero
		   (Mul ty (Div ty a c) (Div ty b d)))
    ;; Square root
    (rewrite (Mul ty (Sqrt ty x) (Sqrt ty x))
		   x
		   :precisely-when ((non-negative x)))
    (rewrite (Sqrt ty (Mul ty x x)) (Fabs ty x))
    (rewrite (Mul ty (Neg ty x) (Neg ty y)) (Mul ty x y))
    (rewrite (Mul ty (Fabs ty x) (Fabs ty x)) (Mul ty x x))
    ;; Absolute values
    (rewrite (Fabs ty (Fabs ty x)) (Fabs ty x))
    (rewrite (Fabs ty (Sub ty a b)) (Fabs ty (Sub ty b a)))
    (rewrite (Fabs ty (Neg ty x)) (Fabs ty x))
    (rewrite (Fabs ty (Mul ty x x)) (Mul ty x x))
    (rewrite (Fabs ty (Mul ty a b))
		   (Mul ty (Fabs ty a) (Fabs ty b)))
    (rewrite (Fabs ty (Div ty a b)) ;; not defined when b is zero
		   (Div ty (Fabs ty a) (Fabs ty b)))
    ;; Cube root
    (rewrite (Pow ty (Cbrt ty x) (Num ty r-three)) x)
    (rewrite (Cbrt ty (Pow ty x (Num ty r-three))) x)
    (rewrite
	(Mul ty (Mul ty (Cbrt ty x) (Cbrt ty x)) (Cbrt ty x))
	x)
    (rewrite
	(Mul ty (Cbrt ty x) (Mul ty (Cbrt ty x) (Cbrt ty x)))
	x)
    (rewrite (Pow ty (Neg ty x) (Num ty r-three))
		   (Neg ty (Pow ty x (Num ty r-three))))
    (rewrite (Pow ty (Mul ty x y) (Num ty r-three))
		   (Mul ty
			   (Pow ty x (Num ty r-three))
			   (Pow ty y (Num ty r-three))))
    (rewrite (Pow ty (Div ty x y) (Num ty r-three)) ;; not defined when y is zero
		   (Div ty
			   (Pow ty x (Num ty r-three))
			   (Pow ty y (Num ty r-three))))
    (rewrite (Pow ty x (Num ty r-three))
		   (Mul ty x (Mul ty x x)))
    ; FIXME: this rewrite is slow and has the potential to blow up the egraph
    ;        this is bc this rule and the second-to-last difference of squares rule
    ;        have some cyclic behavior goin on
    ;        the last identity rule compounds this behavior
    (rewrite (Mul ty x (Mul ty x x))
		   (Pow ty x (Num ty r-three)))
    ;; Exponentials
    (rewrite (Exp ty (Log ty x))
		   x :precisely-when ((positive x)))
    (rewrite (Log ty (Exp ty x)) x)
    (rewrite (Exp ty (Num ty r-zero)) (Num ty r-one))
    (rewrite (Exp ty (Num ty r-one)) (E ty))
    ;; (rewrite one               (Exp ty zero))
    (rewrite (E ty) (Exp ty (Num ty r-one)))
    (rewrite (Exp ty (Add ty a b))
		   (Mul ty (Exp ty a) (Exp ty b)))
    (rewrite (Exp ty (Sub ty a b))
		   (Div ty (Exp ty a) (Exp ty b))) ;; always defined
    (rewrite (Exp ty (Neg ty a))
		   (Div ty (Num ty r-one) (Exp ty a))) ;; always defined
    (rewrite (Mul ty (Exp ty a) (Exp ty b))
		   (Exp ty (Add ty a b)))
    (rewrite (Div ty (Num ty r-one) (Exp ty a)) ;; always defined
		   (Exp ty (Neg ty a)))
    (rewrite (Div ty (Exp ty a) (Exp ty b)) ;; always defined
		   (Exp ty (Sub ty a b)))
    (rewrite (Exp ty (Mul ty a b)) (Pow ty (Exp ty a) b)) ;; e^a is always pos, so always defined
    (rewrite (Exp ty (Div ty a (Num ty r-two))) ;; always defined
		   (Sqrt ty (Exp ty a))) ;; always defined
    (rewrite (Exp ty (Div ty a (Num ty r-three))) ;; always defined
		   (Cbrt ty (Exp ty a)))
    (rewrite (Exp ty (Mul ty a (Num ty r-two)))
		   (Mul ty (Exp ty a) (Exp ty a)))
    (rewrite (Exp ty (Mul ty a (Num ty r-three)))
		   (Pow ty (Exp ty a) (Num ty r-three)))
    ;; Powers
    (rewrite (Pow ty a (Num ty r-neg-one)) ;; not defined for a=0
		   (Div ty (Num ty r-one) a))
    (rewrite (Pow ty a (Num ty r-one)) a) ;; always defined
    (rewrite (Div ty (Num ty r-one) a) ;; not defined when a=0
		   (Pow ty a (Num ty r-neg-one)))
    (rule ((universe a ty))
		((union (Pow ty a (Num ty r-one)) a)))
    ; 0^0 is undefined
    (rewrite (Pow ty a (Num ty r-zero))
		   (Num ty r-one)
		   :when
		   ((non-zero a)
			  (non-error a)
			 ))
    (rewrite (Pow ty (Num ty r-one) a)
		         (Num ty r-one)
						 :when ((non-error a)))
    (rewrite (Exp ty (Mul ty (Log ty a) b)) ;; not defined for a <= 0
		   (Pow ty a b)
		   :precisely-when ((positive a)))
    (rewrite (Mul ty (Pow ty a b) a)
		   (Pow ty a (Add ty b (Num ty r-one)))
		   :when ((positive a)))
    (rewrite (Pow ty a (Num ty r-half))
		   (Sqrt ty a))
    (rewrite (Pow ty a (Num ty r-two)) (Mul ty a a))
    (rewrite (Pow ty a (Num ty r-third))
		   (Cbrt ty a))
    (rewrite (Pow ty a (Num ty r-three))
		   (Mul ty (Mul ty a a) a))
    ; 0^0  or 0 ^ negative is undefined
    (rewrite (Pow ty (Num ty r-zero) a)
		   (Num ty r-zero)
		   :when
		   ((positive a)
			  (non-error a)
			 ))
    ;; Logarithms
    (rewrite (Log ty (Mul ty a b))
		   (Add ty (Log ty a) (Log ty b))
		   :when
		   ((positive a) (positive b)))
    (rewrite (Log ty (Div ty a b)) ;; not defined for b=0
		   (Sub ty (Log ty a) (Log ty b))
		   :when
		   ((positive a) (positive b)))
    (rewrite (Log ty (Div ty (Num ty r-one) a)) ;; not defined for a=0
		   (Neg ty (Log ty a)) ;; not defined for a <= 0
		   :when ((positive a)))
    (rewrite (Log ty (Pow ty a b)) ;; not defined for b=0
		   (Mul ty b (Log ty a))
		   :when
		   ((positive a) (non-zero b)))
    (rewrite (Log ty (E ty)) (Num ty r-one))
    ;; Trigonometry
    (rewrite (Add ty
			   (Mul ty (Cos ty a) (Cos ty a))
			   (Mul ty (Sin ty a) (Sin ty a))) ;;verified
		   (Num ty r-one)
			 :when ((non-error a)))
    (rewrite
	(Sub ty
		(Num ty r-one)
		(Mul ty (Cos ty a) (Cos ty a)))
	(Mul ty (Sin ty a) (Sin ty a)))
    (rewrite
	(Sub ty
		(Num ty r-one)
		(Mul ty (Sin ty a) (Sin ty a)))
	(Mul ty (Cos ty a) (Cos ty a)))
    (rewrite (Add ty
			   (Mul ty (Cos ty a) (Cos ty a))
			   (Num ty r-neg-one))
		   (Neg ty (Mul ty (Sin ty a) (Sin ty a))))
    (rewrite (Add ty
			   (Mul ty (Sin ty a) (Sin ty a))
			   (Num ty r-neg-one))
		   (Neg ty (Mul ty (Cos ty a) (Cos ty a))))
    (rewrite (Sub ty
			   (Mul ty (Cos ty a) (Cos ty a))
			   (Num ty r-one))
		   (Neg ty (Mul ty (Sin ty a) (Sin ty a))))
    (rewrite (Sub ty
			   (Mul ty (Sin ty a) (Sin ty a))
			   (Num ty r-one))
		   (Neg ty (Mul ty (Cos ty a) (Cos ty a))))
    (rewrite
	(Sin ty
		(Div ty
			(PI ty)
			(Num ty (rational "6" "1")))) ;;verified
	(Num ty (rational "1" "2")))
    (rewrite
	(Sin ty
		(Div ty
			(PI ty)
			(Num ty (rational "4" "1"))))
	(Div ty (Sqrt ty (Num ty r-two)) (Num ty r-two)))
    (rewrite
	(Sin ty (Div ty (PI ty) (Num ty r-three))) ;;verified
	(Div ty (Sqrt ty (Num ty r-three)) (Num ty r-two)))
    (rewrite
	(Sin ty
		(Div ty (PI ty) (Num ty r-two))) ;;verified
	(Num ty r-one))
    (rewrite (Sin ty (PI ty)) ;;verified
		   (Num ty r-zero))
    (rewrite (Sin ty (Add ty x (PI ty)))
		   (Neg ty (Sin ty x)))
    (rewrite
	(Sin ty
		(Add ty
			x
			(Div ty (PI ty) (Num ty r-two))))
	(Cos ty x))
    (rewrite
	(Cos ty
		(Div ty
			(PI ty)
			(Num ty (rational "6" "1"))))
	(Div ty (Sqrt ty (Num ty r-three)) (Num ty r-two)))
    (rewrite
	(Cos ty
		(Div ty
			(PI ty)
			(Num ty (rational "4" "1"))))
	(Div ty (Sqrt ty (Num ty r-two)) (Num ty r-two)))
    (rewrite
	(Cos ty (Div ty (PI ty) (Num ty r-three)))
	(Num ty (rational "1" "2")))
    (rewrite
	(Cos ty (Div ty (PI ty) (Num ty r-two)))
	(Num ty r-zero))
    (rewrite (Cos ty (PI ty))
		   (Num ty (rational "-1" "1")))
    (rewrite (Cos ty (Add ty x (PI ty)))
		   (Neg ty (Cos ty x)))
    (rewrite
	(Cos ty
		(Add ty
			x
			(Div ty (PI ty) (Num ty r-two))))
	(Neg ty (Sin ty x)))
    (rewrite
	(Tan ty
		(Div ty
			(PI ty)
			(Num ty (rational "6" "1"))))
	(Div ty (Num ty r-one) (Sqrt ty (Num ty r-three))))
    (rewrite
	(Tan ty
		(Div ty
			(PI ty)
			(Num ty (rational "4" "1"))))
	(Num ty r-one))
    (rewrite
	(Tan ty (Div ty (PI ty) (Num ty r-three)))
	(Sqrt ty (Num ty r-three)))
    (rewrite (Tan ty (PI ty))
		   (Num ty r-zero))
    (rewrite (Tan ty (Add ty x (PI ty)))
		   (Tan ty x))
    (rewrite
			(Tan ty
				(Add ty
					x
					(Div ty (PI ty) (Num ty r-two))))
			(Div ty (Num ty r-neg-one) (Tan ty x)))
    (rewrite
			(Div ty
				(Sin ty a)
				(Add ty (Num ty r-one) (Cos ty a)))
			(Tan ty (Div ty a (Num ty r-two))))
    (rewrite
			(Div ty
				(Neg ty (Sin ty a))
				(Add ty (Num ty r-one) (Cos ty a)))
			(Tan ty (Div ty (Neg ty a) (Num ty r-two))))
    (rewrite
			(Div ty
			   (Sub ty (Num ty r-one) (Cos ty a))
			   (Sin ty a)) ;; not valid when (sin x) = 0
		   (Tan ty (Div ty a (Num ty r-two)))
			 :when ((non-zero a)))
    (rewrite (Div ty
			   (Sub ty (Num ty r-one) (Cos ty a))
			   (Neg ty (Sin ty a)))
		   (Tan ty (Div ty (Neg ty a) (Num ty r-two)))
			 :when ((non-zero a)))
    (rewrite
			(Div ty
				(Add ty (Sin ty a) (Sin ty b))
				(Add ty (Cos ty a) (Cos ty b)))
			(Tan ty (Div ty (Add ty a b) (Num ty r-two))))
    (rewrite (Div ty
			   (Sub ty (Sin ty a) (Sin ty b))
			   (Add ty (Cos ty a) (Cos ty b))) ;;verified
		   (Tan ty (Div ty (Sub ty a b) (Num ty r-two))))
    (rewrite (Sin ty (Num ty r-zero))
		   (Num ty r-zero))
    (rewrite (Cos ty (Num ty r-zero))
		   (Num ty r-one))
    (rewrite (Tan ty (Num ty r-zero))
		   (Num ty r-zero))
    (rewrite (Sin ty (Neg ty x))
		   (Neg ty (Sin ty x)))
    (rewrite (Cos ty (Neg ty x)) (Cos ty x))
    (rewrite (Tan ty (Neg ty x))
		   (Neg ty (Tan ty x)))
    ; Hyperbolics
    (rewrite (Sinh ty x) ;; always defined
		   (Div ty
			   (Sub ty (Exp ty x) (Exp ty (Neg ty x)))
			   (Num ty r-two)))
    (rewrite (Cosh ty x) ;; always defined
		   (Div ty
			   (Add ty (Exp ty x) (Exp ty (Neg ty x)))
			   (Num ty r-two)))
    (rewrite (Tanh ty x) ;; always defined
		   (Div ty
			   (Sub ty (Exp ty x) (Exp ty (Neg ty x)))
			   (Add ty (Exp ty x) (Exp ty (Neg ty x))))) ;; always defined
    (rewrite (Tanh ty x)
		   (Div ty
			   (Sub ty
				   (Exp ty (Mul ty (Num ty r-two) x))
				   (Num ty r-one))
			   (Add ty
				   (Exp ty (Mul ty (Num ty r-two) x))
				   (Num ty r-one)))) ;; always defined
    (rewrite
	(Tanh ty x)
	(Div
	 ty
	 (Sub ty
		 (Num ty r-one)
		 (Exp ty (Mul ty (Num ty (rational "-2" "1")) x)))
	 (Add
	  ty
	  (Num ty r-one)
	  (Exp ty (Mul ty (Num ty (rational "-2" "1")) x))))) ;; always defined
    (rewrite (Sub ty
			   (Mul ty (Cosh ty x) (Cosh ty x))
			   (Mul ty (Sinh ty x) (Sinh ty x)))
		   (Num ty r-one)
			 :when ((non-error x))
			 )
    (rewrite (Add ty (Cosh ty x) (Sinh ty x)) (Exp ty x))
    (rewrite (Sub ty (Cosh ty x) (Sinh ty x))
		   (Exp ty (Neg ty x)))

    ;; Difference of squares flip
    ;; demand for the rule
    (rule ((= (Add ty a b) t1)) ((Sub ty a b)))
    (rewrite (Add ty a b)
		   (Div ty
			   (Sub ty (Mul ty a a) (Mul ty b b))
			   (Sub ty a b)) ;; defined when a-b != 0
		   :when
		   ((non-zero (Sub ty a b))))
    ;; demand for the rule
    (rule ((= (Sub ty a b) t1)) ((Add ty a b)))
    (rewrite (Sub ty a b)
		   (Div ty
			   (Sub ty (Mul ty a a) (Mul ty b b))
			   (Add ty a b)) ;; defined when a+b != 0
		   :when
		   ((non-zero (Add ty a b))))
    ;; difference-of-cubes
    ;; sum of cubes
    (rewrite (Add ty
			   (Pow ty a (Num ty r-three))
			   (Pow ty b (Num ty r-three))) ;; always defined
		   (Mul ty
			   (Add ty
				   (Mul ty a a)
				   (Sub ty (Mul ty b b) (Mul ty a b)))
			   (Add ty a b)))
    ;; difference of cubes
    (rewrite (Sub ty
			   (Pow ty a (Num ty r-three))
			   (Pow ty b (Num ty r-three))) ;; always defined
		   (Mul ty
			   (Add ty
				   (Mul ty a a)
				   (Add ty (Mul ty b b) (Mul ty a b)))
			   (Sub ty a b)))
    ;; flip3-+
    ;; TODO: refactor this into an OR between all these conditions
    ;; demand
    (rule ((= t1 (Add ty a b)))
		((Add ty
			 (Mul ty a a)
			 (Sub ty (Mul ty b b) (Mul ty a b)))))
    (rewrite (Add ty a b)
		   (Div ty
			   (Add ty
				   (Pow ty a (Num ty r-three))
				   (Pow ty b (Num ty r-three)))
			   (Add ty
				   (Mul ty a a)
				   (Sub ty (Mul ty b b) (Mul ty a b))))
		   :when
		   ((non-zero
			(Add ty
				(Mul ty a a)
				(Sub ty (Mul ty b b) (Mul ty a b))))))
    (rewrite (Add ty a b)
		   (Div ty
			   (Add ty
				   (Pow ty a (Num ty r-three))
				   (Pow ty b (Num ty r-three)))
			   (Add ty
				   (Mul ty a a)
				   (Sub ty (Mul ty b b) (Mul ty a b))))
		   :when
		   ((non-zero a)))
    (rewrite (Add ty a b)
		   (Div ty
			   (Add ty
				   (Pow ty a (Num ty r-three))
				   (Pow ty b (Num ty r-three)))
			   (Add ty
				   (Mul ty a a)
				   (Sub ty (Mul ty b b) (Mul ty a b))))
		   :when
		   ((non-zero b)))
    ;;flip3--
    (rewrite
	(Sub ty a b)
	(Div ty
		(Sub ty
			(Pow ty a (Num ty r-three))
			(Pow ty b (Num ty r-three)))
		(Add ty
			(Mul ty a a)
			(Add ty (Mul ty b b) (Mul ty a b))))
	:when
	((non-zero
	  a))) ;; when a or b are non-zero a^2+b^2-ab is positive => a^2+b^2+ab is positive
    (rewrite (Sub ty a b)
		   (Div ty
			   (Sub ty
				   (Pow ty a (Num ty r-three))
				   (Pow ty b (Num ty r-three)))
			   (Add ty
				   (Mul ty a a)
				   (Add ty (Mul ty b b) (Mul ty a b))))
		   :when
		   ((non-zero b)))
    (rewrite (Sub ty a b)
		   (Div ty
			   (Sub ty
				   (Pow ty a (Num ty r-three))
				   (Pow ty b (Num ty r-three)))
			   (Add ty
				   (Mul ty a a)
				   (Add ty (Mul ty b b) (Mul ty a b))))
		   :when
		   ((non-zero
			(Add ty
				(Mul ty a a)
				(Add ty (Mul ty b b) (Mul ty a b))))))

    ;; The following rules that involve Pow often have over-specific conditions
    ;;fractions transform
    (rewrite (Sub ty (Div ty a c) (Div ty b c)) ;; not defined when c = 0
		   (Div ty (Sub ty a b) c))
    (rewrite (Add ty (Div ty a c) (Div ty b c)) ;; not defined when c = 0
		   (Div ty (Add ty a b) c))
    (rewrite (Add ty (Div ty a b) (Div ty c d)) ;; not define when b = 0 or when d = 0
		   (Div ty
			   (Add ty (Mul ty a d) (Mul ty b c))
			   (Mul ty b d)))
    (rewrite (Sub ty (Div ty a b) (Div ty c d)) ;; not define when b = 0 or when d = 0
		   (Div ty
			   (Sub ty (Mul ty a d) (Mul ty b c))
			   (Mul ty b d)))
    (rewrite (Mul ty (Div ty a b) (Div ty c d)) ;; not define when b = 0 or when d = 0
		   (Div ty (Mul ty a c) (Mul ty b d)))
    (rewrite (Div ty a b) (Div ty (Neg ty a) (Neg ty b))) ;; not defined when b = 0
    ;; squares transform
    (rewrite (Sqrt ty (Mul ty x y))
		   (Mul ty (Sqrt ty x) (Sqrt ty y)) ;; not defined for negative x or y
		   :when ((non-negative x) (non-negative y)))
    (rewrite (Sqrt ty (Div ty x y)) ;; not defined for negative x/y or y = 0
		   (Div ty (Sqrt ty x) (Sqrt ty y))
		   :when ((non-negative y) (non-negative x)))
    (rewrite (Sqrt ty (Pow ty x y)) ;; always defined for non-negative x
		   (Pow ty x (Div ty y (Num ty r-two)))
		   :when
		   ((non-negative x)))
    (rewrite (Pow ty (Sqrt ty x) y) ;; defined for non-negative x
		   (Pow ty x (Div ty y (Num ty r-two)))
		   :precisely-when ((non-negative x)))
    (rewrite (Mul ty (Sqrt ty x) (Sqrt ty y)) ;; x and y non-negative
		   (Sqrt ty (Mul ty x y))
		   :precisely-when ((non-negative x) (non-negative y)))
    (rewrite (Div ty (Sqrt ty x) (Sqrt ty y)) ;; x non-negative, y positive
		   (Sqrt ty (Div ty x y)) ;; x*y non-negative, y non-zero
		   :when
		   ((positive y) (non-negative x)))
    (rule ((universe t ty) (non-negative t))
		((union t (Mul ty (Sqrt ty t) (Sqrt ty t)))))
    ;; cubes transform
    (rewrite (Cbrt ty (Mul ty x y))
		   (Mul ty (Cbrt ty x) (Cbrt ty y)))
    (rewrite (Cbrt ty (Div ty x y)) ;; not defined for y=0
		   (Div ty (Cbrt ty x) (Cbrt ty y)))
    (rewrite (Mul ty (Cbrt ty x) (Cbrt ty y))
		   (Cbrt ty (Mul ty x y)))
    (rewrite (Div ty (Cbrt ty x) (Cbrt ty y)) ;; not defined for y=0
		   (Cbrt ty (Div ty x y)))
    (rule ((universe x ty))
		((union x
			   (Mul ty
				   (Cbrt ty x)
				   (Mul ty (Cbrt ty x) (Cbrt ty x)))))) ;; always defined
    (rule ((universe x ty))
		((union x (Cbrt ty (Pow ty x (Num ty r-three)))))) ;; always defined
    (rewrite (Pow ty (Exp ty a) b) ;; always defined
		   (Exp ty (Mul ty a b)))
    (rewrite (Pow ty a b) ;; defined for non-negative a
		   (Exp ty (Mul ty (Log ty a) b)) ;; defined for positive a
		   :when
		   ((positive a)))
    (rewrite
	(Mul ty (Pow ty a b) (Pow ty a c))
	(Pow ty
		a
		(Add ty
			b
			c))
	:when ((positive a)))
    (rewrite (Mul ty (Pow ty b a) (Pow ty c a)) ;; valid when b and c are positive
		   (Pow ty
			   (Mul ty b c) ;; valid when b*c positive
			   a)
		   :when ((positive b) (positive c)))
    (rewrite (Pow ty (Pow ty a b) c) ;; a positive -> a^b positive
		   (Pow ty a (Mul ty b c))
		   :when
		   ((positive a)))
    (rewrite (Pow ty a (Neg ty b))
		   (Div ty (Num ty r-one) (Pow ty a b))
		   :when
		   ((non-zero a)))
    (rewrite
	(Div ty (Num ty r-one) (Pow ty a b))
	(Pow ty a (Neg ty b)) ;; could be more defined when a = 0
	:when ((non-zero a)))
    (rewrite (Div ty (Pow ty a b) (Pow ty a c))
		   (Pow ty a (Sub ty b c))
		   :when ((positive a)))
    (rewrite (Pow ty a (Sub ty b c))
		   (Div ty (Pow ty a b) (Pow ty a c))
		   :when
		   ((positive a)))
    (rewrite (Pow ty a (Mul ty b c))
		   (Pow ty (Pow ty a b) c)
		   :when
		   ((positive a) (positive (Pow ty a b))))
    (rewrite (Pow ty a (Add ty b c))
		   (Mul ty (Pow ty a b) (Pow ty a c))
		   :when
		   ((positive a)))
    (rewrite (Pow ty (Mul ty b c) a)
		   (Mul ty (Pow ty b a) (Pow ty c a))
		   :when
		   ((positive b)
		    (positive c))) ;; overly specific
    (rewrite
	(Sqrt ty a)
	(Pow ty a (Div ty (Num ty r-one) (Num ty r-two))))
    (rewrite (Mul ty a a) (Pow ty a (Num ty r-two)))
    (rewrite
	(Cbrt ty a)
	(Pow ty a (Div ty (Num ty r-one) (Num ty r-three))))
    (rewrite (Mul ty (Mul ty a a) a)
		   (Pow ty a (Num ty r-three)))
    ;; log factor
    (rewrite
	(Add ty (Log ty a) (Log ty b))
	(Log ty (Mul ty a b))
	:when
	((positive a) (positive b)))
    (rewrite
	(Sub ty (Log ty a) (Log ty b))
	(Log ty (Div ty a b))
	:when ((positive a) (positive b)))
    (rewrite
	(Neg ty (Log ty a)) ;; error when a <= 0
	(Log ty (Div ty (Num ty r-one) a)))

    ;; trig expand
    (rewrite
	(Sin ty (Add ty a b))
	(Add ty
		(Mul ty (Sin ty a) (Cos ty b))
		(Mul ty (Cos ty a) (Sin ty b))))
    (rewrite
	(Cos ty (Add ty a b))
	(Sub ty
		(Mul ty (Cos ty a) (Cos ty b))
		(Mul ty (Sin ty a) (Sin ty b))))
    (rewrite (Tan ty (Add ty x y))
		   (Div ty
			   (Add ty (Tan ty x) (Tan ty y))
			   (Sub ty
				   (Num ty r-one)
				   (Mul ty (Tan ty x) (Tan ty y)))))
    (rewrite (Sin ty (Sub ty x y))
		   (Sub ty
			   (Mul ty (Sin ty x) (Cos ty y))
			   (Mul ty (Cos ty x) (Sin ty y))))
    (rewrite (Cos ty (Sub ty x y))
		   (Add ty
			   (Mul ty (Cos ty x) (Cos ty y))
			   (Mul ty (Sin ty x) (Sin ty y))))
    (rewrite
	(Sin ty (Mul ty (Num ty r-two) x))
	(Mul ty (Num ty r-two) (Mul ty (Sin ty x) (Cos ty x))))
    (rewrite
	(Sin ty (Mul ty (Num ty r-three) x))
	(Sub ty
		(Mul ty (Num ty r-three) (Sin ty x))
		(Mul ty
			(Num ty r-four)
			(Pow ty (Sin ty x) (Num ty r-three)))))
    (rewrite
	(Mul ty
		(Num ty r-two)
		(Mul ty (Sin ty x) (Cos ty x)))
	(Sin ty (Mul ty (Num ty r-two) x)))
    (rewrite
	(Sub ty
		(Mul ty (Num ty r-three) (Sin ty x))
		(Mul ty
			(Num ty r-four)
			(Pow ty (Sin ty x) (Num ty r-three))))
	(Sin ty (Mul ty (Num ty r-three) x)))
    (rewrite (Cos ty (Mul ty (Num ty r-two) x))
		   (Sub ty
			   (Mul ty (Cos ty x) (Cos ty x))
			   (Mul ty (Sin ty x) (Sin ty x))))
    (rewrite (Cos ty (Mul ty (Num ty r-three) x))
		   (Sub ty
			   (Mul ty
				   (Num ty r-four)
				   (Pow ty (Cos ty x) (Num ty r-three)))
			   (Mul ty (Num ty r-three) (Cos ty x))))
    (rewrite (Sub ty
			   (Mul ty (Cos ty x) (Cos ty x))
			   (Mul ty (Sin ty x) (Sin ty x)))
		   (Cos ty (Mul ty (Num ty r-two) x)))
    (rewrite (Sub ty
			   (Mul ty
				   (Num ty r-four)
				   (Pow ty (Cos ty x) (Num ty r-three)))
			   (Mul ty (Num ty r-three) (Cos ty x)))
		   (Cos ty (Mul ty (Num ty r-three) x)))
    (rewrite (Mul ty (Sin ty x) (Sin ty x))
		   (Sub ty
			   (Num ty (rational "1" "2"))
			   (Mul ty
				   (Num ty (rational "1" "2"))
				   (Cos ty (Mul ty (Num ty r-two) x)))))
    (rewrite (Mul ty (Cos ty x) (Cos ty x))
		   (Add ty
			   (Num ty (rational "1" "2"))
			   (Mul ty
				   (Num ty (rational "1" "2"))
				   (Cos ty (Mul ty (Num ty r-two) x)))))
    (rewrite
	(Sub ty (Sin ty x) (Sin ty y))
	(Mul
	 ty
	 (Num ty r-two)
	 (Mul ty
		 (Sin ty (Div ty (Sub ty x y) (Num ty r-two)))
		 (Cos ty (Div ty (Add ty x y) (Num ty r-two))))))
    (rewrite
	(Sub ty (Cos ty x) (Cos ty y))
	(Mul
	 ty
	 (Neg ty (Num ty r-two))
	 (Mul ty
		 (Sin ty (Div ty (Add ty x y) (Num ty r-two)))
		 (Sin ty (Div ty (Sub ty x y) (Num ty r-two))))))
    (rewrite (Mul ty (Cos ty x) (Cos ty y))
		   (Div ty
			   (Add ty
				   (Cos ty (Add ty x y))
				   (Cos ty (Sub ty x y)))
			   (Num ty r-two)))
    (rewrite (Mul ty (Sin ty x) (Sin ty y))
		   (Div ty
			   (Sub ty
				   (Cos ty (Sub ty x y))
				   (Cos ty (Add ty x y)))
			   (Num ty r-two)))
    (rewrite (Mul ty (Sin ty x) (Cos ty y))
		   (Div ty
			   (Add ty
				   (Sin ty (Sub ty x y))
				   (Sin ty (Add ty x y)))
			   (Num ty r-two)))
    (rewrite (Sub ty (Atan ty x) (Atan ty y))
		   (Atan2 ty
				(Sub ty x y)
				(Add ty (Num ty r-one) (Mul ty x y))))
    (rewrite (Add ty (Atan ty x) (Atan ty y))
		   (Atan2 ty
				(Add ty x y)
				(Sub ty (Num ty r-one) (Mul ty x y))))
    (rewrite (Tan ty x) (Div ty (Sin ty x) (Cos ty x)))
    (rewrite (Div ty (Sin ty x) (Cos ty x)) (Tan ty x))
    (rewrite (Tan ty (Div ty (Add ty x y) (Num ty r-two)))
		   (Div ty
			   (Add ty (Sin ty x) (Sin ty y))
			   (Add ty (Cos ty x) (Cos ty y))))
    (rewrite (Tan ty (Div ty (Sub ty x y) (Num ty r-two)))
		   (Div ty
			   (Sub ty (Sin ty x) (Sin ty y))
			   (Add ty (Cos ty x) (Cos ty y))))
    (rewrite
	(Mul ty (Sin ty x) (Sin ty x))
	(Sub ty (Num ty r-one) (Mul ty (Cos ty x) (Cos ty x))))
    (rewrite
	(Mul ty (Cos ty x) (Cos ty x))
	(Sub ty (Num ty r-one) (Mul ty (Sin ty x) (Sin ty x))))
    ;; trig inverses
    (rule ((= t1 (Sin ty (Asin ty x))) (= l (lo t1))
							    (= h (hi t1))
							    (>= l r-neg-one)
							    (<= h r-one))
		((union x t1)))
    (rule ((= t1 (Cos ty (Acos ty x))) (= l (lo t1))
							    (= h (hi t1))
							    (>= l r-neg-one)
							    (<= h r-one))
		((union x t1)))
    (rewrite (Tan ty (Atan ty x)) x)
    (rule ((= t1 (Atan ty (Tan ty x))) (= l (lo t1))
							    (= h (hi t1))
							    (>= l r-neg-one)
							    (<= h r-one))
		((union x t1)))
    ;; specialied numerical functions
    (rewrite (Sub ty (Exp ty x) (Num ty r-one))
		   (Expm1 ty x))
    (rewrite (Log ty (Add ty (Num ty r-one) x))
		   (Log1p ty x))
    (rewrite (Log1p ty (Expm1 ty x)) x)
    (rule ((= t1 (Expm1 ty (Log1p ty x))) (= l (lo x))
								  (> l r-neg-one))
		((union x t1)))
    (rewrite (Sqrt ty (Add ty (Mul ty x x) (Mul ty y y)))
		   (Hypot ty x y))
    (rewrite (Sqrt ty (Add ty (Num ty r-one) (Mul ty y y)))
		   (Hypot ty (Num ty r-one) y))
    (rewrite (Add ty (Mul ty x y) z) (Fma ty x y z))
    (rewrite (Fma ty x y z) (Add ty (Mul ty x y) z))
    (rewrite (Sub ty (Mul ty x y) z)
		   (Fma ty x y (Neg ty z)))
    (rewrite (Expm1 ty x)
		   (Sub ty (Exp ty x) (Num ty r-one)))
    (rewrite (Log1p ty x)
		   (Log ty (Add ty (Num ty r-one) x)))
    (rewrite (Hypot ty x y)
             		   (Sqrt ty (Add ty (Mul ty x x) (Mul ty y y))))
									 

		;; Added after egglog rewrite							 
		;; ------------------------------------------------
		;; Pavel's rule that breaks unsound herbie
		(rewrite (Add ty a b)
		         (Mul ty a (Add ty (Num ty r-one) (Div ty b a)))
						 :when
						 ((non-zero a)))
		(rewrite
			(Div ty a b)
			(Div ty
				(Mul ty a b)
				(Mul ty b b)))

		;; End of rewrites
)))

;; the script adds a (point i) for every point i
;; it also adds a (point -1) for a general interval analysis
(define ground-truth
  	(add-to-ruleset
			'ground-truth
     `((add-ruleset ground-truth)
       (rule ((= term (Num ty r))
		          (point i))
						 ((set (ival term i) (interval r r))))
    (rule ((= term (PI ty))
		 (point i))
		((set (ival term i) (ival-Pi))))
    (rule ((= term (INFINITY ty))
		 (point i))
		((set (ival term i) (ival-Inf))))
    (rule ((= term (E ty))
		 (point i))
		((set (ival term i) (ival-E))))
		(rule ((= term (NAN ty))
			(point i))
		((set (ival term i) (ival-NAN))))
    (rule ((= term (TRUE ty))
		 (point i))
		((set (bval term i) (true-interval))))
    (rule ((= term (FALSE ty))
		 (point i))
		((set (bval term i) (false-interval))))

    ,@(expand-for-list *-*-ops Op
				   `(rule ((= term (,Op ty x))
						 (= x-interval (,(tval Op 0) x i)))
						((set (,(tval Op 1) term i)
							 (,(ival-op Op) x-interval)))))
    ,@(expand-for-list
				;; TODO re-enable POW
				(filter (lambda (x) (not (equal? x 'Pow)))
				*-*-*-ops) Op
				   `(rule ((= term (,Op ty x y))
						 (= x-interval (,(tval Op 0) x i))
						 (= y-interval (,(tval Op 1) y i)))
						((set (,(tval Op 2) term i)
							 (,(ival-op Op) x-interval y-interval)))))

    ,@(expand-for-list *-*-*-*-ops Op
				   `(rule ((= term (,Op ty x y z))
						 (= x-interval (,(tval Op 0) x i))
						 (= y-interval (,(tval Op 1) y i))
						 (= z-interval (,(tval Op 2) z i)))
						((set (,(tval Op 3) term i)
							 (,(ival-op Op) x-interval y-interval
										 z-interval)))))

    (rule ((= interval (ival term i)))
		((set (true-float term i) (to-f64 interval))))
    (rule ((= binterval (bval term i)))
          		((set (true-bool term i) (to-bool binterval)))))))

(define (varname i)
  (string->symbol
   (string-append "eggvar" (number->string i))))

(define (build-exprs ctx eggdata exprs)
  (for/list ([expr exprs] [i (in-naturals)])
    `(define ,(varname i)
	  ,(expr->egglog ctx expr eggdata)
	  :cost 10000000)))

(define (build-iter)
	`((set-option match_limit ,egg-match-limit)
		;; runs the interval analysis
		(run ground-truth 3) 
		;; runs normal analysis
		(run analysis 3)
		;; runs normal rewriting
		(run rewrites 1)
		
		(set-option match_limit ,egg-if-match-limit)
		;; simplify if statement stuff
		(run simplify-if 2)
		;; permute the order of if statements
		(run if-permute 1)))

(define (build-runner)
	(apply append
		(for/list
				([i (in-range egg-iters)])
				(build-iter))))

(define (build-extract exprs)
  (for/list ([expr exprs] [i (in-naturals)])
    `(extract ,(varname i))))

(define (ast-prefix op)
  (string->symbol (string-append "Ast" (symbol->string op))))

(define (remove-ast-prefix expr)
	(match expr
		[`(Type ,ty) `(Type ,ty)]
		[`(AstNum ,ty ,r) `(Num ,ty ,r)]
		[`(AstVar ,ty ,s) `(Var ,ty ,s)]
		[`(,with-prefix ,args ...) 
			(when (not 
				(equal? (substring (symbol->string with-prefix) 0 3) "Ast"))
				(error (format "Tried to remove Ast prefix from ~a" with-prefix)))
			`(,(string->symbol (substring (symbol->string with-prefix) 3))
				,@(map remove-ast-prefix args))]
		[else
			(error (format "Tried to remove Ast prefix from ~a" expr))]))


(define (build-math-ast ctx exprs eggdata)
  `((datatype AstMath
		    (AstNum HerbieType Rational)
		    (AstVar HerbieType String)
		    ,@(expand-for-list all-ops Op
						   `(,(ast-prefix Op) HerbieType
							    ,@(build-list (arity Op) (lambda (x)  'AstMath))))
				)))


(define (make-extract-rules need-existing)
	(expand-for-list
	   all-ops Op
	   `(rule (
			 (= term (,Op ty ,@(rep Op 'c)))
			 (point i)
			 ;; floating point values for the children
			 ,@(build-list (arity Op)
			      (lambda (v)
						   `(= (,(append-type 'Some Op v) ,(ivar 'val v)) (,(append-type 'mostaccurate Op v) i ,(ivar 'c v)))))
			 ;; AstMath for the children
			 ,@(build-list (arity Op)
			      (lambda (v)
						   `(= ,(ivar 'child v) (mostaccurate i ,(ivar 'c v)))))
			 ;; the ground truth
			 (= true-physical
			    ,(if (equal? (return-type Op) 'num)
					     `(true-float term i)
							 `(true-bool term i)))

			 (= current-physical
			    (,(physical-op Op) ,@(rep Op 'val)))
			 ;; evaluate and compare to ground truth
			 (= difference
			    (rel-error
					 current-physical
					 true-physical))
			
			 ,@(if need-existing
					`((= (,(some Op) current-mostnum)
							(,(append-type 'mostaccurate Op (arity Op)) i term))
							
					(= to-beat
							(rel-error current-mostnum
										true-physical))
					(< difference to-beat))
				 `(

						(= (,(none Op))
							(,(append-type 'mostaccurate Op (arity Op)) i term))
				)
			 ))
			((set (mostaccurate i term)
				 (,(ast-prefix Op) ty ,@(rep Op 'child)))
			 (set (,(append-type 'mostaccurate Op (arity Op)) i term)
			 	 (,(some Op) current-physical))
			 ))))

(define (build-ground-truth-extract ctx exprs eggdata)
  (add-to-ruleset 'compute-accuracy
                  	(append
   (build-math-ast ctx exprs eggdata)
   `((ruleset compute-accuracy)
		 (datatype Option-f64
		 		(Some-num f64)
				(None-num))

		 (datatype Option-bool
		 		(Some-bool bool)
				(None-bool))

		 (function mostaccurate (i64 Math) AstMath
			:cost ,HIGH-COST :merge new)
	   (function mostaccurate-num (i64 Math) Option-f64
			:merge new :default (None-num))
		 (function mostaccurate-bool (i64 Math) Option-bool
		 	:merge new :default (None-bool))

	;; if an eclass contains a num or variable it is
	;; the most accurate by definition
	(rule ((= term (Num ty n))
	       (point i)
				 (= true-physical (true-float term i)))
				((set (mostaccurate i term) (AstNum ty n))
				 (set (mostaccurate-num i term) (Some-num (to-f64 n)))))
	(rule ((= term (Var ty v))
	       (point i)
				 (= true-physical (true-float term i)))
				((set (mostaccurate i term) (AstVar ty v))
				 (set (mostaccurate-num i term) (Some-num true-physical))))
	(rule ((= term (Var ty v))
	       (point i)
				 (= true-physical (true-bool term i)))
				((set (mostaccurate i term) (AstVar ty v))
				 (set (mostaccurate-bool i term) (Some-bool true-physical))))

	;; initialize the most accurate value to default of None
	;; very ugly, but the best way in egglog right now?
	,@(expand-for-list
	   all-ops Op
	   `(rule (
			 (= term (,Op ty ,@(rep Op 'c)))
			 (point i)
			 )
			((,(append-type 'mostaccurate Op (arity Op)) i term)
			 )))

	,@(make-extract-rules #t)
	,@(make-extract-rules #f)

  (run compute-accuracy ,compute-accuracy-iters)
	
	;; Finally, extract out the best expr for each expr and each point
	,@(apply append
			(for/list ([expr exprs]
				         [ei (in-naturals)])
				(for/list ([point (in-range egg-num-sample)])
					`(extract (mostaccurate ,point ,(varname ei))))
			))
	))))



(define (build-ground-truth-compute ctx pctx exprs eggdata)
	(define points (for/list ([(point exact) (in-pcontext pctx)])
		point))
	(define shuffled (shuffle points))
	(append
		(for/list ([point shuffled]
		           [i (in-range egg-num-sample)])
		`(point ,i)) ;; initialize the number of points
		(apply append
			(for/list ([point shuffled]
						[i (in-range egg-num-sample)])
				(for/list ([var (context-vars ctx)]
						[num point])
				`(set (ival ,(expr->egglog ctx var eggdata)
							,i)
					(interval ,num ,num)))))
                		`((set-option node_limit 10000000)
											(set-option match_limit 10000000)
                		(run ground-truth ,ground-truth-iters)) ;; run ground truth computation

	;; TODO run ground truth extraction
	))

(define (build-interval-analysis ctx eggdata)
	`((point -1)
		,@(for/list ([var (context-vars ctx)])
				`(set (ival ,(expr->egglog ctx var eggdata) -1)
							(ival-Empty)))))

(define (build-egglog ctx pctx eggdata exprs accuracy-extract)
  (append
		header
		analysis
		rewrites
		if-permute
		simplify-if
		ground-truth
		(build-exprs ctx eggdata exprs)
		(build-interval-analysis ctx eggdata)
		(build-runner)
		
		(if (not accuracy-extract)
				(build-extract exprs) ;; normal extraction
    		(append 
					(build-ground-truth-compute ctx pctx exprs eggdata)
    			(build-ground-truth-extract ctx exprs eggdata)))
		))

(define (side-condition->bool condition)
	(match condition
		[`(non-zero ,expr)
			`(NotEq (Type "bool") ,expr (Num ty r-zero))]
		[`(non-negative ,expr)
			`(GreaterEq (Type "bool") ,expr (Num ty r-zero))]
		[`(positive ,expr)
			`(Greater (Type "bool") ,expr (Num ty r-zero))]
		[else (error (format "Failed to match side condition ~a" condition))]))

(define (side-conditions->bool conditions)
	(let loop
		([acc (side-condition->bool (first conditions))]
		 [conds (rest conditions)])
		(cond
			[(empty? conds) acc]
			[else 
				(loop `(And (Type "bool") ,acc ,(side-condition->bool (first conds))) (rest conds))])))

(define (safe-op op)
	(set-member? safe-ops op))

(define (possibly-errors expr)
	(match expr
		[`(,any-op ,exprs ...)
			(or (not (safe-op any-op))
					(ormap possibly-errors exprs))]
		[else
			#f]))

(define (cant-convert-condition condition)
	(match condition
		[`(non-error ,expr)
			#t]
		;; we can't convert conditions that are not on variables
		;; because the conditions may also error
		;; for example, if the condition contains a pow
		[`(,any-condition ,expression)
			(possibly-errors expression)]
		[else #f]))

(define (rewrite-if egglog-program)
	(apply append
		(for/list ([line egglog-program])
			(match line
			[`(rewrite ,lhs ,rhs :when (,conditions ...) ,other ...)
				(if (ormap cant-convert-condition conditions)
						(list line)
						(list
						`(rewrite ,lhs ,rhs :when (,@conditions) ,@other)
						`(rewrite ,lhs
								(If ty ,(side-conditions->bool conditions) ,rhs ,lhs) ,@other))
			)]
			[`(rewrite ,lhs ,rhs :precisely-when (,conditions ...) ,other ...)
				(define normal `(rewrite ,lhs ,rhs :when (,@conditions) ,@other))
				(if (ormap cant-convert-condition conditions)
						(list normal)
						(list
							normal
						`(rewrite ,lhs
								(If ty ,(side-conditions->bool conditions) ,rhs (NAN ty)) ,@other))
			)]
			[`(rewrite ,lhs ,rhs)
				(list line)]
			[`(rewrite ,lhs ,rhs :ruleset ,ruleset)
				(list line)]
			[`(rewrite ,args ...)
				(error (format "Failed to match rewrite in rewrite-if ~a" line))]
			[else
			(list line)]))))


(define (get-vars expr)
	(cond
		[(list? expr)
		(apply set-union (map get-vars (rest expr)))]
		[(symbol? expr) (set expr)]
		[else (set)]))

(define (get-non-error-vars conditions)
	(apply set
	(apply append
		(for/list ([condition conditions])
		(match condition
			[`(non-error ,v)
				(list v)]
			[else empty])))))

(define allowed-drop-variables
	(list->set
		`(else1 then2 then else cond droppable0 droppable1 droppable2 droppable3)))

(define (sanity-check-rewrites egglog-program)
	(for ([line egglog-program])
		(match line
		[`(rewrite ,lhs ,rhs ,other ...)
			(define non-error-vars
				(match other
					[`(:when ,cond ,otherdata ...)
						(get-non-error-vars cond)]
					[`(:precisely-when ,cond ,otherdata ...)
						(get-non-error-vars cond)]
					[else (set)]))

			(define vars (get-vars lhs))
			(define rhs-vars (get-vars rhs))
			(define not-contains
				(set->list 
					(set-subtract
					(set-subtract
					(set-subtract
						(set-subtract vars rhs-vars)
						egglog-consts)
				non-error-vars)
				allowed-drop-variables)))
			(when (not (empty? not-contains))
				(error (format "Rewrite rule ~a:\nContains variables that are not in the rhs: ~a" line not-contains)))
			]
		[else
		#t
		])))


(define (apply-egglog-macros egglog-program)
	(sanity-check-rewrites egglog-program)
	(define res (rewrite-if egglog-program))
	(sanity-check-rewrites res)
	res)

(define (extracted-mostaccurate? expr)
	(match expr
		[`(mostaccurate ,args ...) #f]
		[else #t]))

;; 0 variants means just extract the best expression
(define (run-egglog ctx pctx exprs #:accuracy-extract accuracy-extract)
	(println exprs)
	(define eggdata
	(egraph-data (make-hash)
				(make-hash)))
	(define-values (egglog-process egglog-output egglog-in err)
	(subprocess #f #f (current-error-port) egglog-binary))

	(define egglog-program
          		(apply ~s #:separator "\n"
                               			(apply-egglog-macros (build-egglog ctx pctx eggdata exprs accuracy-extract))))
	;; save the egglog program
  (timeline-push! 'egglog egglog-program)

	(displayln egglog-program egglog-in)
	(close-output-port egglog-in)
	

	(define results
		(if (not accuracy-extract)
			(for/list ([expr exprs])
				(read egglog-output))
			(for/list ([expr exprs])
				;; list of exprs for this expr at each point
				(map remove-ast-prefix
					(filter extracted-mostaccurate?
						(for/list ([i (in-range egg-num-sample)])
							;; just extracted one thing
							(first (read egglog-output))))))))

	(close-input-port egglog-output)


	(for ([result results])
		(when (equal? result eof)
			(displayln egglog-program)
		   (error "Egglog failed to produce a result")))

	
	(define converted
		(for/list ([variants results])
			(map (curry egglog->expr ctx eggdata) variants)))

	(subprocess-wait egglog-process)

	(pretty-print converted)

	converted)


(module+ test
	(check-false (possibly-errors `(Add a b)))
	(check-true (possibly-errors `(Add a (Pow b c)))))

