#lang racket

(require casio/common)
(require casio/points)
(require casio/rules)
(require casio/programs)
(require casio/alternative)
(require casio/analyze-local-error)
(require casio/simplify)
(require casio/main)

(provide shell)

(define ((sh/keep . kept) alts locs)
  (let* ([alts* (filter (curryr memq kept) alts)]
	 [locs* (filter (lambda (x) (memq (car x) alts*)) locs)])
    (values (void) alts* locs*)))

(define ((sh/drop . dropped) alts locs)
  (let* ([alts* (filter (negate (curryr memq dropped)) dropped)]
	 [locs* (filter (lambda (x) (memq (car x) alts*)) locs)])
    (values (void) alts* locs*)))

(define ((sh/list . args) alts locs)
  (values args alts locs))

(define (get-alt-loc loc)
  (if (alt? loc)
      (values loc '(cdr cdr car))
      (values (car loc) (cdr loc))))

(define (get-alt-number alt alts)
  (let loop ([idx 0] [alts alts])
    (cond
     [(null? alts)
      (error "Could not find alternative" alt alts)]
     [(eq? alt (car alts))
      idx]
     [else
      (loop (+ idx 1) (cdr alts))])))

(define ((sh/tree loc) alts locs)
  (let-values ([(alt loc) (get-alt-loc loc)])
    (values (alt-rewrite-tree alt #:root loc) alts locs)))

(define ((sh/expr loc) alts locs)
  (let-values ([(alt loc) (get-alt-loc loc)])
    (values (alt-rewrite-expression alt #:root loc) alts locs)))

(define ((sh/simpl loc) alts locs)
  (let-values ([(alt loc) (get-alt-loc loc)])
    (values (list (simplify alt)) alts locs)))

(define ((sh/join . lists) alts locs)
  (values (apply append lists) alts locs))

(define ((sh/only . alts*) alts locs)
  (values (void) (remove-duplicates (apply append alts*))
	  (filter (lambda (x) (memq (car x) alts*)) locs)))

(define ((sh/also . alts*) alts locs)
  (values (void) (remove-duplicates (apply append alts alts*))
	  locs))

(define ((sh/analyze alt) alts locs)
  (let ([locs* (map car (analyze-local-error alt))])
    (values (void) alts
	    (remove-duplicates (append locs (map (curry cons alt) locs*))))))

(define shell-ops
  `([keep ,sh/keep] [drop ,sh/drop]
    [only ,sh/only] [also ,sh/also] [@ ,sh/list] [join ,sh/join]
    [tree ,sh/tree] [expr ,sh/expr] [simpl ,sh/simpl]
    [analyze ,sh/analyze]))

(define (eval-symbol s alts locs)
  (cond
   [(eq? (string-ref (symbol->string s) 0) #\$)
    (list-ref alts (string->number (substring (symbol->string s) 1 (string-length (symbol->string s)))))]
   [(eq? (string-ref (symbol->string s) 0) #\@)
    (list-ref locs (string->number (substring (symbol->string s) 1 (string-length (symbol->string s)))))]
   [(eq? (string-ref (symbol->string s) 0) #\!)
    (let ([name (string->symbol (substring (symbol->string s) 1 (string-length (symbol->string s))))])
      (findf (lambda (rule) (eq? (rule-name rule) name)) *rules*))]
   [(assoc s shell-ops)
    (cadr (assoc s shell-ops))]
   [else
    (error "Cannot evaluate symbol" s)]))

(define (eval-expr expr alts locs)
  (cond
   [(list? expr)
    (let loop ([expr expr] [rexpr '()] [alts alts] [locs locs])
      (if (null? expr)
	  (let ([expr* (reverse rexpr)])
	    ((apply (car expr*) (cdr expr*)) alts locs))
	  (let-values ([(ret alts* locs*) (eval-expr (car expr) alts locs)])
	    (loop (cdr expr) (cons ret rexpr) alts* locs*))))]
   [(symbol? expr)
    (values (eval-symbol expr alts locs) alts locs)]))

(define (shell prog)
  (debug-reset)
  (define-values (points exacts) (prepare-points prog))
  (parameterize ([*points* points] [*exacts* exacts])
    (let ([orig (make-alt prog)])
      (let toploop ([alts (list orig)] [locs '()])

	(println "Alternatives: ")
	(let printloop ([alts alts] [idx 0])
	  (unless (null? alts)
	      (println "$" idx ": " (car alts))
	      (printloop (cdr alts) (+ idx 1))))

	(println "Locations: ") 
	(let printloop ([locs locs] [idx 0])
	  (unless (null? locs)
	    (println "@" idx ": " (location-get (cdar locs) (alt-program (caar locs)))
		     " in $" (get-alt-number (caar locs) alts))
	    (printloop (cdr locs) (+ idx 1))))

	(when (null? locs)
	  (println "; Use (analyze $x) to get some locations to use"))

	(display "casio> ")
	(let ([cmd (read)])
	  (if (equal? cmd '(done))
	      (car alts)
	      (let-values ([(ret alts* locs*) (eval-expr cmd alts locs)])
		(cond
		 [(void? ret)
		  'ok]
		 [(list? ret)
		  (for ([elt ret])
		    (println "  " elt))
		  (println "; Use (only ...) or (also ...) to consider new alternatives")]
		 [else (println ret)])

		(toploop (sort alts* <
                               #:key (λ (altn)
                                        (avg-bits-error (alt-errors altn))))
                         locs*))))))))
