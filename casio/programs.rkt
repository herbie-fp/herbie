#lang racket

; Programs are just lambda expressions
(define program-body caddr)
(define program-variables cadr)

(define (location-map fun list)
  (letrec ([loc-map (λ (list fun acc location)
		       (if (null? list)
			   acc
			   (loc-map (cdr list) fun 
				    (cons (fun (car list) location) acc)
				    (cons 'cdr location))))])
    (reverse (loc-map list fun '() '()))))


(define (location-induct
	 prog
	 #:toplevel [toplevel (λ (expr location) expr)] #:constant [constant (λ (c location) c)]
	 #:variable [variable (λ (x location) x)] #:primitive [primitive (λ (list location) list)]
	 #:symbol [symbol-table (λ (sym location) sym)])
  (define (inductor prog location)
    (cond
     [(real? prog) (constant prog (reverse location))]
     [(symbol? prog) (variable prog (reverse location))]
     [(and (list? prog) (memq (car prog) '(λ lambda)))
      (let ([body* (inductor (program-body prog) location)])
	(toplevel `(λ ,(program-variables prog) ,body*) (reverse location)))]
     [(list? prog)
      (primitive (cons (symbol-table (car prog) (reverse (cons 'car location)))
		       (location-map (λ (prog loc)
					(inductor prog (append (cons 'car (cons 'cdr loc)) location)))
				     (cdr prog)))
		 (reverse location))]))
  (inductor prog '()))

(define (program-induct
         prog
         #:toplevel [toplevel identity] #:constant [constant identity]
         #:variable [variable identity] #:primitive [primitive identity]
         #:symbol [symbol-table identity])

  (define ((ignore f) x y) (f x))

  (location-induct prog
    #:toplevel (ignore toplevel) #:constant (ignore constant) #:variable (ignore variable)
    #:primitive (ignore primitive) #:symbol (ignore symbol-table)))

(define (location-do loc prog f)
  (cond
   [(null? loc)
    (f prog)]
   [(eq? (car loc) 'car)
    (cons (location-do (cdr loc) (car prog) f) (cdr prog))]
   [(eq? (car loc) 'cdr)
    (cons (car prog) (location-do (cdr loc) (cdr prog) f))]))

(provide program-body program-variables location-induct program-induct location-do)
