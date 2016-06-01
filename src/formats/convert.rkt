#lang racket

(require "../common.rkt")

(provide convert)

(define (args&body* args)
  (match args
    [(list (? keyword? name) value args* ...)
     (define out* (args&body* args*))
     (cons (car out*) (cons (cons name value) (cdr out*)))]
    [(list body args* ...)
     (define out* (args&body* args*))
     (assert (not (car out*)) #:extra-info (λ () (format "Two body expressions ~a and ~a" (car out*) body)))
     (cons body (cdr out*))]
    [(list)
     (cons #f '())]))

(define (args&body args)
  (define out* (args&body* args))
  (assert (car out*) #:extra-info (λ () "No body expression"))
  out*)

; parse old herbie syntax into FPCore
(define (convert expr)
  (define-values (vars* args*)
    (match expr
      [(list 'herbie-test (list vars ...) (? string? name) input)
       (values vars (list '#:name name input))]
      [(list 'herbie-test (list vars ...) (? string? name) input output)
       (values vars (list '#:name name '#:target output input))]
      [(list 'herbie-test (list vars ...) input output)
       (values vars (list '#:name "Unnamed Test" '#:target output input))]
      [(list 'lambda (list vars ...) args ...)
       (values vars args)]
      [(list 'define name (list vars ...) args ...)
       (values vars (list*'#:name name args))]))
  (match-define (list body args ...) (args&body args*))

  (define (translate-prop old-name new-name [transformer identity])
    (let ([prop-value (dict-ref args old-name #f)])
      (if prop-value (list new-name (transformer prop-value)) (list))))

  `(FPCore ,vars*
    ,@(translate-prop '#:name ':name)
    ,@(translate-prop '#:expected ':herbie-expected)
    ,@(translate-prop '#:target ':target search-replace-let*)
    ,(search-replace-let* body)))

; we assume vars and vals are of the same length
(define (expand-let* vars vals body)
	(if (and (null? vars) (null? vals))
			body
			`(let ([,(car vars) ,(car vals)])
				,(expand-let* (cdr vars) (cdr vals) body))))

(define (search-replace-let* expr)
	(match expr
	 [`(let* ([,vars ,vals] ...) ,body)
	 	(expand-let* vars vals (search-replace-let* body))]
	 [(list elements ...)
		(map search-replace-let* elements)]
	 [_ expr]))

(module+ main
 (command-line
   #:program "herbie-to-fpcore"
   #:args (file)
   (for ([test (in-port read (open-input-file file))])
     (pretty-print (convert test) (current-output-port) 1))))

