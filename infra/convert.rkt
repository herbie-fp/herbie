#lang racket

(require "../src/common.rkt")

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

(define (var&dist expr)
  (match expr
    [(list var samp) (list var samp)]
    [var (list var 'default)]))

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
  (match-define (list (list vars samps) ...) (map var&dist vars*))

  (define (translate-prop old-name new-name [transformer identity])
    (if (dict-has-key? args old-name)
        (list new-name (transformer (dict-ref args old-name)))
        (list)))

  (define (translate-samplers)
    (define-values (samplers pre)
      (reap [samplers pre]
            (for ([var vars] [samp samps])
              (define samp*
                (match samp
                  [(list (and (or '> '< '<= '>=) op) (? number? lb) samp)
                   (pre (list op lb var))
                   samp]
                  [(list (and (or '> '< '<= '>=) op) samp (? number? ub))
                   (pre (list op var ub))
                   samp]
                  [(list (and (or '> '< '<= '>=) op) (? number? lb) samp (? number? ub))
                   (pre (list op lb var))
                   (pre (list op var ub))
                   samp]
                  [_ samp]))
              (unless (equal? samp* 'default)
                (samplers (list var samp*))))))
    (append
     (if (null? samplers) '() (list ':herbie-samplers samplers))
     (if (null? pre) '() (list ':pre (cons 'and pre)))))

  `(FPCore ,vars
    ,@(translate-samplers)
    ,@(translate-prop '#:name ':name)
    ,@(translate-prop '#:expected ':herbie-expected)
    ,@(translate-prop '#:target ':herbie-target (curryr search-replace vars))
    ,(search-replace body vars)))

; we assume vars and vals are of the same length
(define (expand-let* vars vals body)
	(if (and (null? vars) (null? vals))
			body
			`(let ([,(car vars) ,(car vals)])
				,(expand-let* (cdr vars) (cdr vals) body))))

(define (search-replace expr bound)
	(match expr
	 [`(let* ([,vars ,vals] ...) ,body)
    (define vals*
      (let loop ([vars vars] [vals vals] [bound bound])
        (if (null? vars)
            '()
            (cons (search-replace (car vals) bound)
                  (loop (cdr vars) (cdr vals) (cons (car vars) bound))))))
	 	(expand-let* vars vals* (search-replace body (append vars bound)))]
   [(list (and (or 'abs 'expt 'mod) f) elements ...)
	  (define replacements '((abs . fabs) (expt . pow) (mod . fmod)))
    (cons (dict-ref replacements f) (map (curryr search-replace bound) elements))]
	 [(list elements ...)
		(map (curryr search-replace bound) elements)]
   [(or 'e 'pi)
    (define replacements '((e . E) (pi . PI)))
    (if (member expr bound) expr (dict-ref replacements expr))]
	 [_ expr]))

(module+ main
 (command-line
   #:program "herbie-to-fpcore"
   #:args (file)
   (for ([test (in-port read (open-input-file file))])
     (pretty-print (convert test) (current-output-port) 1))))

