#lang racket

(require casio/common)
(require casio/rules)

(provide (all-defined-out))

(define (location-parent loc)
  (reverse (cdr (reverse loc))))

(define (location-sibling loc)
  (if (<= (length loc) 1)
      #f
      (let ([loc* (reverse loc)])
        (cond
         [(= (car loc*) 1)
          (reverse (cons 2 (cdr loc*)))]
         [(= (car loc*) 2)
          (reverse (cons 1 (cdr loc*)))]
         [else
          #f]))))

;; Returns true if location 'a' is inside location 'b', false otherwise.
(define (is-inside? a b)
  (cond [(null? a) #f]
	[(null? b) #t]
	[(= (car a) (car b)) (is-inside? (cdr a) (cdr b))]
	[#t #f]))

;; Translates locations from a location within translations specified
;; by from-func to the location specified by to-func.
(define (translate-location loc translations from-func to-func)
  (debug "Translating" loc "in" translations "from" from-func "to" to-func
	 #:from 'translate-location #:tag 'info)

  (define (recurse translations)
    (if (null? translations)
	#f
	(let ([translation (car translations)])
	  (if (= 1 (length (from-func translation))) ;;Is the translation one-to-n?
	      (let ([tail (match-loc-fst loc
				     (car (from-func translation)))])
		(debug "Tail" tail "on" (car (from-func translation))
		       #:from 'translate-location #:tag 'info)
		(if tail
		    (map (lambda (l) (append l tail))
			 (to-func translation))
		    (recurse (cdr translations))))
	      (recurse (cdr translations))))))

  (recurse translations))

;; Simple location match utility function. If 'a' is a continutation of 'b',
;; such as in a='(2 1) b='(2), returns the tail of
;; 'a' after 'b', '(1). Visa-versa for 'b' as a continuation of 'a'. If
;; 'a' and 'b' diverge at some point before the end, returns false.
(define (match-loc a b)
  (cond [(null? a) b]
	[(null? b) a]
	[(= (car a) (car b)) (match-loc (cdr a) (cdr b))]
	[#t #f]))

(define (match-loc-fst inside outside)
  (cond [(null? outside) inside]
	[(null? inside) #f]
	[(= (car outside) (car inside))
	 (match-loc-fst (cdr inside) (cdr outside))]
	[#t #f]))

(define (rule-location-translations rule)
  (define (var-locs pattern loc)
    (cond
     [(list? pattern)
	(apply alist-append
               (idx-map (lambda (x idx)
                          (var-locs x (append loc (list idx))))
                        (cdr pattern) #:from 1))]
     [(number? pattern) '()]
     [(symbol? pattern) (list (cons pattern (list loc)))]
     [#t (error "Improper rule: " rule)]))
  (let ([in-locs (var-locs (rule-input rule) '())]
	[out-locs (var-locs (rule-output rule) '())])
    (map (lambda (x)
	   (list (cdr x) (cdr (assoc (car x) out-locs))))
	 in-locs)))
