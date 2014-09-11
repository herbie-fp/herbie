#lang racket

(provide *debug* *debug-port* debug)

(define *debug* (make-parameter #f))
(define *debug-port* (make-parameter (current-error-port)))

(define *tags*
  #hasheq([misc  . "[misc]"]
          [enter . "[enter]"]
          [exit  . "[exit]"]
          [info  . "[info]"]
          [error . "[ERROR]"]))

;; To set a particular #:from max-depth, pass it in here.
;; To turn on all messages for a particular #:from, pass in a depth of #t.
;; To set the default, pass in a max depth with the #:from #t.
(define (set-debug-level! from depth)
  (let ([existing (cond [(not (*debug*)) '((#t . 0))]
			[(eq? #t (*debug*)) '((#t . #t))]
			[#t (*debug*)])])
    (*debug* (cons (cons from depth) existing))))

(define (should-print-debug? from depth)
  (or (eq? (*debug*) #t) ;; If debug is true, print no matter what
      (and (*debug*) ;; If debug is false, never print
	   (let ([max-depth (if (and from (dict-has-key? (*debug*) from))
				;; If we were given a #:from, and we have it in the dictionary,
				;; look up it's max depth
				(dict-ref (*debug*) from)
				;; Otherwise, just use whatevers default.
				(dict-ref (*debug*) #t))])
	     ;; If the max depth is true, turn everything on.
	     ;; If the max depth isn't positve, turn everything off.
	     ;; Otherwise, if our dept is less than the max-depth,
	     ;; return true.
	     (or (eq? max-depth #t)
		 (and (>= max-depth depth)
		      (> max-depth 0)))))))

(define (debug #:from [from 'casio] #:tag [tag 'misc] #:depth [depth 1] . args)
  (when (should-print-debug? from depth)
    (debug-print from tag args (*debug-port*))))

(define (debug-print from tag args port)
  (display (hash-ref *tags* tag "; ") port)
  (write from port)
  (display ": " port)
  (for/list ([arg args])
    (display " " port)
    ((if (string? arg) display write) arg port))
  (newline port))
