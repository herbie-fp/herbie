#lang racket
(require "config.rkt")
(provide *debug* *debug-port* *debug-pref-range* debug set-debug-level!)

;; Sets how powerful, and therefore how computationally expensive, the
;; debug system is.
;;
;; * A value of 0 says to always print debug messages.
;;
;; * A value of 1 says to use the individual debug level settings as
;; controlled by set-debug-level!, but do not handle ranges. Note:
;; This was the default behavior on and before commit 4387b49.
;;
;; * A value of 2 says to do what 1 does, but also handle ranges.
;; Debug setting ranges indicate that the debug system should start
;; with the debug level of the low end of the range, and adjust within
;; the range to as much as possible get you a debug message no less
;; often than every (car (*debug-pref-range*)), but no more often than
;; every (cadr (*debug-pref-range*)).
(define-for-syntax _debug-system-complexity_ 2)

(define *debug* (make-parameter #f))
(define *debug-port* (make-parameter (current-error-port)))
(define *debug-pref-range* (make-parameter '(.025 .3)))
(define *last-time-printed* (/ (current-inexact-milliseconds) 1000))

(define (equal?-or-=? val1 val2)
  (if (and (number? val1) (number? val2))
      (= val1 val2)
      (equal? val1 val2)))

(define *cur-debug-levels* (make-hash))
;; To understand this it might be useful to understand the system.
;; The current level of any debug flag is fluid, within a range.
;; It's "setting" determines the range, or a fixed value,
;; and the "cur-debug-level" determines the actual current value.
(define cur-debug-level
  (λ (from)
    (let* ([cur-setting (get-setting from)]
	   [result (hash-ref! *cur-debug-levels* from
			      (if (list? cur-setting) (car cur-setting) cur-setting))])
      (cond [(and (list? cur-setting)
		  (or (not result)
		      (< result (car cur-setting))
		      (> result (cadr cur-setting))))
	       (hash-set! *cur-debug-levels* from (car cur-setting))
	       (car cur-setting)]
	    [(not (or (list? cur-setting) (equal?-or-=? cur-setting result)))
	     (hash-set! *cur-debug-levels* from cur-setting)
	     cur-setting]
	    [#t result]))))

(define *tags*
  #hasheq([misc  . "[misc]"]
          [enter . "[enter]"]
          [exit  . "[exit]"]
          [info  . "[info]"]
          [error . "[ERROR]"]))

;; To set a particular #:from max-depth, pass it in here.
;; To turn on all messages for a particular #:from, pass in a depth of #t.
;; To set the default, pass in a max depth with the #:from #t.
;; Also supports numeric ranges, which will be autoadjusted within according to *debug-pref-range*
(define (set-debug-level! from depth)
  (let ([existing (cond [(not (*debug*)) '((#t . 0))]
			[(eq? #t (*debug*)) '((#t . #t))]
			[#t (*debug*)])])
    (*debug* (cons (cons from depth) existing))))

(define-syntax should-print-debug?
  (cond [(= _debug-system-complexity_ 0)
	 (syntax-rules ()
	   [(should-print-debug? from depth) #t])]
	[(= _debug-system-complexity_ 1)
	 (syntax-rules ()
	   [(should-print-debug? from depth)
	    (let ([setting (get-setting from)])
	      (or (eq? setting #t) (and setting (>= setting depth) (> setting 0))))])]
	[#t
	 (syntax-rules ()
	   [(should-print-debug? from depth)
	    (begin (let ([setting (get-setting from)])
		     (when (list? setting)
		       (let* ([current-level (cur-debug-level from)]
			      [cur-time (/ (current-inexact-milliseconds) 1000)]
			      [time-passed (- cur-time *last-time-printed*)])
			 (cond [(and (< time-passed (car (*debug-pref-range*)))
				     (> current-level (car setting)))
				(hash-set! *cur-debug-levels* from (sub1 current-level))]
			       [(and (> time-passed (cadr (*debug-pref-range*)))
				     (< current-level (cadr setting)))
				(hash-set! *cur-debug-levels* from (add1 current-level))]))))
		   (let ([current-level (cur-debug-level from)])
		     (or (eq? current-level #t) (and current-level (>= current-level depth) (> current-level 0)))))])]))

(define (get-setting from)
  (cond [(equal? (*debug*) #t) #t]
	[(not (*debug*)) #f]
	[(and from (dict-has-key? (*debug*) from)) (dict-ref (*debug*) from)]
	[#t (dict-ref (*debug*) #t)]))

(define (debug #:from [from 'none] #:depth [depth 1] . args)
  (when (should-print-debug? from depth)
    (set! *last-time-printed* (/ (current-inexact-milliseconds) 1000))
    (debug-print from depth args (*debug-port*))))

(define debug-start-time (current-inexact-milliseconds))
(register-reset (λ () (set! debug-start-time (current-inexact-milliseconds))))

(define (debug-print from depth args port)
  (fprintf port "~a ~a [~a]:"
           (~r (/ (- (current-inexact-milliseconds) debug-start-time) 1000)
               #:precision '(= 3))
           (string-join (build-list depth (const "*")) " ")
           from)
  (for ([arg args]) (fprintf port " ~a" arg))
  (newline port)
  (flush-output port))
