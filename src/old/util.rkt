#lang racket

(require "../points.rkt")
(require "../alternative.rkt")
(require "../common.rkt")
(require "../core/matcher.rkt")
(require "../programs.rkt")
(require "../glue.rkt")
(require "../mainloop.rkt")
(require "../core/egraph.rkt")
(require "../syntax/rules.rkt")
(require "../plot.rkt")

(provide (all-defined-out))


(define (visualize alt #:marks [marks '()] #:axis [axis 0])
  (define pts (for/list ([(pt ex) (in-pcontext (*pcontext*))]) pt))
  (define errs (alt-errors alt))

  (define renderers
    (list* (error-avg errs pts #:axis axis) (error-points errs pts #:axis axis)
           (for/list ([x-val marks]) (error-mark x-val))))

  (apply herbie-plot renderers))


;; (define (saturate-iters expr)
;;   (let ([eg (mk-egraph expr)])
;;     (let loop ([iters-done 1])
;;       (let ([start-cnt (egraph-cnt eg)])
;; 	(one-iter eg (*simplify-rules*))
;; 	(printf "Did iter #~a, have ~a nodes.~n" iters-done (egraph-cnt eg))
;; 	(if (> (egraph-cnt eg) start-cnt)
;; 	    (loop (add1 iters-done))
;; 	    (sub1 iters-done))))))

(define (print-improve prog max-iters)
  (match-let ([`(,end-prog ,context) (run-improve prog max-iters #:get-context #t)])
    (parameterize ([*pcontext* context])
      (let ([start (make-alt prog)]
	    [end (make-alt end-prog)])
        (printf "Started at: ~a\n" start)
        (printf "Ended at: ~a\n" end)
        (printf "Improvement by an average of ~a bits of precision\n"
                 (- (errors-score (alt-errors start)) (errors-score (alt-errors end))))
        (void)))))

(define (prog-improvement prog1 prog2)
  (let-values ([(points exacts) (prepare-points prog1)])
    (- (errors-score (errors prog1 points exacts)) (errors-score (errors prog2 points exacts)))))

(define (annotated-alts-compare alt1 alt2)
  (match-let ([(list pts exs) (sorted-context-list (*pcontext*) 0)])
    (parameterize ([*pcontext* (mk-pcontext pts exs)])
      (annotated-errors-compare (alt-errors alt1) (alt-errors alt2)))))

(define (annotated-errors-compare errs1 errs2)
  (printf "~a\n"
   (reverse
    (first-value
     (for/fold ([acc '()] [region #f])
	 ([err-diff (for/list ([e1 errs1] [e2 errs2])
		      (cond [(> e1 e2) '>]
			    [(< e1 e2) '<]
			    [#t '=]))]
	  [(pt _) (in-pcontext (*pcontext*))])
       (if (eq? region err-diff)
	   (values (cons err-diff acc)
		   region)
	   (values (cons (cons pt err-diff) acc)
		   err-diff)))))))

(define (compare-alts . altns)
  (printf "~a\n"
   (reverse
    (first-value
     (for/fold ([acc '()] [region-idx -1])
	 ([(pt ex) (in-pcontext (*pcontext*))]
	  [errs (flip-lists (map alt-errors altns))])
       (let ([best-idx
	      (argmin (curry list-ref errs) (range (length altns)))])
	 (if (= best-idx region-idx)
	     (values (cons best-idx acc) region-idx)
	     (values (cons (list best-idx (list-ref altns best-idx) pt)
			   acc)
		     best-idx))))))))

(define (print-alt-info altn)
  (if (not (alt-prev altn))
      (printf "Started with: ~a\n" (alt-program altn))
      (begin (print-alt-info (alt-prev altn))
             (let ([chng (alt-change altn)])
               (printf "Applied rule ~a at ~a [ ~a ], and got: ~a\n"
                       (change-rule chng) (change-location chng)
                       (location-get (change-location chng)
                                     (alt-program (alt-prev altn)))
                        (alt-program altn))
               (void)))))

(define (incremental-changes-apply changes expr)
  (let loop ([rest-chngs changes] [cur-expr expr])
    (if (null? rest-chngs)
	cur-expr
	(begin (printf "~a\n" cur-expr)
	       (printf "~a\n" (car rest-chngs))
	       (loop (cdr rest-chngs) (change-apply (car rest-chngs) cur-expr))))))
