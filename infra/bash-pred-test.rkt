#lang racket

(require "../src/common.rkt")
(require "../src/points.rkt")
(require "../src/formats/test.rkt")
(require "../src/main.rkt")
(require "../src/alternative.rkt")
(require "../src/programs.rkt")
(require racket/engine)

(define *seed* #f)
(define *timeout* (* 1000 60 10))
(define *lenient* #f)
(define *reeval-pts* 8000)

(define (bash-pred-test benchdir testname)
  (let* ([all-tests (load-tests benchdir)]
	 [test (findf (λ (t) (string=? (test-name t) testname)) all-tests)])
    (if (not test) (begin (println "Couldn't find the test!") (exit 1))
	(let ([eng (engine (λ _ (improve (test-program test) (*num-iterations*)
					 #:samplers (test-samplers test))))])
	  (begin (engine-run *timeout* eng)
		 (let ([result-alt (engine-result eng)])
		   (cond
		    [result-alt
		     (define newcontext
		       (parameterize ([*num-points* *reeval-pts*])
			 (prepare-points (test-program test) (test-samplers test))))
		     (match-define (list newpoints newexacts) (sorted-context-list newcontext 0))
		     (let* ([start-errors (errors (test-program test) newcontext)]
			    [end-errors (errors (alt-program result-alt) newcontext)]
			    [start-score (errors-score start-errors)]
			    [end-score (errors-score end-errors)])
		       (if (not (test-output test)) (if (start-score . < . end-score) (exit 0) (exit 1))
			   (let* ([target-errors (errors `(λ ,(program-variables (test-program test))
							    ,(test-output test)) newcontext)]
				  [target-score (errors-score target-errors)])
			     (if (or *lenient* (target-score . <= . end-score)) (exit 0) (exit 1)))))]
		   [#f (println "Timeout.") (exit 1)])))))))

(command-line
 #:program "bash-pred-test"
 #:once-each
 [("-r") rs "The random seed vector to use in point generation."
  (set-seed! (read (open-input-string rs)))]
 [("-n") fu "The amount of 'fuel' to use"
  (*num-iterations* (string->number fu))]
 [("-s") points "The number of points to use during search"
  (*num-points* (string->number points))]
 [("-e") epoints "The number of points to use during eval"
  (set! *reeval-pts* epoints)]
 [("-t") timeout "The number of seconds to wait before killing the test."
  (set! *timeout* timeout)]
 [("-l") "Return true as long as we finish and are better than start program."
  (set! *lenient* #t)]
 #:args (benchdir testname)
 (bash-pred-test benchdir testname))
