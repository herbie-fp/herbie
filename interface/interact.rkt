#lang racket

(require herbie/common)
(require herbie/main)
(require herbie/programs)
(require herbie/points)
(require herbie/localize-error)
(require interface/visualize)

(define (setup-prog! prog #:samplers [samplers #f])
  (*start-prog* prog)
  (let* ([samplers (or samplers (map (curryr cons sample-default)
				     (program-variables prog)))]
	 [context (prepare-points prog samplers)])
    (*pcontext* context)
    (*analyze-context* ((flag 'localize 'cache) context #f))))
