#lang racket

(require "../common.rkt")
(require "../main.rkt")
(require "../points.rkt")
(require "../localize-error.rkt")

(struct session-data (alts pcontext pcontext-extended))

(define *graph-name* (make-parameter "graph.png"))

;; Starts a session with the herbie-web-viz. Returns a list of two
;; things: a table of objects to respond with mapped to their names,
;; and a session-data object representing the state of their session.
(define (start-session prog)
  (parameterize ([*start-prog* prog])
    (define samplers (map (curryr cons sample-default) (program-variables prog)))
    (define pcontext (parameterize ([*num-points* 64]) (prepare-points prog samplers)))
    (define pcontext-extended (parameterize ([*num-points* 1024]) (prepare-points prog samplers)))
    (parameterize ([*pcontext* context] [*analyze-context* context])
      (define alt (simplify-alt (make-alt prog)))
      (define session-dat (session-data (list alt) pcontext pcontext-extended))
      (define locs (localize-error alt))
      (define axis (find-best-axis alt))
      (define response
	(hash
	 'formula (texify-formula prog locs)
	 'error-graph (*graph-name*)
	 'loc-ranges (make-ranges alt locs axis)))
      (define content
	(hash
	 (*graph-name*) (graph-error alt axis)))
      (values content response))))
