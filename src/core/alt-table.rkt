#lang racket

(require "../common.rkt")
(require "../alternative.rkt")
(require "../points.rkt")
(require "../syntax/softposit.rkt")

(provide
 (contract-out
  (make-alt-table (pcontext? alt? . -> . alt-table?))
  (atab-all-alts (alt-table? . -> . (listof alt?)))
  (atab-not-done-alts (alt-table? . -> . (listof alt?)))
  (atab-add-altns (alt-table? (listof alt?) . -> . alt-table?))
  (atab-pick-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
			     #:only-fresh boolean?
			     . -> . (values alt? alt-table?)))
  (atab-peek-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
			     #:only-fresh boolean?
			     . -> . (values alt? alt-table?)))
  (atab-completed? (alt-table? . -> . boolean?))
  (atab-context (alt-table? . -> . pcontext?))
  (split-atab (alt-table? (non-empty-listof any/c) . -> . (listof alt-table?)))
  (atab-new-context (alt-table? pcontext? . -> . alt-table?))))

;; Public API

(struct alt-table (point->alts alt->points alt->done? context) #:prefab)

(define atab-context alt-table-context)

(define in-atab-pcontext (compose in-pcontext atab-context))

(define (make-alt-table context initial-alt)
   (alt-table (make-hash
     (for/list ([(pt ex) (in-pcontext context)]
                [err (errors (alt-program initial-alt) context)])
       (cons pt (point-rec err (list initial-alt)))))
     (hash initial-alt (for/list ([(pt ex) (in-pcontext context)])
       pt))
     (hash initial-alt #f)
     context))

(define (atab-new-context atab ctx)
  (let* ([old-done (alt-table-alt->done? atab)]
         [alts (atab-all-alts atab)]
         [table-init (make-alt-table ctx (car alts))])
    (alt-table-with
     (atab-add-altns table-init (cdr alts))
     #:alt->done? old-done)))

(define (atab-add-altns atab altns)
  (for/fold ([atab atab]) ([altn altns])
    (atab-add-altn atab altn)))

(define (atab-pick-alt atab #:picking-func [pick car]
		       #:only-fresh [only-fresh? #t])
  (let* ([picked (atab-peek-alt atab #:picking-func pick #:only-fresh only-fresh?)]
	 [atab* (alt-table-with atab #:alt->done? (hash-set (alt-table-alt->done? atab) picked #t))])
    (values picked atab*)))

(define (atab-peek-alt atab #:picking-func [pick car]
		       #:only-fresh [only-fresh? #f])
  (pick (if only-fresh?
	    (atab-not-done-alts atab)
	    (atab-all-alts atab))))

(define (atab-all-alts atab)
  (hash-keys (alt-table-alt->points atab)))

(define (atab-completed? atab)
  (andmap identity (hash-values (alt-table-alt->done? atab))))

;; Split the alt table into several alt tables, each of which corresponds to a pred
;; in 'preds', and only contains points which satisfy that pred.
(define (split-atab atab preds)
  (for/list ([pred preds])
    (let* ([point->alts (make-immutable-hash (for/list ([(pt ex) (in-atab-pcontext atab)]
							#:when (pred pt))
					       (cons pt (hash-ref (alt-table-point->alts atab) pt))))]
	   [alt->points (make-immutable-hash (filter (compose (negate null?) cdr)
						     (for/list ([(alt points)
								 (in-hash (alt-table-alt->points atab))])
						       (cons alt (filter pred points)))))]
	   [alt->done? (make-immutable-hash (for/list ([alt (in-hash-keys alt->points)])
					      (cons alt (hash-ref (alt-table-alt->done? atab) alt))))]
	   [context (call-with-values
			(λ () (for/lists (pts exs)
				  ([(pt ex) (in-atab-pcontext atab)]
				   #:when (pred pt))
				(values pt ex)))
		      mk-pcontext)])
      (minimize-alts (alt-table point->alts alt->points alt->done? context)))))

;; Helper Functions

(define (alt-table-with atab
			#:point->alts [pnt->alts #f]
			#:alt->points [alt->pnts #f]
			#:alt->done? [alt->done? #f]
			#:context [pcontext #f])
  (alt-table (or pnt->alts (alt-table-point->alts atab))
	     (or alt->pnts (alt-table-alt->points atab))
	     (or alt->done? (alt-table-alt->done? atab))
	     (or pcontext (alt-table-context atab))))

(define (alternate . lsts)
  (let loop ([rest-lsts lsts] [acc '()])
    (if (ormap null? rest-lsts)
	(reverse acc)
	(loop (map cdr rest-lsts) (append (reverse (map car rest-lsts)) acc)))))

(define (hash-set-lsts hash keys values)
  (apply hash-set* hash (alternate keys values)))

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

;; Implementation

(struct point-rec (berr altns) #:prefab)

(define (test-hash-ref lst key)
  (if (eq? null lst)
    (error "No Key!")
    (if (equal? (caar lst) key)
      (cdar lst)
      (test-hash-ref (cdr lst) key))))

(define (best-and-tied-at-points point->alt altn)
  (define a (for/list ([(p _) (in-pcontext (*pcontext*))])
              p))
  #;(println (sort (map car (map (λ (x) (map posit16->double x)) a)) <))
  #;(println (sort (map car (map (λ (x) (map posit16->double x)) (map car (hash->list point->alt)))) <))
  (define keys (hash-keys point->alt))
  (for ([point a])
    (if (set-member? keys point)
      '()
      (printf "missing key ~a\n" (list (posit16->double (car point))))))
  (let-values ([(best tied)
		(for/lists (best tied) ([(pnt ex) (in-pcontext (*pcontext*))]
                            [err (errors (alt-program altn) (*pcontext*))])
      (assert (equal? (hash-keys point->alt) keys))
      (assert (set-member? (hash-keys point->alt) pnt))
      (assert (hash-equal? point->alt))
      (for ([(k2 v) (in-hash point->alt)] #:when (hash-has-key? point->alt k2))
        (eprintf "~a -> ~a\n" k2 v))
      (eprintf "OK!\n")
      (for ([k1 (in-list (hash-keys point->alt))])
        (assert (hash-has-key? point->alt k1)))
      (eprintf "OK 2!\n")
      (assert (hash-has-key? point->alt pnt))
		  (let* ([pnt-rec (hash-ref point->alt pnt)]
			 [table-err (point-rec-berr pnt-rec)])
		    (cond [(< err table-err)
			   (values pnt #f)]
			  [(= err table-err)
			   (values #f pnt)]
			  [else (values #f #f)])))])
    (list (filter identity best) (filter identity tied))))

(define (remove-chnged-pnts point->alts alt->points chnged-pnts)
  (let* ([chnged-entries (map (curry hash-ref point->alts) chnged-pnts)]
	 [chnged-altns (remove-duplicates (append-map point-rec-altns chnged-entries))])
    (hash-set-lsts
     alt->points chnged-altns
     (map (λ (altn)
	    (remove* chnged-pnts (hash-ref alt->points altn)))
	  chnged-altns))))

(define (override-at-pnts points->alts pnts altn)
  (let ([pnt->alt-err (make-immutable-hash (for/list ([(pnt ex) (in-pcontext (*pcontext*))]
						      [err (errors (alt-program altn) (*pcontext*))])
					     (cons pnt err)))])
    (hash-set-lsts
     points->alts pnts
     (map (λ (pnt) (point-rec (hash-ref pnt->alt-err pnt) (list altn)))
	  pnts))))

(define (append-at-pnts points->alts pnts altn)
  (hash-set-lsts
   points->alts pnts
   (map (λ (pnt) (let ([old-val (hash-ref points->alts pnt)])
		   (point-rec (point-rec-berr old-val)
			      (cons altn (point-rec-altns old-val)))))
	pnts)))

(define (minimize-alts atab)
  (define (get-essential pnts->alts)
    (remove-duplicates (filter identity
			       (map (λ (pnt-rec) (let ([altns (point-rec-altns pnt-rec)])
						   (cond [(> (length altns) 1) #f]
							 [(= (length altns) 1) (car altns)]
							 [else (error "This point has no alts which are best at it!" pnt-rec)])))
				    (hash-values pnts->alts)))))

  (define (get-tied-alts essential-alts alts->pnts pnts->alts)
    (remove* essential-alts (hash-keys alts->pnts)))

  (define (worst atab altns)
    (let* ([alts->pnts (curry hash-ref (alt-table-alt->points atab))]
           [alts->done? (curry hash-ref (alt-table-alt->done? atab))])
      ; There must always be a not-done tied alt,
      ; since before adding any alts there weren't any tied alts
      (let ([undone-altns (filter (compose not alts->done?) altns)])
        (argmax
         alt-cost
         (argmins (compose length alts->pnts) (if (null? undone-altns) altns undone-altns))))))

  (let loop ([cur-atab atab])
    (let* ([alts->pnts (alt-table-alt->points cur-atab)]
	   [pnts->alts (alt-table-point->alts cur-atab)]
	   [essential-alts (get-essential pnts->alts)]
	   [tied-alts (get-tied-alts essential-alts alts->pnts pnts->alts)])
      (if (null? tied-alts) cur-atab
	  (let ([atab* (rm-alts cur-atab (worst cur-atab tied-alts))])
	    (loop atab*))))))

(define (rm-alts atab . altns)
  (let* ([rel-points (remove-duplicates
		      (apply append
			     (map (curry hash-ref (alt-table-alt->points atab))
				  altns)))]
	 [pnts->alts* (let ([pnts->alts (alt-table-point->alts atab)])
			(hash-set-lsts
			 pnts->alts rel-points
			 (map (λ (pnt) (let ([old-val (hash-ref pnts->alts pnt)])
					 (point-rec (point-rec-berr old-val) (remove* altns (point-rec-altns old-val)))))
			      rel-points)))]
	 [alts->pnts* (hash-remove* (alt-table-alt->points atab)
				    altns)]
	 [alts->done?* (hash-remove* (alt-table-alt->done? atab)
				     altns)])
    (alt-table pnts->alts* alts->pnts* alts->done?* (alt-table-context atab))))

(define (atab-add-altn atab altn)
  (match-let* ([pnts->alts (alt-table-point->alts atab)]
	       [alts->pnts (alt-table-alt->points atab)]
	       [`(,best-pnts ,tied-pnts) (best-and-tied-at-points pnts->alts altn)])
    (if (null? best-pnts)
	atab
	(let ()
    (define alts->pnts*1 (remove-chnged-pnts pnts->alts alts->pnts best-pnts))
    (println "test")
    (define alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts)))
    (define pnts->alts*1 (override-at-pnts pnts->alts best-pnts altn))
    (define pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn))
    (define alts->done?* (hash-set (alt-table-alt->done? atab) altn #f))
    (define atab*1 (alt-table pnts->alts*2 alts->pnts*2 alts->done?* (alt-table-context atab)))
    (define atab*2 (minimize-alts atab*1))
	  atab*2))))

(define (atab-not-done-alts atab)
  (filter (negate (curry hash-ref (alt-table-alt->done? atab)))
	  (hash-keys (alt-table-alt->points atab))))

;; The completeness invariant states that at any time, for every point there exists some
;; alt that is best at it.
(define (check-completeness-invariant atab #:message [message ""])
  (if (andmap (negate (compose null? point-rec-altns
			       (curry hash-ref (alt-table-point->alts atab))))
	      (hash-keys (alt-table-point->alts atab)))
      atab
      (error (string-append "Completeness invariant violated. " message))))

;; The reflexive invariant is this: a) For every alternative, for every point it maps to,
;; those points also map back to the alternative. b) For every point, for every alternative
;; it maps to, those alternatives also map back to the point.
(define (check-reflexive-invariant atab #:message [message ""])
  (if (and (andmap (λ (altn)
		     (andmap (λ (pnt)
			       (member altn (point-rec-altns (hash-ref (alt-table-point->alts atab) pnt))))
			     (hash-ref (alt-table-alt->points atab) altn)))
		   (hash-keys (alt-table-alt->done? atab)))
	   (andmap (λ (pnt)
		     (andmap (λ (altn)
			       (member pnt (hash-ref (alt-table-alt->points atab) altn)))
			     (point-rec-altns (hash-ref (alt-table-point->alts atab) pnt))))
		   (hash-keys (alt-table-point->alts atab))))
      atab
      (error (string-append "Reflexive invariant violated. " message))))

;; The minimality invariant states that every alt must be untied and best on at least one point.
(define (check-minimality-invariant atab #:message [message ""])
  (hash-for-each (alt-table-alt->points atab)
                 (λ (k v)
                    (let ([cnt (for/list ([pt v])
                                 (length (point-rec-altns (hash-ref (alt-table-point->alts atab) pt))))])
                      (when (not (= (apply min cnt) 1))
                        (error (string-append "Minimality invariant violated. " message)))))))


(define (assert-points-orphaned alts->pnts opnts all-pnts #:message [msg ""])
  (hash-for-each alts->pnts
		 (λ (altn pnts)
		   (when (ormap (curryr member pnts) opnts)
		     (error (string-append "Assert Failed: The given points were not orphaned. " msg)))))
  (let ([hopefully-unorphaned-points (remove* opnts all-pnts)]
	[actually-unorphaned-points (remove-duplicates (apply append (hash-values alts->pnts)))])
    (when (ormap (negate (curryr member actually-unorphaned-points)) hopefully-unorphaned-points)
      (error (string-append "Assert Failed: Points other than the given points were orphaned. " msg)))))
