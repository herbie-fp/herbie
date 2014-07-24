#lang racket

(require casio/common)
(require casio/alternative)
(require casio/points)

(provide make-alt-table atab-all-alts
	 atab-add-altns atab-pick-alt
	 atab-completed?)

;; Public API

(struct alt-table (points->alts alts->points alts->done?) #:prefab)

(define (make-alt-table points initial-alt)
   (alt-table (make-immutable-hash (map cons
					points
					(map (λ (err) (point-rec err (list initial-alt)))
					     (alt-errors initial-alt))))
	      (hash initial-alt points)
	      (hash initial-alt #f)))

(define (atab-add-altns atab altns)
  (pipe atab (map (curry curryr atab-add-altn)
		  altns)))

(define (atab-pick-alt atab #:picking-func [pick car])
  (let* ([picked (pick (atab-not-done-alts atab))]
	 [atab* (alt-table-with atab #:alts->done? (hash-set (alt-table-alts->done? atab) picked #t))])
    (values picked atab*)))

(define (atab-all-alts atab)
  (hash-keys (alt-table-alts->points atab)))

(define (atab-completed? atab)
  (andmap identity (hash-values (alt-table-alts->done? atab))))

;; Helper Functions

(define (alt-table-with atab
			#:points->alts [pnts->alts #f]
			#:alts->points [alts->pnts #f]
			#:alts->done? [alts->done? #f])
  (alt-table (or pnts->alts (alt-table-points->alts atab))
	     (or alts->pnts (alt-table-alts->points atab))
	     (or alts->done? (alt-table-alts->done? atab))))

(define (alternate . lsts)
  (let loop ([rest-lsts lsts] [acc '()])
    (if (ormap null? rest-lsts)
	(reverse acc)
	(loop (map cdr rest-lsts) (append (reverse (map car rest-lsts)) acc)))))

(define (hash-set-lsts hash keys values)
  (apply (curry hash-set* hash)
	 (alternate keys values)))

(define (hash-remove* hash keys)
  (pipe hash (map (curry curryr hash-remove) keys)))

;; Implementation

(struct point-rec (berr altns) #:prefab)

(define (best-and-tied-at-points points->alts altn)
  (let loop ([rest-points (*points*)] [rest-errs (alt-errors altn)]
	     [best-acc '()] [tied-acc '()])
    (if (null? rest-points) (values best-acc tied-acc)
	(let* ([pnt (car rest-points)]
	       [pnt-rec (hash-ref points->alts pnt)]
	       [table-err (point-rec-berr pnt-rec)]
	       [altn-err (car rest-errs)])
	  (cond [(< altn-err table-err)
		 (loop (cdr rest-points) (cdr rest-errs)
		       (cons pnt best-acc) tied-acc)]
		[(= altn-err table-err)
		 (loop (cdr rest-points) (cdr rest-errs)
		       best-acc (cons pnt tied-acc))]
		[else (loop (cdr rest-points) (cdr rest-errs)
			    best-acc tied-acc)])))))

(define (remove-chnged-pnts points->alts alts->points chnged-pnts)
  (let* ([chnged-entries (map (curry hash-ref points->alts) chnged-pnts)]
	 [chnged-altns (remove-duplicates (apply append (map point-rec-altns chnged-entries)))])
    (hash-set-lsts
     alts->points chnged-altns
     (map (λ (altn)
	    (remove* chnged-pnts (hash-ref alts->points altn)))
	  chnged-altns))))

(define (override-at-pnts points->alts pnts altn)
  (let ([pnt->alt-err (make-immutable-hash (map cons
						(*points*)
						(alt-errors altn)))])
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
    (let ([tied-pnts (remove* (apply append (map (curry hash-ref alts->pnts) essential-alts))
			      (hash-keys pnts->alts))])
      (remove-duplicates
       (apply append (map (compose point-rec-altns (curry hash-ref pnts->alts))
			  tied-pnts)))))
  (define (worst altns)
    (argmax alt-history-length (argmaxs alt-cost altns)))
  (let loop ([cur-atab atab])
    (let* ([alts->pnts (alt-table-alts->points cur-atab)]
	   [pnts->alts (alt-table-points->alts cur-atab)]
	   [essential-alts (get-essential pnts->alts)]
	   [tied-alts (get-tied-alts essential-alts alts->pnts pnts->alts)])
      (if (null? tied-alts) cur-atab
	  (let ([atab* (rm-alts cur-atab (worst tied-alts))])
	    (loop atab*))))))

(define (rm-alts atab . altns)
  (let* ([rel-points (remove-duplicates
		      (apply append
			     (map (curry hash-ref (alt-table-alts->points atab))
				  altns)))]
	 [pnts->alts* (let ([pnts->alts (alt-table-points->alts atab)])
			(hash-set-lsts
			 pnts->alts rel-points
			 (map (λ (pnt) (let ([old-val (hash-ref pnts->alts pnt)])
					 (point-rec (point-rec-berr old-val) (remove* altns (point-rec-altns old-val)))))
			      rel-points)))]
	 [alts->pnts* (hash-remove* (alt-table-alts->points atab)
				    altns)]
	 [alts->done?* (hash-remove* (alt-table-alts->done? atab)
				     altns)])
    (alt-table pnts->alts* alts->pnts* alts->done?*)))

(define (atab-add-altn atab altn)
  (let*-values ([(pnts->alts) (alt-table-points->alts atab)]
		[(alts->pnts) (alt-table-alts->points atab)]
		[(best-pnts tied-pnts) (best-and-tied-at-points pnts->alts altn)])
    (if (null? best-pnts)
	atab
	(let* ([alts->pnts*1 (remove-chnged-pnts pnts->alts alts->pnts best-pnts)]
	       [alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts))]
	       [pnts->alts*1 (override-at-pnts pnts->alts best-pnts altn)]
	       [pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn)]
	       [alts->done?* (hash-set (alt-table-alts->done? atab) altn #f)]
	       [atab*1 (alt-table pnts->alts*2 alts->pnts*2 alts->done?*)]
	       [atab*2 (minimize-alts atab*1)])
	  atab*2))))

(define (atab-not-done-alts atab)
  (filter (negate (curry hash-ref (alt-table-alts->done? atab)))
	  (hash-keys (alt-table-alts->points atab))))

;; The completeness invariant states that at any time, for every point there exists some
;; alt that is best at it.
(define (check-completeness-invariant atab #:message [message ""])
  (if (andmap (negate (compose null? point-rec-altns
			       (curry hash-ref (alt-table-points->alts atab))))
	      (hash-keys (alt-table-points->alts atab)))
      atab
      (error (string-append "Completeness invariant violated. " message))))

;; The reflexive invariant is this: a) For every alternative, for every point it maps to,
;; those points also map back to the alternative. b) For every point, for every alternative
;; it maps to, those alternatives also map back to the point.
(define (check-reflexive-invariant atab #:message [message ""])
  (if (and (andmap (λ (altn)
		     (andmap (λ (pnt)
			       (member altn (point-rec-altns (hash-ref (alt-table-points->alts atab) pnt))))
			     (hash-ref (alt-table-alts->points atab) altn)))
		   (hash-keys (alt-table-alts->done? atab)))
	   (andmap (λ (pnt)
		     (andmap (λ (altn)
			       (member pnt (hash-ref (alt-table-alts->points atab) altn)))
			     (point-rec-altns (hash-ref (alt-table-points->alts atab) pnt))))
		   (hash-keys (alt-table-points->alts atab))))
      atab
      (error (string-append "Reflexive invariant violated. " message))))

(define (assert-points-orphaned alts->pnts opnts all-pnts #:message [msg ""])
  (hash-for-each alts->pnts
		 (λ (altn pnts)
		   (when (ormap (curryr member pnts) opnts)
		     (error (string-append "Assert Failed: The given points were not orphaned. " msg)))))
  (let ([hopefully-unorphaned-points (remove* opnts all-pnts)]
	[actually-unorphaned-points (remove-duplicates (apply append (hash-values alts->pnts)))])
    (when (ormap (negate (curryr member actually-unorphaned-points)) hopefully-unorphaned-points)
      (error (string-append "Assert Failed: Points other than the given points were orphaned. " msg)))))
