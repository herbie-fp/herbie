#lang racket

(require casio/common)
(require casio/alternative)

(provide make-alt-table atab-add-altn
	 atab-all-alts atab-not-done-alts)

;; Public API

(struct alt-table (points->alts alts->points) #:prefab)

(define (make-alt-table points initial-alt)
  (let ([initial-dalt (dalt initial-alt #f)])
    (alt-table (make-immutable-hash (map cons
					 points
					 (map (λ (err) (point-rec err (list initial-dalt)))
					      (alt-errors initial-alt))))
	       (hash initial-dalt points))))

(define (atab-add-altn atab altn)
  (let* ([pnts->alts (alt-table-points->alts atab)]
	 [alts->pnts (alt-table-alts->points atab)]
	 [daltn (dalt altn #f)]
	 [best-pnts (best-at-points pnts->alts daltn)]
	 [tied-pnts (tied-at-points pnts->alts daltn)]
	 [alts->pnts*1 (remove-chnged-pnts pnts->alts alts->pnts best-pnts)]
	 [alts->pnts*2 (hash-set alts->pnts*1 daltn (append best-pnts tied-pnts))]
	 [pnts->alts*1 (override-at-pnts pnts->alts best-pnts daltn)]
	 [pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts daltn)]
	 [atab*1 (alt-table pnts->alts*2 alts->pnts*2)]
	 [useless-alts (find-useless atab*1)]
	 [atab*2 (rm-alts atab*1 useless-alts)])
    atab*2))

(define (atab-all-alts atab)
  (map dalt-altn (hash-keys (alt-table-alts->points atab))))

(define (atab-not-done-alts atab)
  (map dalt-altn (filter (negate dalt-done?)
			 (hash-keys (alt-table-alts->points atab)))))

;; Helper Functions

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

(struct dalt (altn done?) #:prefab)
(struct point-rec (berr altns) #:prefab)

(define (best-at-points points->alts daltn)
  (filter identity
	  (map (λ (table-rec altn-err)
		 (let* ([pnt (car table-rec)]
			[pnt-rec (cdr table-rec)]
			[table-err (point-rec-berr pnt-rec)])
		   (if (< altn-err table-err)
		       pnt #f)))
	       (hash->list points->alts)
	       (alt-errors (dalt-altn daltn)))))

(define (tied-at-points points->alts daltn)
  (filter identity
	  (map (λ (table-rec altn-err)
		 (let* ([pnt (car table-rec)]
			[pnt-rec (cdr table-rec)]
			[table-err (point-rec-berr pnt-rec)])
		   (if (= altn-err table-err)
		       pnt #f)))
	       (hash->list points->alts)
	       (alt-errors (dalt-altn daltn)))))

(define (remove-chnged-pnts points->alts alts->points chnged-pnts)
  (let* ([chnged-entries (map (curry hash-ref points->alts) chnged-pnts)]
	 [chnged-altns (remove-duplicates (apply append (map point-rec-altns chnged-entries)))])
    (hash-set-lsts
     alts->points chnged-altns
     (map (λ (daltn)
	    (remove* chnged-pnts (hash-ref alts->points daltn)))
	  chnged-altns))))

(define (override-at-pnts points->alts pnts daltn)
  (let ([pnt->alt-err (make-immutable-hash (map cons
						(hash-keys points->alts)
						(alt-errors (dalt-altn daltn))))])
    (hash-set-lsts
     points->alts pnts
     (map (λ (pnt) (point-rec (hash-ref pnt->alt-err pnt) (list daltn)))
	  pnts))))

(define (append-at-pnts points->alts pnts daltn)
  (let ([pnt->alt-err (make-immutable-hash (map cons
						(hash-keys points->alts)
						(alt-errors (dalt-altn daltn))))])
    (hash-set-lsts
     points->alts pnts
     (map (λ (pnt) (let ([old-val (hash-ref points->alts pnt)])
		     (point-rec (point-rec-berr old-val)
				(cons daltn (point-rec-altns old-val)))))
	  pnts))))

(define (find-useless atab)
  (let* ([essential-dalts (remove-duplicates
			   (filter identity
				   (map (λ (pnt-rec)
					  (let ([altns (point-rec-altns pnt-rec)])
					    (if (> (length altns) 1)
						#f (car altns))))
					(hash-values (alt-table-points->alts atab)))))]
	 [ambigiously-tied-pnts
	  (remove* (apply append (map (curry hash-ref (alt-table-alts->points atab))
				      essential-dalts))
		   (hash-keys (alt-table-points->alts atab)))]
	 [ambigiously-tied-dalts
	  (remove-duplicates (apply append (map (compose point-rec-altns (curry hash-ref (alt-table-points->alts atab)))
						ambigiously-tied-pnts)))])
    (remove* (append essential-dalts ambigiously-tied-dalts)
	     (hash-keys (alt-table-alts->points atab)))))

(define (rm-alts atab daltns)
  (let* ([rel-points (remove-duplicates
		      (apply append
			     (map (curry hash-ref (alt-table-alts->points atab))
				  daltns)))]
	 [pnts->alts* (let ([pnts->alts (alt-table-points->alts atab)])
			(hash-set-lsts
			 pnts->alts rel-points
			 (map (λ (pnt) (let ([old-val (hash-ref pnts->alts pnt)])
					 (point-rec (point-rec-berr old-val) (remove* daltns (point-rec-altns old-val)))))
			      rel-points)))]
	 [alts->pnts* (hash-remove* (alt-table-alts->points atab)
				    daltns)])
    (alt-table pnts->alts* alts->pnts*)))
