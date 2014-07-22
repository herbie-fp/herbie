#lang racket

(require casio/common)
(require casio/alternative)

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

(define (atab-add-altn atab altn)
  (let* ([pnts->alts (alt-table-points->alts atab)]
	 [alts->pnts (alt-table-alts->points atab)]
	 [best-pnts (best-at-points pnts->alts altn)])
    (if (null? best-pnts)
	atab
	(let* ([tied-pnts (tied-at-points pnts->alts altn)]
	       [alts->pnts*1 (remove-chnged-pnts pnts->alts alts->pnts best-pnts)]
	       [alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts))]
	       [pnts->alts*1 (override-at-pnts pnts->alts best-pnts altn)]
	       [pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn)]
	       [alts->done?* (hash-set (alt-table-alts->done? atab) altn #f)]
	       [atab*1 (alt-table pnts->alts*2 alts->pnts*2 alts->done?*)]
	       [useless-alts (find-useless atab*1)]
	       [atab*2 (rm-alts atab*1 useless-alts)])
	  atab*2))))

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

(define (best-at-points points->alts altn)
  (filter identity
	  (map (λ (table-rec altn-err)
		 (let* ([pnt (car table-rec)]
			[pnt-rec (cdr table-rec)]
			[table-err (point-rec-berr pnt-rec)])
		   (if (< altn-err table-err)
		       pnt #f)))
	       (hash->list points->alts)
	       (alt-errors altn))))

(define (tied-at-points points->alts altn)
  (filter identity
	  (map (λ (table-rec altn-err)
		 (let* ([pnt (car table-rec)]
			[pnt-rec (cdr table-rec)]
			[table-err (point-rec-berr pnt-rec)])
		   (if (= altn-err table-err)
		       pnt #f)))
	       (hash->list points->alts)
	       (alt-errors altn))))

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
						(hash-keys points->alts)
						(alt-errors altn)))])
    (hash-set-lsts
     points->alts pnts
     (map (λ (pnt) (point-rec (hash-ref pnt->alt-err pnt) (list altn)))
	  pnts))))

(define (append-at-pnts points->alts pnts altn)
  (let ([pnt->alt-err (make-immutable-hash (map cons
						(hash-keys points->alts)
						(alt-errors altn)))])
    (hash-set-lsts
     points->alts pnts
     (map (λ (pnt) (let ([old-val (hash-ref points->alts pnt)])
		     (point-rec (point-rec-berr old-val)
				(cons altn (point-rec-altns old-val)))))
	  pnts))))

(define (minimize-alts atab)
  (define (get-essential pnts->alts)
    (remove-duplicates (filter identity
			       (map (λ (pnt-rec) (let ([altns (point-rec-altns pnt-rec)])
						   (if (> (length altns) 1)
						       #f (car altns))))
				    (hash-values pnts->alts)))))
  (define (get-tied-alts essential-alts alts->pnts pnts->alts)
    (let ([tied-pnts (remove* (apply append (map (curry hash-ref alts->pnts) essential-alts))
			      (hash-keys pnts->alts))])
      (remove-duplicates
       (apply append (map (compose point-rec-altns (curry hash-ref pnts->alts))
			  tied-pnts)))))
  (define (worst altns) ;; Sort of hacky but good enough for now
    (argmax (λ (altn) (+ (* 100 (alt-cost altn)) (alt-history-length altn))) altns))
  (let loop ([cur-atab atab])
    (let* ([alts->pnts (alt-table-alts->points cur-atab)]
	   [pnts->alts (alt-table-points->alts cur-atab)]
	   [essential-alts (get-essential pnts->alts)]
	   [tied-alts (get-tied-alts essential-alts alts->pnts pnts->alts)])
      (if (null? tied-alts) cur-atab
	  (let ([atab* (rm-alts cur-atab (list (worst tied-alts)))])
	    (loop atab*))))))

(define (rm-alts atab altns)
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
  (let* ([pnts->alts (alt-table-points->alts atab)]
	 [alts->pnts (alt-table-alts->points atab)]
	 [best-pnts (best-at-points pnts->alts altn)])
    (if (null? best-pnts)
	atab
	(let* ([tied-pnts (tied-at-points pnts->alts altn)]
	       [alts->pnts*1 (remove-chnged-pnts pnts->alts alts->pnts best-pnts)]
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
