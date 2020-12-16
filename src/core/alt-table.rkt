#lang racket

(require racket/hash)
(require "../common.rkt" "../alternative.rkt" "../points.rkt"
         "../timeline.rkt" "../programs.rkt")

(provide
 (contract-out
  (make-alt-table (pcontext? alt? any/c . -> . alt-table?))
  (atab-active-alts (alt-table? . -> . (listof alt?)))
  (atab-all-alts (alt-table? . -> . (listof alt?)))
  (atab-not-done-alts (alt-table? . -> . (listof alt?)))
  (atab-add-altns (alt-table? (listof alt?) any/c . -> . alt-table?))
  (atab-pick-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
                             #:only-fresh boolean?
                             . -> . (values alt? alt-table?)))
  (atab-peek-alt (alt-table? #:picking-func ((listof alt?) . -> . alt?)
                             #:only-fresh boolean?
                             . -> . (values alt? alt-table?)))
  (atab-completed? (alt-table? . -> . boolean?))
  (atab-context (alt-table? . -> . pcontext?))
  (atab-min-errors (alt-table? . -> . (listof real?)))
  (split-atab (alt-table? (non-empty-listof any/c) . -> . (listof alt-table?)))))

;; Public API

(struct alt-table (point->alts alt->points alt->done? context all) #:prefab)

(define atab-context alt-table-context)

(define in-atab-pcontext (compose in-pcontext atab-context))

(define (make-alt-table context initial-alt repr)
  (alt-table (make-immutable-hash
               (for/list ([(pt ex) (in-pcontext context)]
                          [err (errors (alt-program initial-alt) context repr)])
                 (cons pt (point-rec err (alt-cost initial-alt) (list initial-alt)))))
             (hash initial-alt (for/list ([(pt ex) (in-pcontext context)]) pt))
             (hash initial-alt #f)
             context
             (mutable-set initial-alt)))

(define (atab-pick-alt atab #:picking-func [pick car]
           #:only-fresh [only-fresh? #t])
  (let* ([picked (atab-peek-alt atab #:picking-func pick #:only-fresh only-fresh?)]
         [atab* (struct-copy alt-table atab
                             [alt->done? (hash-set (alt-table-alt->done? atab)
                                                   picked #t)])])
    (values picked atab*)))

(define (atab-peek-alt atab #:picking-func [pick car] #:only-fresh [only-fresh? #f])
  (pick (if only-fresh?
      (atab-not-done-alts atab)
      (atab-active-alts atab))))

(define (atab-active-alts atab)
  (hash-keys (alt-table-alt->points atab)))

(define (atab-all-alts atab)
  (set->list (alt-table-all atab)))

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
      (minimize-alts (alt-table point->alts alt->points alt->done?
                                context (alt-table-all atab))))))

;; Helper Functions

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

;; Implementation

(struct point-rec (berr cost altns) #:prefab)

(define (best-and-tied-at-points atab altn errs)
  (define point->alt (alt-table-point->alts atab))
  (reap [best! tied!]
    (for ([(pnt ex) (in-pcontext (alt-table-context atab))] [err errs])
      (match-define (point-rec table-err _ _) (hash-ref point->alt pnt))
      (cond 
       [(< err table-err)
        (best! pnt)]
       [(= err table-err)
        (tied! pnt)]
       [else (void)]))))

(define (remove-chnged-pnts point->alts alt->points chnged-pnts)
  (define chnged-entries (map (curry hash-ref point->alts) chnged-pnts))
  (define chnged-altns (remove-duplicates (append-map point-rec-altns chnged-entries)))

  (hash-union
   alt->points
   (for/hash ([altn chnged-altns])
     (values altn (remove* chnged-pnts (hash-ref alt->points altn))))
   #:combine (λ (a b) b)))

(define (override-at-pnts points->alts pnts altn errs)
  (define pnt->alt-err
    (for/hash ([(pnt ex) (in-pcontext (*pcontext*))] [err errs])
                        (values pnt err)))
  (hash-union
   points->alts
   (for/hash ([pnt pnts])
     (values pnt (point-rec (hash-ref pnt->alt-err pnt)
                            (alt-cost altn)
                            (list altn))))
   #:combine (λ (a b) b)))

(define (append-at-pnts points->alts pnts altn)
  (hash-union
   points->alts
   (for/hash ([pnt pnts])
     (match-define (point-rec berr cost altns) (hash-ref points->alts pnt))
     (values pnt (point-rec berr (min (alt-cost altn) cost) (cons altn altns))))
   #:combine (λ (a b) b)))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

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
  (match-define (alt-table point->alts alt->points alt->done? _ _) atab)
  (define rel-points
    (remove-duplicates
     (apply append (map (curry hash-ref (alt-table-alt->points atab)) altns))))

  (define pnts->alts*
    (hash-union
     point->alts
     (for/hash ([pnt rel-points])
       (define old-val (hash-ref point->alts pnt))
       (values pnt (struct-copy point-rec old-val
                                [altns (remove* altns (point-rec-altns old-val))])))
     #:combine (λ (a b) b)))

   ;[alts->pnts* (hash-remove* alt->points altns)]
   ;[alts->done?* (hash-remove* alt->done altns)])
  (struct-copy alt-table atab
               [point->alts pnts->alts*]
               [alt->points (hash-remove* alt->points altns)]
               [alt->done? (hash-remove* alt->done? altns)]))

(define (atab-add-altns atab altns repr)
  (define progs (map alt-program altns))
  (define errss (apply vector-map list (batch-errors progs (alt-table-context atab) repr)))
  (for/fold ([atab atab]) ([altn (in-list altns)] [errs (in-vector errss)])
    (atab-add-altn atab altn errs repr)))

(define (atab-add-altn atab altn errs repr)
  (define cost (alt-cost altn))
  (match-define (alt-table point->alts alt->points _ _ all-alts) atab)
  (define-values (best-pnts tied-pnts) (best-and-tied-at-points atab altn errs))
  (cond
   [(and (null? best-pnts)
         (for/and ([pnt (in-list tied-pnts)])
           (>= cost (point-rec-cost (hash-ref point->alts pnt)))))
    atab]
   [else
    (define alts->pnts*1 (remove-chnged-pnts point->alts alt->points best-pnts))
    (define alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts)))
    (define pnts->alts*1 (override-at-pnts point->alts best-pnts altn errs))
    (define pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn))
    (define alts->done?* (hash-set (alt-table-alt->done? atab) altn #f))
    (set-add! all-alts altn)
    (minimize-alts (alt-table pnts->alts*2 alts->pnts*2 alts->done?*
                              (alt-table-context atab) all-alts))]))

(define (atab-not-done-alts atab)
  (filter (negate (curry hash-ref (alt-table-alt->done? atab)))
    (hash-keys (alt-table-alt->points atab))))

(define (atab-min-errors atab)
  (for/list ([(pt ex) (in-pcontext (alt-table-context atab))])
    (point-rec-berr (hash-ref (alt-table-point->alts atab) pt))))

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
