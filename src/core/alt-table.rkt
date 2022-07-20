#lang racket

(require racket/hash)
(require "../common.rkt" "../alternative.rkt" "../points.rkt" "../programs.rkt" "../syntax/types.rkt")

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
  (atab-peek-alt (alt-table? #:picking-func ((listof alt?) . -> . (or/c alt? boolean?))
                             #:only-fresh boolean?
                             . -> . (or/c alt? boolean?)))
  (atab-completed? (alt-table? . -> . boolean?))
  (atab-context (alt-table? . -> . pcontext?))
  (atab-min-errors (alt-table? . -> . (listof real?)))
  (split-atab (alt-table? (non-empty-listof any/c) . -> . (listof alt-table?)))))

;; Public API

(struct alt-table (point->alts alt->points alt->done? alt->cost context all) #:prefab)
(struct cost-rec (berr altns) #:prefab)

(define atab-context alt-table-context)

(define in-atab-pcontext (compose in-pcontext atab-context))

(define (backup-alt-cost altn)
  (let loop ([expr (program-body (alt-program altn))])
    (match expr
     [(list 'if cond ift iff) (+ 1 (loop cond) (max (loop ift) (loop iff)))]
     [(list op args ...) (apply + 1 (map loop args))]
     [_ 1])))

; In normal mode, cost is not considered so we return a constant
; The alt table becomes "degenerate"
(define (alt-cost* altn repr)
  (if (*pareto-mode*)
      (alt-cost altn repr)
      1))

(define (make-alt-table pcontext initial-alt ctx)
  (define cost (alt-cost* initial-alt (context-repr ctx)))
  (alt-table (make-immutable-hash
               (for/list ([(pt ex) (in-pcontext pcontext)]
                          [err (errors (alt-program initial-alt) pcontext ctx)])
                 (cons pt (hash cost (cost-rec err (list initial-alt))))))
             (hash initial-alt (for/list ([(pt ex) (in-pcontext pcontext)]) pt))
             (hash initial-alt #f)
             (hash initial-alt cost)
             pcontext
             (list initial-alt)))

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
  (alt-table-all atab))

(define (atab-completed? atab)
  (andmap identity (hash-values (alt-table-alt->done? atab))))

;; Split the alt table into several alt tables, each of which corresponds to a pred
;; in 'preds', and only contains points which satisfy that pred.
(define (split-atab atab preds)
  (for/list ([pred preds])
    (define-values (pts exs)
      (for/lists (pts exs)
        ([(pt ex) (in-atab-pcontext atab)] #:when (pred pt))
          (values pt ex)))
    (define point->alts
      (for/hash ([pt pts])
        (values pt (hash-ref (alt-table-point->alts atab) pt))))
    (define alt->points
      (make-immutable-hash
        (filter-not (compose null? cdr)
          (for/list ([(alt pnts) (in-hash (alt-table-alt->points atab))])
            (cons alt (filter (curry set-member? pts) pnts))))))
    (define alt->done?
      (for/hash ([alt (in-hash-keys alt->points)])
        (values alt (hash-ref (alt-table-alt->done? atab) alt))))
    (define alt->cost
      (for/hash ([alt (in-hash-keys alt->points)])
        (values alt (hash-ref (alt-table-alt->cost atab) alt))))
    (define context (mk-pcontext pts exs))
    (minimize-alts
      (alt-table point->alts alt->points alt->done? alt->cost
                 context (alt-table-all atab)))))

;; Helper Functions

(define (hash-remove* hash keys)
  (for/fold ([hash hash]) ([key keys])
    (hash-remove hash key)))

;; Implementation

(define (best-and-tied-at-points atab altn cost errs)
  (define point->alt (alt-table-point->alts atab))
  (reap [best! tied! tied-errs!]
    (for ([(pnt ex) (in-pcontext (alt-table-context atab))] [err errs])
      (define cost-hash (hash-ref point->alt pnt))
      (define rec (hash-ref cost-hash cost #f))
      (if rec
          (let ([table-err (cost-rec-berr rec)])
            (cond
             [(< err table-err)
              (best! pnt)]
             [(= err table-err)
              (tied! pnt)
              (tied-errs! err)]
             [else (void)]))
          (best! pnt)))))

(define (remove-chnged-pnts point->alts alt->points alt->cost chnged-pnts cost)
  (define chnged-entries (map (curry hash-ref point->alts) chnged-pnts))
  (define chnged-altns (mutable-set))
  (for* ([entry chnged-entries] #:when (hash-has-key? entry cost)
         [altn (cost-rec-altns (hash-ref entry cost))])
    (set-add! chnged-altns altn))
  (define chnged-set (list->seteq chnged-pnts))
  (hash-union
   alt->points
   (for/hash ([altn (in-set chnged-altns)])
     (values altn (set->list (set-subtract (list->seteq (hash-ref alt->points altn)) chnged-set))))
   #:combine (λ (a b) b)))

(define (override-at-pnts points->alts pnts altn cost errs)
  (define pnt->errs
    (for/hash ([(pnt ex) (in-pcontext (*pcontext*))] [err errs])
      (values pnt err)))
  (hash-union
    points->alts
    (for/hash ([pnt pnts])
      (values pnt (hash-set (hash-ref points->alts pnt) cost
                            (cost-rec (hash-ref pnt->errs pnt) (list altn)))))
    #:combine (λ (a b) b)))

(define (append-at-pnts points->alts pnts altn cost)
  (hash-union
   points->alts
   (for/hash ([pnt pnts])
     (define cost-hash (hash-ref points->alts pnt))
     (match-define (cost-rec berr altns) (hash-ref cost-hash cost))
     (values pnt (hash-set cost-hash cost
                           (cost-rec berr (cons altn altns)))))
   #:combine (λ (a b) b)))

(struct set-cover (removable coverage))

(define (atab->set-cover atab)
  (match-define (alt-table pnts->alts alts->pnts alt->done? alt->cost _ _) atab)
  
  (define tied (list->mutable-seteq (hash-keys alts->pnts)))
  (define coverage '())
  (for* ([cost-hash (hash-values pnts->alts)] [rec (hash-values cost-hash)])
    (match (cost-rec-altns rec)
      [(list)
       (error "This point has no alts which are best at it!" rec)]
      [(list altn)
       (set-remove! tied altn)]
      [altns
       (set! coverage (cons (list->vector altns) coverage))]))
  (set-cover tied coverage))

(define (set-cover-remove! sc altn)
  (match-define (set-cover removable coverage) sc)
  (set-remove! removable altn)
  (for ([s (in-list coverage)])
    (define count 0)
    (define last #f)
    (for ([i (in-naturals)] [a (in-vector s)])
      (cond
       [(eq? a altn)
        (vector-set! s i #f)]
       [a
        (set! count (add1 count))
        (set! last a)]))
    (when (= count 1)
      (set-remove! removable last))))

(define (worst atab altns)
  (let* ([alts->pnts (curry hash-ref (alt-table-alt->points atab))]
         [alts->done? (curry hash-ref (alt-table-alt->done? atab))]
         [alt->cost (if (*pareto-mode*)
                        (curry hash-ref (alt-table-alt->cost atab))
                        backup-alt-cost)]
    ; There must always be a not-done tied alt,
    ; since before adding any alts there weren't any tied alts
         [undone-altns (filter (compose not alts->done?) altns)])
    (argmax alt->cost
      (argmins (compose length alts->pnts)
               (if (null? undone-altns) altns undone-altns)))))

(define (minimize-alts atab)
  (define sc (atab->set-cover atab))
  (let loop ([removed '()])
    (if (set-empty? (set-cover-removable sc))
        (apply rm-alts atab removed)
        (let ([worst-alt (worst atab (set->list (set-cover-removable sc)))])
          (set-cover-remove! sc worst-alt)
          (loop (cons worst-alt removed))))))

(define (rm-alts atab . altns)
  (match-define (alt-table point->alts alt->points alt->done? alt->cost _ _) atab)
  (define rel-points
    (remove-duplicates
     (append-map (curry hash-ref (alt-table-alt->points atab)) altns)))
  
  (define altn-set (list->seteq altns))

  (define pnts->alts*
    (hash-union
      point->alts
      (for/hash ([pnt rel-points])
        (define cost-hash
          (for/hash ([(cost rec) (hash-ref point->alts pnt)])
            (values cost (cost-rec (cost-rec-berr rec)
                                   (set->list (set-subtract (list->seteq (cost-rec-altns rec))
                                                            altn-set))))))
        (values pnt cost-hash))
      #:combine (λ (a b) b)))

  (struct-copy alt-table atab
               [point->alts pnts->alts*]
               [alt->points (hash-remove* alt->points altns)]
               [alt->done? (hash-remove* alt->done? altns)]
               [alt->cost (hash-remove* alt->cost altns)]))

(define (is-nan? expr)
  (and (impl-exists? expr) (equal? (impl->operator expr) 'NAN)))

(define (atab-add-altns atab altns ctx)
  (define progs (map alt-program altns))
  (define errss (flip-lists (batch-errors progs (alt-table-context atab) ctx)))
  (define atab*
    (minimize-alts
     (for/fold ([atab atab]) ([altn (in-list altns)] [errs (in-list errss)])
       (atab-add-altn atab altn errs (context-repr ctx)))))
  (struct-copy alt-table atab*
               [all (set-union (alt-table-all atab) (hash-keys (alt-table-alt->points atab*)))]))

(define (worse-than? point->alts altn cost tied-pnts tied-errs)
  (cond
   [(*pareto-mode*)
    (for/and ([pt tied-pnts] [err tied-errs])
      (let* ([cost-hash (hash-ref point->alts pt)]
             [less (filter (curryr < cost) (hash-keys cost-hash))]
             [err-acc (compose cost-rec-berr (curry hash-ref cost-hash))])
        (and (not (null? less))
             (> err (err-acc (argmax identity less))))))]
   [else
    (define cost* (backup-alt-cost altn))
    (for/and ([pt tied-pnts])
      (let* ([cost-table (hash-ref point->alts pt)]
             [altns (cost-rec-altns (hash-ref cost-table 1))]
             [mincost (argmin identity (map backup-alt-cost altns))])
        (>= cost* mincost)))]))

(define (atab-add-altn atab altn errs repr)
  (define cost (alt-cost* altn repr))
  (match-define (alt-table point->alts alt->points alt->done? alt->cost _ all-alts) atab)
  (define-values (best-pnts tied-pnts tied-errs) (best-and-tied-at-points atab altn cost errs))
  (cond
   [(or (ormap (curry alt-equal? altn) (set->list all-alts))
        (and (null? best-pnts) (worse-than? point->alts altn cost tied-pnts tied-errs)))
    atab]
   [else
    (define alts->pnts*1 (remove-chnged-pnts point->alts alt->points alt->cost best-pnts cost))
    (define alts->pnts*2 (hash-set alts->pnts*1 altn (append best-pnts tied-pnts)))
    (define pnts->alts*1 (override-at-pnts point->alts best-pnts altn cost errs))
    (define pnts->alts*2 (append-at-pnts pnts->alts*1 tied-pnts altn cost))
    (define alts->done?* (hash-set alt->done? altn #f))
    (define alt->cost* (hash-set alt->cost altn cost))
    (alt-table pnts->alts*2 alts->pnts*2 alts->done?*
               alt->cost* (alt-table-context atab) all-alts)]))

(define (atab-not-done-alts atab)
  (filter (negate (curry hash-ref (alt-table-alt->done? atab)))
    (hash-keys (alt-table-alt->points atab))))

(define (atab-min-errors atab)
  (define pnt->alts (alt-table-point->alts atab))
  (for/list ([(pt ex) (in-pcontext (alt-table-context atab))])
    (for/fold ([best #f]) ([rec (hash-values (hash-ref pnt->alts pt))])
      (let ([err (cost-rec-berr rec)])
        (cond [(not best) err]
              [(< err best) err]
              [(>= err best) best])))))

;; The completeness invariant states that at any time, for every point there exists some
;; alt that is best at it.
(define (check-completeness-invariant atab #:message [message ""])
  (if (for/and ([(pt ch) (in-hash (alt-table-point->alts atab))])
        (for/or ([(c rec) (in-hash ch)])
          (not (null? (cost-rec-altns rec)))))
      atab
      (error (string-append "Completeness invariant violated. " message))))

(define (pnt-maps-to-alt? pt altn pnt->alts)
  (define cost-hash (hash-ref pnt->alts pt))
  (for ([rec (hash-values cost-hash)])
    (member altn (cost-rec-altns rec))))

(define (alt-maps-to-pnt? altn pt alt->pnts)
  (member pt (hash-ref alt->pnts altn)))

;; The reflexive invariant is this: a) For every alternative, for every point it maps to,
;; those points also map back to the alternative. b) For every point, for every alternative
;; it maps to, those alternatives also map back to the point.
(define (check-reflexive-invariant atab #:message [message ""])
  (define pnt->alts (alt-table-point->alts atab))
  (define alt->pnts (alt-table-alt->points atab))
  (if (and 
        (for/and ([(altn pnts) (in-hash alt->pnts)])
          (andmap (curryr pnt-maps-to-alt? altn pnt->alts) pnts))
        (for/and ([(pt ch) (in-hash pnt->alts)])
          (for/and ([(c rec) (in-hash ch)])
            (andmap (curryr alt-maps-to-pnt? pt alt->pnts) (cost-rec-altns rec)))))
      atab
      (error (string-append "Reflexive invariant violated. " message))))

;; The minimality invariant states that every alt must be untied and best on at least one point.
(define (check-minimality-invariant atab repr #:message [message ""])
  (hash-for-each (alt-table-alt->points atab)
                 (λ (k v)
                    (let ([cnt (for/list ([pt v])
                                 (let ([cost (alt-cost* k repr)]
                                       [cost-hash (hash-ref (alt-table-point->alts atab) pt)])
                                   (length (cost-rec-altns (hash-ref cost-hash cost)))))])
                      (when (not (= (apply min cnt) 1))
                        (error (string-append "Minimality invariant violated. " message)))))))

; In normal mode, ensure that for each point, the hash contains a single cost
(define (check-normal-mode-flatness atab)
  (for ([(pt ch) (in-hash (alt-table-point->alts atab))])
    (when (not (= (length (hash-keys ch)) 1))
      (error "Point to alternative hash table not flat"))))

(define (assert-points-orphaned alts->pnts opnts all-pnts #:message [msg ""])
  (hash-for-each alts->pnts
     (λ (altn pnts)
       (when (ormap (curryr member pnts) opnts)
         (error (string-append "Assert Failed: The given points were not orphaned. " msg)))))
  (let ([hopefully-unorphaned-points (remove* opnts all-pnts)]
  [actually-unorphaned-points (remove-duplicates (apply append (hash-values alts->pnts)))])
    (when (ormap (negate (curryr member actually-unorphaned-points)) hopefully-unorphaned-points)
      (error (string-append "Assert Failed: Points other than the given points were orphaned. " msg)))))
