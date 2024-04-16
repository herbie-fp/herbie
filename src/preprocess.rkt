#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt" "syntax/sugar.rkt"
         "syntax/types.rkt" "accelerator.rkt" "alternative.rkt"
         "common.rkt" "errors.rkt" "programs.rkt" "points.rkt"
         "timeline.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext remove-unnecessary-preprocessing)

;; Spec contains no accelerators
(define (spec-has-accelerator? spec)
  (match spec
    [(list (? accelerator?) _ ...) #t]
    [(list _ args ...) (ormap spec-has-accelerator? args)]
    [_ #f]))

;; Filter rules to extract all rules over spec.
(define (real-rules rules)
  (filter-not
    (lambda (rule)
      (or (representation? (rule-otype rule))
          (spec-has-accelerator? (rule-input rule))
          (spec-has-accelerator? (rule-output rule))))
    rules))

;; The even identities: f(x) = f(-x)
(define (make-even-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))])
    (replace-vars `((,var . (neg ,var))) spec)))

;; The odd identities: f(x) = -f(-x)
(define (make-odd-identities spec ctx)
  (for/list ([var (in-list (context-vars ctx))])
    (replace-vars `((,var . (neg ,var))) `(neg ,spec))))

;; Swap identities: f(a, b) = f(b, a)
(define (make-swap-identities spec ctx)
  (define pairs (combinations (context-vars ctx) 2))
    (for/list ([pair (in-list pairs)])
      (match-define (list a b) pair)
      (replace-vars `((,a . ,b) (,b . ,a)) spec)))

;; Initial simplify
; (define (initial-simplify init)
;   (define specs (list (prog->spec init)))
;   (define rules (real-rules (*simplify-rules*)))

;   ; egg schedule (2-phases for real rewrites and implementation selection)
;   (define schedule
;     `((run ,rules ((node . ,(*node-limit*))))
;       (run ,(*lowering-rules*) ((iteration . 1) (scheduler . simple)))
;       (convert)
;       (prune-spec)))
  
;   (void))

;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing init spec ctx)
  (define spec* (prog->spec spec))

  ;; the identities
  (define even-identities (make-even-identities spec* ctx))
  (define odd-identities (make-odd-identities spec* ctx))
  (define swap-identities (make-swap-identities spec* ctx))

  ;; the expressions
  (define exprs
    (cons spec*
          (append even-identities
                  odd-identities
                  swap-identities)))

  ;; egg query
  (define rules (real-rules (*simplify-rules*)))
  (define egg-query
    (make-egg-query exprs
                    (map (lambda (_) (context-repr ctx)) exprs)
                    `((run ,rules ((node . ,(*node-limit*)))))))

  ;; run egg to extract simplest
  (define exprs* (simplify-batch egg-query))
  (define spec** (last (first exprs*)))
  (define evens (map last (take (drop exprs* 1) (length even-identities))))
  (define odds (map last (take (drop exprs* (+ 1 (length even-identities))) (length odd-identities))))
  (define swaps (map last (drop exprs* (+ 1 (length even-identities) (length odd-identities)))))

  (define pairs (combinations (context-vars ctx) 2))
  (define components
    (connected-components
      (context-vars ctx)
      (for/list ([pair (in-list pairs)]
                 [swap (in-list swaps)]
                 #:when (equal? spec* swap))
        pair)))

  ; instructions (TODO: `equal?` should be an egraph query)
  (define abs-instrs
    (for/list ([var (in-list (context-vars ctx))]
               [even (in-list evens)]
               #:when (equal? spec** even))
      (list 'abs var)))
  (define negabs-instrs
    (for/list ([var (in-list (context-vars ctx))]
               [odd (in-list odds)]
               #:when (equal? spec** odd))
      (list 'negabs var)))
  (define sort-instrs
    (for/list ([component (in-list components)]
               #:when (> (length component) 1))
      (cons 'sort component)))
  
  (define instrs (append abs-instrs negabs-instrs sort-instrs))
  (define start-alts
    (if (flag-set? 'setup 'simplify)
        (list (make-alt-preprocessing init instrs)) ; TODO: initial simplify
        (list (make-alt-preprocessing init instrs))))
  
  (values start-alts instrs))

; 
; (define (find-preprocessing* initial specification context)
;   ;; f(x) = f(-x)
;   (define even-identities
;     (reap [sow]
;       (for ([variable (in-list (context-vars context))]
;             [representation (in-list (context-var-reprs context))])
;         (with-handlers ([exn:fail:user:herbie? void])
;           (define negate (get-parametric-operator 'neg representation))
;           ; Check if representation has an fabs operator
;           (define fabs (get-parametric-operator 'fabs representation))
;           (sow (replace-vars (list (cons variable (list negate variable))) specification))))))
;   ;; f(x) = -f(-x)
;   (define odd-identities
;     (with-handlers ([exn:fail:user:herbie? (const empty)])
;       (define negate (get-parametric-operator 'neg (context-repr context)))
;       ; Check if representation has an fabs operator
;       (define fabs (get-parametric-operator 'fabs (context-repr context)))
;       (map (lambda (expression) (list negate expression)) even-identities)))
;   ;; f(a, b) = f(b, a)
;   (define pairs (combinations (context-vars context) 2))
;   (define swap-identities
;     (for/list ([pair (in-list pairs)])
;       (match-define (list a b) pair)
;       (replace-vars (list (cons a b) (cons b a)) specification)))
;   (define query
;     (make-egg-query
;      (list*
;       initial
;       specification
;       (append even-identities odd-identities swap-identities))
;      (*simplify-rules*)))
;   ;; TODO: This is clearly bad and should be changed to something more general.
;   (define (split-others others)
;     (define-values (evens others*) (split-at others (length even-identities)))
;     (define-values (odds swaps) (split-at others* (length odd-identities)))
;     (values evens odds swaps))
;   (match-define
;     (cons
;      ;; The first element of the list returned by `simplify-batch` will be a list
;      ;; containing progressively simpler versions of `initial` at each iteration
;      ;; of the e-graph, the first of which will always be the unsimplified
;      ;; `initial` from iteration 0, which we want to exclude here to avoid adding
;      ;; it twice to the alt-table.
;      (app rest initials)
;      (app (curry map last)
;           (cons
;            specification*
;            (app split-others evens odds swaps))))
;     (simplify-batch query))
;   (define alternative (make-alt initial))
;   (define simplified
;     (cons
;      ;; We excluded the first element of `initials` above so that we can add it
;      ;; here manually, but without a self-referential history.
;      alternative
;      (remove-duplicates
;       (for/list ([expression (in-list initials)])
;         (alt
;          expression
;          (list 'simplify null query #f #f)
;          (list alternative)
;          '()))
;       alt-equal?)))
;   (define components
;     (connected-components
;      (context-vars context)
;      (filter-map
;       (lambda (pair swap) (and (equal? specification* swap) pair))
;       pairs
;       swaps)))
;   (define abs-instructions
;     (for/list ([variable (in-list (context-vars context))]
;                [even (in-list evens)]
;                #:when (equal? specification* even))
;       (list 'abs variable)))
;   (define negabs-instructions
;     (for/list ([variable (in-list (context-vars context))]
;                [odd (in-list odds)]
;                #:when (equal? specification* odd))
;       (list 'negabs variable)))
;   (define sort-instructions
;     (for/list ([component (in-list components)]
;                #:when (> (length component) 1))
;       (cons 'sort component)))
;   (values
;    (if (flag-set? 'setup 'simplify)
;        (for/list ([alt simplified])
;         (alt-add-preprocessing alt (append abs-instructions negabs-instructions sort-instructions)))
;        (list (make-alt-preprocessing initial (append abs-instructions negabs-instructions sort-instructions))))
;    ;; Absolute value should happen before sorting
;    (append abs-instructions negabs-instructions sort-instructions)))


(define (connected-components variables swaps)
  (define components (disjoint-set (length variables)))
  (for ([swap (in-list swaps)])
    (match-define (list a b) swap)
    (disjoint-set-union! components
                         (disjoint-set-find! components (index-of variables a))
                         (disjoint-set-find! components (index-of variables b))))
  (group-by
   (compose (curry disjoint-set-find! components) (curry index-of variables))
   variables))

(define (preprocess-pcontext context pcontext preprocessing)
  (define preprocess
    (apply compose (map
                    (curry instruction->operator context)
                    ;; Function composition applies the rightmost function first
                    (reverse preprocessing))))
  (for/pcontext ([(x y) pcontext]) (preprocess x y)))

(define (instruction->operator context instruction)
  (define variables (context-vars context))
  (define sort* (curryr sort (curryr </total (context-repr context))))
  (match instruction
    [(list 'sort component ...)
     (unless (subsequence? component variables)
       (error 'instruction->operator
              "component should always be a subsequence of variables"))
     (define indices (indexes-where variables (curryr member component)))
     (lambda (x y)
       (let* ([subsequence (map (curry list-ref x) indices)]
              [sorted (sort* subsequence)])
         (values (list-set* x indices sorted) y)))]
    [(list 'abs variable)
     (define index (index-of variables variable))
     (define abs (impl-info
                  (get-parametric-operator
                   'fabs
                   (list-ref (context-var-reprs context) index))
                  'fl))
     (lambda (x y) (values (list-update x index abs) y))]
    [(list 'negabs variable)
     (define index (index-of variables variable))
     (define negate-variable
       (impl-info (get-parametric-operator 'neg (list-ref (context-var-reprs context) index)) 'fl))
     (define negate-expression
       (impl-info (get-parametric-operator 'neg (context-repr context)) 'fl))
     (lambda (x y)
       ;; Negation is involutive, i.e. it is its own inverse, so t^1(y') = -y'
       (if (negative? (list-ref x index))
           (values
            (list-update x index negate-variable)
            (negate-expression y))
           (values x y)))]))

; until fixed point, iterate through preprocessing attempting to drop preprocessing with no effect on error
(define (remove-unnecessary-preprocessing expression context pcontext preprocessing #:removed [removed empty])
  (define-values (result newly-removed)
    (let loop ([preprocessing preprocessing] [i 0] [removed removed])
      (cond
        [(>= i (length preprocessing))
         (values preprocessing removed)]
        [(preprocessing-<=? expression context pcontext (drop-at preprocessing i) preprocessing)
         (loop (drop-at preprocessing i) i (cons (list-ref preprocessing i) removed))]
        [else
         (loop preprocessing (+ i 1) removed)])))
  (cond
    [(< (length result) (length preprocessing))
     (remove-unnecessary-preprocessing expression context pcontext result #:removed newly-removed)]
    [else
     (timeline-push! 'remove-preprocessing (map ~a newly-removed))
     result]))

(define (preprocessing-<=? expression context pcontext preprocessing1 preprocessing2)
  (define pcontext1 (preprocess-pcontext context pcontext preprocessing1))
  (define pcontext2 (preprocess-pcontext context pcontext preprocessing2))
  (<= (errors-score (errors expression pcontext1 context))
      (errors-score (errors expression pcontext2 context))))
