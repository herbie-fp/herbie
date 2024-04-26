#lang racket

(require "core/egg-herbie.rkt" "core/simplify.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt" "syntax/sugar.rkt"
         "syntax/types.rkt" "alternative.rkt" "accelerator.rkt" "common.rkt"
         "programs.rkt" "points.rkt" "timeline.rkt" "float.rkt")

(provide find-preprocessing preprocess-pcontext remove-unnecessary-preprocessing)

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
(define (initial-simplify init ctx)
  (define rules (real-rules (*simplify-rules*)))
  (define lowering-rules (platform-lowering-rules))

  ; egg schedule (2-phases for real rewrites and implementation selection)
  (define schedule
    `((run ,rules ((node . ,(*node-limit*))))
      (run ,lowering-rules ((iteration . 1) (scheduler . simple)))))

  ; egg query
  (define spec (expand-accelerators (*rules*) (prog->spec init)))
  (define egg-query
    (make-egg-query (list spec)
                    (list (context-repr ctx))
                    schedule
                    #:extractor (typed-egg-extractor platform-egg-cost-proc)))

  ; run egg 
  (match-define (list simplified) (simplify-batch egg-query))

  ; alternatives
  (define start-alt (make-alt init))
  (cons start-alt
        (remove-duplicates
          (for/list ([expr (rest simplified)])
            (alt expr `(simplify () ,egg-query #f #f) (list start-alt) '()))
          alt-equal?)))


;; See https://pavpanchekha.com/blog/symmetric-expressions.html
(define (find-preprocessing init spec ctx)
  (define spec* (prog->spec spec))

  ;; identities
  (define even-identities (make-even-identities spec* ctx))
  (define odd-identities (make-odd-identities spec* ctx))
  (define swap-identities (make-swap-identities spec* ctx))

  ;; expressions
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
                 #:when (equal? spec** swap))
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
        (for/list ([altn (initial-simplify init ctx)])
          (alt-add-preprocessing altn instrs))
        (list (make-alt-preprocessing init instrs))))
  
  (values start-alts instrs))


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
