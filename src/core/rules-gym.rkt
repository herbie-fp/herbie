#lang racket

(require "egg-herbie.rkt"
         "../utils/float.rkt"
         "../syntax/types.rkt"
         "../syntax/load-plugin.rkt"
         "rival.rkt")

(define max-op-cnt 5)
(define min-op-cnt 2)
(define random-choices '(neg + - * const))
(define vars-choices '(x y z))
(define num-expressions 100)
(define num-testing-points 50)

(define (get-constant)
  (random -3 4))

(define (get-var)
  (define rnd (random 0 (length vars-choices)))
  (list-ref vars-choices rnd))

(define (generate-expr)
  (let loop ([count (random min-op-cnt max-op-cnt)])
    (cond
      [(<= count 0)
       (if (zero? (random 0 2))
           (get-constant)
           (get-var))]
      [else
       (define rnd (random 0 (length random-choices)))
       (define node (list-ref random-choices rnd))
       (match node
         [(or '+ '- '*) (list node (loop (- count 1)) (loop (- count 2)))]
         ['neg (list node (loop (- count 1)))]
         ['const (get-constant)])])))

(define (generate-points cnt)
  (define-values (sampler)
    (λ ()
      (vector-map random-generate
                  (list->vector (map (const (get-representation 'binary64)) vars-choices)))))
  (for/list ([n (in-range cnt)])
    (sampler)))

(define (evaluate-exprs exprs)
  (define ctxs (map (const (make-debug-context vars-choices)) exprs))
  (define compiler (make-real-compiler exprs ctxs))
  (define test-pts (generate-points num-testing-points))

  (define exs
    (for/list ([pt (in-list test-pts)])
      (define-values (statuses values) (real-apply compiler pt))
      (map (λ (ex idx) (cons ex idx)) values (range (length exprs)))))
  exs)

(define (group-exprs-by-evaluations exs)
  (define groups
    (for/list ([ex (in-list exs)])
      (define group (group-by car ex))
      (define filtered (filter (λ (x) (< 1 (length x))) group))
      (map (λ (x) (map cdr x)) filtered))) ; leave only exprs indices results of which do match

  #;(pretty-print groups)
  (define equal-exprs (map list->set (car groups)))
  (for ([g (in-list (rest groups))])
    (define equal-exprs*
      (for/list ([candidate (in-list g)])
        (define intersections (map (λ (x) (set-intersect (list->set candidate) x)) equal-exprs))
        (filter (λ (x) (> (set-count x) 1)) intersections)))
    (set! equal-exprs (append* equal-exprs*)))
  equal-exprs)

(module+ main
  (load-herbie-plugins)
  (define exprs
    (remove-duplicates (for/list ([n (in-range num-expressions)])
                         (generate-expr))))

  #;(pretty-print exprs)

  (define exs (evaluate-exprs exprs))
  (define e-classes (group-exprs-by-evaluations exs))

  (pretty-print e-classes)
  (for ([e-class (in-list e-classes)]
        [n (in-naturals)])
    (printf "E-class ~a:\n" n)
    (for ([idx (in-set e-class)])
      (printf "\t~a\n" (list-ref exprs idx)))))
