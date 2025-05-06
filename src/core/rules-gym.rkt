#lang racket

(require "egg-herbie.rkt"
         "../utils/float.rkt"
         "../syntax/types.rkt"
         "../syntax/load-plugin.rkt"
         "rival.rkt")

(define max-op-cnt 8)
(define min-op-cnt 2)
(define random-choices '(neg + - * / #;const var))
(define vars-choices '(x y z))
(define num-expressions 1000)
(define num-testing-points 100)
(define verbose #t)

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
         [(or '+ '- '* '/) (list node (loop (- count 1)) (loop (- count 2)))]
         [(or 'neg 'sin 'cos 'tan) (list node (loop (- count 1)))]
         ['const (get-constant)]
         ['var (get-var)])])))

(define (generate-points cnt)
  (define-values (sampler)
    (λ ()
      (vector-map random-generate
                  (list->vector (map (const (get-representation 'binary64)) vars-choices)))))
  (for/list ([n (in-range cnt)])
    (sampler)))

(define (evaluate-exprs exprs)
  (define ctx (make-debug-context vars-choices))
  (define compilers
    (for/list ([expr exprs])
      (make-real-compiler (list expr) (list ctx))))
  (define test-pts (generate-points num-testing-points))

  (define exs
    (for/list ([pt (in-list test-pts)])
      (define values
        (for/list ([compiler compilers])
          (define-values (status value) (real-apply compiler pt))
          value))
      (with-handlers ([exn:fail?
                       (λ (e) (map (λ (idx) (cons #f idx)) (range (length exprs))))]) ; some errors
        (map (λ (ex idx) (cons ex idx)) values (range (length exprs)))))) ; attaching id of expression
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

(define (check-rewrites e-classes exprs)
  (for ([e-class (in-list e-classes)])
    (define a (list-ref exprs (set-first e-class)))
    (for ([idx (in-set (set-rest e-class))])
      (define b (list-ref exprs idx))
      (when verbose
        (printf "\n\n~a -> ~a\n" a b))
      (define proof (check-rewrite-exists a b))
      (when verbose
        (pretty-print proof))
      (set! a b))))

(module+ main
  (load-herbie-plugins)
  (define exprs
    (remove-duplicates (for/list ([n (in-range num-expressions)])
                         (generate-expr))))

  (define exs (evaluate-exprs exprs))
  (define e-classes (group-exprs-by-evaluations exs))

  (when verbose
    (pretty-print e-classes)
    (for ([e-class (in-list e-classes)]
          [n (in-naturals)])
      (printf "E-class ~a:\n" n)
      (for ([idx (in-set e-class)])
        (printf "\t~a\n" (list-ref exprs idx)))))

  (check-rewrites e-classes exprs))
