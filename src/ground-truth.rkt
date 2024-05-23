#lang racket

(require math/bigfloat
         rival)

(require "syntax/types.rkt"
         "common.rkt" "float.rkt"
         "compiler.rkt" "timeline.rkt")

(provide eval-progs-real
         ground-truth-require-convergence 
         ival-eval
         make-search-func)

(struct discretization (convert distance))

(define bool-discretization
  (discretization identity
                  (lambda (x y) (if (eq? x y) 0 1))))

(define (representation->discretization repr)
  (discretization
   (representation-bf->repr repr)
   (lambda (x y) (- (ulp-difference x y repr) 1))))

(define ground-truth-require-convergence (make-parameter #t))

(define (is-samplable-interval disc interval)
  (define convert (discretization-convert disc))
  (define distance (discretization-distance disc))
  (define (close-enough? lo hi)
    (= (distance (convert lo) (convert hi)) 0))
  ((close-enough->ival close-enough?) interval))

(define (rival-compile exprs vars discs)
  (define fns (compile-specs exprs vars))
  (define outlen (length exprs))
  (define (rival-compiled inputs)
    (define outvec (apply fns inputs))
    (for/vector #:length outlen ([y (in-vector outvec)] [disc (in-list discs)])
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? y)) 'invalid)
       ; 'infinte case handle in `ival-eval`
       (ival-assert
        (if (ground-truth-require-convergence)
            (is-samplable-interval disc y)
            (ival (ival-hi (is-samplable-interval disc y))))
        'unsamplable)
       y)))
  rival-compiled)

;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func pre specs ctxs)
  (define vars (context-vars (car ctxs)))
  (define var-reprs (context-var-reprs (car ctxs)))
  (define discs (map (compose representation->discretization context-repr) ctxs))
  (define compiled (rival-compile (cons pre specs) vars (cons bool-discretization discs)))
  (define (compiled-spec . inputs)
    ; inputs can either be intervals or representation values
    (define inputs*
      (for/list ([input (in-list inputs)] [repr (in-list var-reprs)])
        (if (ival? input) input (ival ((representation-repr->bf repr) input)))))
    (define outvec (compiled inputs*))
    (define ival-pre (vector-ref outvec 0))
    (for/list ([y (in-vector outvec 1)] [ctx (in-list ctxs)])
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? ival-pre)) 'invalid)
       (ival-assert ival-pre 'precondition)
       y)))
  compiled-spec)

(define (ival-eval fn ctxs pt [iter 0])
  (define start (current-inexact-milliseconds))
  (define <-bfs
    (for/list ([ctx (in-list ctxs)])
      (representation-bf->repr (context-repr ctx))))
  (define-values (status final-prec value)
    (let loop ([iter iter])
      (define exs
        (parameterize ([*sampling-iteration* iter]) (apply fn pt)))
      (match-define (ival err err?) (apply ival-or (map ival-error? exs)))
      (define iter* (+ 1 iter))
      (cond
        [err
         (values err iter #f)]
        [(not err?)
         (values 'valid iter
                 (for/list ([ex exs] [<-bf <-bfs]) (<-bf (ival-lo ex))))]
        [(> iter* (*max-sampling-iterations*))
         (values 'exit iter #f)]
        [else
         (loop iter*)])))
  (timeline-push!/unsafe 'outcomes (- (current-inexact-milliseconds) start)
                         final-prec (~a status) 1)
  (values status value))

; ENSURE: all contexts have the same list of variables
(define (eval-progs-real progs ctxs)
  (define fn (make-search-func '(TRUE) progs ctxs))
  (define bad-pt 
    (for/list ([ctx* (in-list ctxs)])
      ((representation-bf->repr (context-repr ctx*)) +nan.bf)))
  (define (<eval-prog-real> . pt)
    (define-values (result exs) (ival-eval fn ctxs pt))
    (or exs bad-pt))
  <eval-prog-real>)

