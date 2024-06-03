#lang racket

(require math/bigfloat
         math/flonum
         rival)

(require "syntax/types.rkt"
         "common.rkt"
         "compiler.rkt" "timeline.rkt" "config.rkt"
         (only-in "errors.rkt" warn))

(provide eval-progs-real
         ground-truth-require-convergence 
         ival-eval
         make-search-func
         sollya-eval)

(define ground-truth-require-convergence (make-parameter #t))

(define (is-samplable-interval repr interval)
  (define <-bf (representation-bf->repr repr))
  (define (close-enough? lo hi)
    (let ([lo* (<-bf lo)] [hi* (<-bf hi)])
      (or (equal? lo* hi*) (and (number? lo*) (= lo* hi*)))))
  ((close-enough->ival close-enough?) interval))


;; Returns a function that maps an ival to a list of ivals
;; The first element of that function's output tells you if the input is good
;; The other elements of that function's output tell you the output values
(define (make-search-func pre specs ctxs)
  (define fns (compile-specs (cons pre specs) (context-vars (car ctxs)) (context-repr (car ctxs))))
  ; inputs can either be intervals or representation values
  (define (compiled-spec . inputs)
    (define inputs*
      (for/list ([input (in-list inputs)]
                 [repr (context-var-reprs (car ctxs))])
        (if (ival? input) input (ival ((representation-repr->bf repr) input)))))
    (define outvec (apply fns inputs*))
    (define ival-pre (vector-ref outvec 0))
    (for/list ([y (in-vector outvec 1)] [ctx (in-list ctxs)])
      (define repr (context-repr ctx))
      (ival-then
       ; The two `invalid` ones have to go first, because later checks
       ; can error if the input is erroneous
       (ival-assert (ival-not (ival-error? y)) 'invalid)
       (ival-assert (ival-not (ival-error? ival-pre)) 'invalid)
       (ival-assert ival-pre 'precondition)
       ; 'infinte case handle in `ival-eval`
       (ival-assert
        (if (ground-truth-require-convergence)
            (is-samplable-interval repr y)
            (ival (ival-hi (is-samplable-interval repr y))))
        'unsamplable)
       y)))
  compiled-spec)

(define (ival-eval fn ctxs pt [iter 0])
  (define start (current-inexact-milliseconds))
  (define <-bfs
    (for/list ([ctx (in-list ctxs)])
      (representation-bf->repr (context-repr ctx))))
  (define-values (status final-iter value)
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
  (define time (- (current-inexact-milliseconds) start))
  (values status final-iter value time))

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

(define (sollya-eval fn-sollya pt rival-status rival-final-iter rival-exs rival-time)
  (cond
    ; Rival has produced valid outcomes
    [(equal? rival-status 'valid)
         
     ; Interval evaluation
     #;(match-define (list internal-interval-time external-interval-time
                         (list sollya-lower sollya-upper) sollya-interval-status) (fn-sollya pt #t))

     ; Point evaluation
     (match-define (list internal-point-time external-point-time sollya-point sollya-point-status) (fn-sollya pt #f))

     
         
     (define match (if (and (equal? sollya-point-status 'valid)
                            (<= 2 (flonums-between (last rival-exs) sollya-point)))
                       #t
                       #f))

     ; When a point failed - maybe it is a memory issue or something
     (when match
       (sleep 0.1)
       (match-define (list internal-point-time* external-point-time* sollya-point* sollya-point-status*) (fn-sollya pt #f))
       (set! match (if (and (equal? sollya-point-status* 'valid)
                            (<= 2 (flonums-between (last rival-exs) sollya-point*)))
                       #t
                       #f))
       (set! sollya-point sollya-point*)
       (set! sollya-point-status sollya-point-status*)
       (set! external-point-time external-point-time*))

     (when (equal? sollya-point-status rival-status)
       (timeline-push!/unsafe 'outcomes external-point-time
                              rival-final-iter (format "~a-sollya" sollya-point-status) 1)
       (timeline-push!/unsafe 'outcomes rival-time
                              rival-final-iter (format "~a-rival" rival-status) 1))
     
     (when match
       #;(timeline-push!/unsafe 'sollya-eval
                              pt (~a (last rival-exs)) (~a sollya-point)
                              (symbol->string rival-status) (symbol->string sollya-point-status)
                              rival-final-iter external-point-time)
       (warn 'ground-truth (format "Sollya didn't converge on: pt=~a, sollya-point=~a, rival-point=~a\n" pt sollya-point (last rival-exs))))]

    ; Rival has not produced a valid outcome, rival-exs=#f, nothing to compare to Sollya's output, only statuses comparisons
    [(not (equal? rival-status 'precondition))
         
     ; Interval evaluation
     #;(match-define (list internal-interval-time external-interval-time
                         (list sollya-lower sollya-upper) sollya-interval-status) (fn-sollya pt #t))

     ; Point evaluation
     #;(match-define (list internal-point-time external-point-time sollya-point sollya-point-status) (fn-sollya pt #f))

     #;(timeline-push!/unsafe 'outcomes external-point-time
                            rival-final-iter (format "~a-sollya" sollya-point-status) 1)
     #;(timeline-push!/unsafe 'outcomes rival-time
                            rival-final-iter (format "~a-rival" rival-status) 1)
         
     #;(define match (or (equal? sollya-point-status rival-status)
                       (and (equal? sollya-point-status 'invalid)
                            (equal? rival-status 'unsamplable))
                       (and (equal? sollya-point-status 'unsamplable)
                            (equal? rival-status 'invalid))))
     
     #;(unless match
       (timeline-push!/unsafe 'sollya-eval
                              pt (~a rival-exs) (~a sollya-point)
                              (symbol->string rival-status) (symbol->string sollya-point-status)
                              rival-final-iter sollya-iter
                              external-point-time))]))
