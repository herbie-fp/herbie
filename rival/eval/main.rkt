#lang racket

(require racket/flonum)
(require "../ops/all.rkt"
         "machine.rkt"
         "compile.rkt"
         "run.rkt"
         "types.rkt"
         "adjust.rkt")

(provide rival-type
         rival-functions
         rival-types
         rival-compile
         rival-apply
         rival-analyze
         rival-analyze-with-hints
         (struct-out exn:rival)
         (struct-out exn:rival:invalid)
         (struct-out exn:rival:unsamplable)
         (struct-out discretization)
         *rival-max-precision*
         *rival-max-iterations*
         *rival-use-shorthands*
         *rival-name-constants*
         rival-profile
         (struct-out execution)
         *rival-profile-executions*)

(define (rival-machine-full machine vhint)
  (set-rival-machine-iteration! machine (*sampling-iteration*))
  (rival-machine-adjust machine vhint)
  (cond
    [(>= (*sampling-iteration*) (*rival-max-iterations*)) (values #f #f #f #t #f)]
    [else
     (rival-machine-run machine vhint)
     (rival-machine-return machine)]))

(struct exn:rival exn:fail ())
(struct exn:rival:invalid exn:rival (pt))
(struct exn:rival:unsamplable exn:rival (pt))

(struct execution (name number precision time memory iteration) #:prefab)

(define (rival-profile machine param)
  (match param
    ['instructions (vector-length (rival-machine-instructions machine))]
    ['iterations (rival-machine-iteration machine)]
    ['bumps (rival-machine-bumps machine)]
    ['executions
     (define profile-ptr (rival-machine-profile-ptr machine))
     (define profile-instruction (rival-machine-profile-instruction machine))
     (define profile-number (rival-machine-profile-number machine))
     (define profile-time (rival-machine-profile-time machine))
     (define profile-memory (rival-machine-profile-memory machine))
     (define profile-precision (rival-machine-profile-precision machine))
     (define profile-iteration (rival-machine-profile-iteration machine))
     (begin0 (for/vector #:length profile-ptr
                         ([instruction (in-vector profile-instruction 0 profile-ptr)]
                          [number (in-vector profile-number 0 profile-ptr)]
                          [precision (in-vector profile-precision 0 profile-ptr)]
                          [time (in-flvector profile-time 0 profile-ptr)]
                          [memory (in-vector profile-memory 0 profile-ptr)]
                          [iter (in-vector profile-iteration 0 profile-ptr)])
               (execution instruction number precision time memory iter))
       (set-rival-machine-profile-ptr! machine 0))]))

(define (ival-real x)
  (ival x))

; Assumes that hint (if provided) is correct for the given pt
(define (rival-apply machine pt [hint #f])
  ; Load arguments
  (rival-machine-load machine (vector-map ival-real pt))
  (let loop ([iter 0])
    (define-values (good? done? bad? stuck? fvec)
      (parameterize ([*sampling-iteration* iter])
        (rival-machine-full machine (or hint (rival-machine-default-hint machine)))))
    (cond
      [bad? (raise (exn:rival:invalid "Invalid input" (current-continuation-marks) pt))]
      [done? fvec]
      [stuck? (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [(>= iter (*rival-max-iterations*))
       (raise (exn:rival:unsamplable "Unsamplable input" (current-continuation-marks) pt))]
      [else (loop (+ 1 iter))])))

; Assumes that hint (if provided) is correct for the given rect
(define (rival-analyze-with-hints machine rect [hint #f])
  ; Load arguments
  (rival-machine-load machine rect)
  (define-values (good? done? bad? stuck? fvec)
    (parameterize ([*sampling-iteration* 0])
      (rival-machine-full machine (or hint (rival-machine-default-hint machine)))))
  (define-values (hint* hint*-converged?)
    (make-hint machine (or hint (rival-machine-default-hint machine))))
  (list (ival (or bad? stuck?) (not good?)) hint* hint*-converged?))

(define (rival-analyze machine rect)
  (car (rival-analyze-with-hints machine rect)))
