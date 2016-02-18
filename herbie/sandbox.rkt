#lang racket
(require profile)
(require math/bigfloat)
(require racket/engine)

(require "interface/interact.rkt")
(require "common.rkt")
(require "debug.rkt")
(require "programs.rkt")
(require "points.rkt")
(require "test.rkt")
(require "alternative.rkt")
(require "main.rkt")

(provide get-test-result *reeval-pts* *timeout*
         (struct-out test-result) (struct-out test-failure) (struct-out test-timeout))


; For things that don't leave a thread
(struct test-result
  (test rdir time bits
   start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error timeline))
(struct test-failure (test bits exn time rdir timeline))
(struct test-timeout (test bits time rdir timeline))

(define *reeval-pts* (make-parameter 8000))
(define *timeout* (make-parameter (* 1000 60 10)))

(define (default-setup)
  (set-debug-level! #t #t)
  (set-debug-level! 'backup-simplify #f))

(define (get-p&es context)
  (call-with-values
      (λ ()
        (for/lists (pts exs)
            ([(pt ex) (in-pcontext context)])
          (values pt ex)))
    list))

(define (get-test-result test rdir
                         #:setup! [setup! default-setup]
                         #:seed [seed #f] #:profile [profile? #f])
  (define (file name) (build-path rdir name))

  (define (on-error e) `(error ,e ,(bf-precision)))

  (define (compute-result test)
    (call-with-output-file (file "debug.txt") #:exists 'replace
      (λ (p)
        (parameterize ([*debug-port* p])
          (when seed (set-seed! seed))
          (setup!)
          (with-handlers ([(const #t) on-error])
            (match-define (list alt context)
                          (run-improve (test-program test)
                                       (*num-iterations*)
                                       #:get-context #t
                                       #:samplers (test-samplers test)))
            `(good ,(make-alt (test-program test)) ,alt ,context))))))

  (define (in-engine _)
    (if profile?
        (with-output-to-file (file "profile.txt") #:exists 'replace
          (λ () (profile (compute-result test))))
        (compute-result test)))
  
  (let* ([start-time (current-inexact-milliseconds)] [eng (engine in-engine)])
    (engine-run (*timeout*) eng)

    (match (engine-result eng)
      [`(good ,start ,end ,context)
       (define newcontext
         (parameterize ([*num-points* (*reeval-pts*)])
           (prepare-points (alt-program start) (test-samplers test))))
       (match-define (list newpoints newexacts) (get-p&es newcontext))
       (match-define (list points exacts) (get-p&es context))
       (test-result test rdir
                    (- (current-inexact-milliseconds) start-time)
                    (bf-precision)
                    start end points exacts
                    (errors (alt-program start) context)
                    (errors (alt-program end) context)
                    newpoints newexacts
                    (errors (alt-program start) newcontext)
                    (errors (alt-program end) newcontext)
                    (if (test-output test)
                        (errors (test-target test) newcontext)
                        #f)
                    (^timeline^))]
      [`(error ,e ,bits)
       (test-failure test bits e (- (current-inexact-milliseconds) start-time) rdir (^timeline^))]
      [#f
       (test-timeout test (bf-precision) (*timeout*) rdir (^timeline^))])))
