#lang racket

(require (only-in fpbench core->c c-header))
(require "syntax/read.rkt"
         "syntax/types.rkt"
         "web/common.rkt"
         "web/thread-pool.rkt"
         "common.rkt"
         "datafile.rkt"
         "sandbox.rkt")

(provide run-improve)

(define ((write-header! lang) p)
  (match lang
    [(or "fpcore" #f)
     (void)]
    ["c"
     (fprintf p (c-header))
     (newline p)]))

(define ((write-comment! lang) p fmt . args)
  (match lang
    [(or "fpcore" #f)
     (fprintf p ";; ")
     (apply fprintf p fmt args)
     (newline p)]
    ["c"
     (fprintf p "// ")
     (apply fprintf p fmt args)
     (newline p)]))

(define ((write-test! lang) p test)
  (match lang
    [(or "fpcore" #f)
     (for ([line (string-split (render-fpcore test) "\n")])
       (fprintf p ";; ")
       (fprintf p line)
       (newline p))]
    ["c"
     (for ([line (string-split (render-fpcore test) "\n")])
       (fprintf p "// ")
       (fprintf p line)
       (newline p))]))

(define ((write-result! lang) p res)
  (match lang
    [(or "fpcore" #f)
     (write (unparse-result res) p)]
    ["c"
     (when (table-row-output res)
       (define name (table-row-name res))
       (define vars (table-row-vars res))
       (define repr (get-representation (table-row-precision res)))
       (define ctx (context vars repr (map (const repr) vars)))
       (define core (program->fpcore (table-row-output res) ctx))
       (fprintf p (core->c core name)))]))

(define (print-outputs tests results p
                       #:seed [seed #f]
                       #:lang [lang #f])
  (define header! (write-header! lang))
  (define comment! (write-comment! lang))
  (define test! (write-test! lang))
  (define result! (write-result! lang))

  (when seed (comment! p "seed: ~a\n" seed))
  (header! p)

  (for ([res results] [test tests] #:when res)
    (define name (table-row-name res))
    (match (table-row-status res)
      ["error"
       (comment! p "Error in ~a" name)
       (test! p test)
       (result! p res)]
      ["crash"
       (comment! p "Crash in ~a" name)
       (test! p test)
       (result! p res)]
      ["timeout"
       (comment! p "~a times out in ~as" (/ (*timeout*) 1000) name)
       (test! p test)
       (result! p res)]
      [(? string?)
       (match-define (list (list start-cost start-err) (list best-cost best-err) _ ...)
                     (table-row-cost-accuracy res))
       (test! p test)
       (comment! p "")
       (comment! p "start error: ~a" start-err)
       (comment! p "start cost: ~a" start-cost)
       (comment! p "best error: ~a" best-err)
       (comment! p "best cost: ~a" best-cost)
       (result! p res)])
    (newline p)))

(define (run-improve input output
                     #:threads [threads #f]
                     #:lang [lang #f])
  (unless (set-member? '(#f "fpcore" "c") lang)
    (error 'run-improve "unrecognized language ~a" lang))

  (define seed (get-seed))
  (define tests (load-tests input))
  (define results (get-test-results tests #:threads threads #:seed seed #:profile #f #:dir #f))

  (if (equal? output "-")
      (print-outputs tests results (current-output-port) #:seed seed)
      (call-with-output-file output
         #:exists 'replace
         (λ (p) (print-outputs tests results p #:seed seed #:lang lang)))))
