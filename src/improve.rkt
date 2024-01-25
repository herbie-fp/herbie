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

(define (in-table-row tr)
  (unless (table-row? tr)
    (raise-argument-error 'in-table-row "table-row?" tr))
  (match (table-row-status tr)
    [(or "error" "crash" "timeout")
     (raise-argument-error 'in-table-row "table row not valid result" tr)]
    [_
     (match-define (list _ (list best-cost best-err) other) (table-row-cost-accuracy tr))
     (define first (list best-cost best-err (table-row-output tr)))
     (in-list (cons first other))]))

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

(define ((write-result! lang) p res expr)
  (match lang
    [(or "fpcore" #f)
     (pretty-print (unparse-result res #:expr expr) p 1)]
    ["c"
     (when (table-row-output res)
       (define name (table-row-name res))
       (define vars (table-row-vars res))
       (define repr (get-representation (table-row-precision res)))
       (define ctx (context vars repr (map (const repr) vars)))
       (define core (program->fpcore expr ctx))
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
       (result! p res #f)]
      ["crash"
       (comment! p "Crash in ~a" name)
       (test! p test)
       (result! p res #f)]
      ["timeout"
       (comment! p "~a times out in ~as" (/ (*timeout*) 1000) name)
       (test! p test)
       (result! p res #f)]
      [(? string?)
       (for ([i (in-naturals 1)] [entry (in-table-row res)])
         (match-define (list cost _ expr) entry)
         (define res*
           (struct-copy table-row res
             [name (format "~a variant ~a" (table-row-name res) i)]))
         (test! p test)
         (comment! p "---")
         (comment! p "cost: ~a" cost)
         (result! p res* expr)
         (newline p))])
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
