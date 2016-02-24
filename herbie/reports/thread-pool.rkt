#lang racket

(require racket/place)
(require "../common.rkt")
(require "../test.rkt")
(require "../points.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../sandbox.rkt")
(require "make-graph.rkt")
(require "datafile.rkt")

(provide get-test-results get-table-data)

(define (get-table-data result rdir)
  (cond
   [(test-result? result)
    (let* ([name (test-name (test-result-test result))]
           [start-errors  (test-result-start-error  result)]
           [end-errors    (test-result-end-error    result)]
           [target-errors (test-result-target-error result)]

           [start-score (errors-score start-errors)]
           [end-score (errors-score end-errors)]
           [target-score (and target-errors (errors-score target-errors))]

           [est-start-score (errors-score (test-result-start-est-error result))]
           [est-end-score (errors-score (test-result-end-est-error result))])

      (let*-values ([(reals infs) (partition ordinary-float? (map - end-errors start-errors))]
                    [(good-inf bad-inf) (partition positive? infs)]
                    [(link) (path-element->string (last (explode-path rdir)))])
        (table-row name
                   (if target-score
                       (cond
                        [(< end-score (- target-score 1)) "gt-target"]
                        [(< end-score (+ target-score 1)) "eq-target"]
                        [(> end-score (+ start-score 1)) "lt-start"]
                        [(> end-score (- start-score 1)) "eq-start"]
                        [(> end-score (+ target-score 1)) "lt-target"])
                       (cond
                        [(and (< start-score 1) (< end-score (+ start-score 1))) "ex-start"]
                        [(< end-score (- start-score 1)) "imp-start"]
                        [(< end-score (+ start-score 1)) "apx-start"]
                        [else "uni-start"]))
                   start-score
                   end-score
                   (and target-score target-score)
                   (length good-inf)
                   (length bad-inf)
                   est-end-score
                   (program-variables (alt-program (test-result-start-alt result)))
                   (test-sampling-expr (test-result-test result))
                   (program-body (alt-program (test-result-start-alt result)))
                   (program-body (alt-program (test-result-end-alt result)))
                   (test-result-time result)
                   (test-result-bits result)
                   link)))]
   [(test-failure? result)
    (define link (path-element->string (last (explode-path rdir))))
    (match-define (test name vars sampling-expr input output _) (test-failure-test result))
    (table-row (test-name (test-failure-test result)) "crash"
               #f #f #f #f #f #f vars sampling-expr input #f
               (test-failure-time result) (test-failure-bits result) link)]
   [(test-timeout? result)
    (define link (path-element->string (last (explode-path rdir))))
    (match-define (test name vars sampling-expr input output _) (test-timeout-test result))
    (table-row (test-name (test-timeout-test result)) "timeout"
               #f #f #f #f #f #f vars sampling-expr input #f
               (test-timeout-time result) (test-timeout-bits result) link)]))

(define (make-graph-if-valid result tname index rdir #:profile profile?)
  (let* ([dir (build-path report-output-path rdir)])
    (with-handlers ([(const #f) (λ _ #f)])
      (when (not (directory-exists? dir))
        (make-directory dir))

      (write-file (build-path dir "graph.html")
                  ((cond [(test-result? result) make-graph]
                         [(test-timeout? result) make-timeout]
                         [(test-failure? result) make-traceback])
                   result profile?)))))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/|'|\"" "")]
         [index-label (number->string index)])
    (string-append index-label stripped-tname)))

(define (call-with-output-files names k)
  (let loop ([names names] [ps '()])
    (if (null? names)
        (apply k (reverse names))
        (if (car names)
            (call-with-output-file
                (car names) #:exists 'replace
                (λ (p) (loop (cdr names) (cons p ps))))
            (loop (cdr names) (cons #f ps))))))

(define (run-test index test #:seed seed #:profile profile?)
  (let* ([rdir (graph-folder-path (test-name test) index)]
         [rdir* (build-path report-output-path rdir)])

    (when (not (directory-exists? rdir*))
      (make-directory rdir*))
    
    (define result
      (call-with-output-files
       (list (build-path rdir* "debug.txt") (and profile? (build-path rdir* "profile.txt")))
       (λ () (get-test-results test rdir* #:seed seed #:profile profile?))))
    
    (make-graph-if-valid result (test-name test) index rdir #:profile profile?)
    (get-table-data result rdir)))

(define (make-worker)
  (place ch
    (let loop ([seed #f] [profile? #f])
      (match (place-channel-get ch)
	[`(init
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           points ,points
           profile? ,profile)

	 (set! seed vec)
         (set! profile? profile)
	 (*flags* flag-table)
	 (*num-iterations* iterations)
         (*num-points* points)]
        [`(apply ,self ,id ,test)
         (let ([result (run-test id test #:seed seed #:profile profile?)])
           (place-channel-put ch
             `(done ,id ,self ,result)))])
      (loop seed profile?))))

(define (print-test-result tr)
  (match (table-row-status tr)
    ["crash"  
     (printf "[   CRASH   ]\t~a\n" (table-row-name tr))]
    ["timeout"
     (printf "[  timeout  ]\t~a\n" (table-row-name tr))]
    [_
     (printf "[ ~ams]\t(~a→~a)\t~a\n" (~a (table-row-time tr) #:width 8)
             (~r (table-row-start tr) #:min-width 2 #:precision 0)
             (~r (table-row-result tr) #:min-width 2 #:precision 0)
             (table-row-name tr))]))

(define (run-workers progs threads #:seed seed #:profile profile?)
  (define config
    `(init rand ,seed
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           points ,(*num-points*)
           profile? ,profile?))

  (define workers
    (for/list ([wid (in-range threads)])
      (define worker (make-worker))
      (place-channel-put worker config)
      worker))

  (define work
    (for/list ([id (in-naturals)] [prog progs])
      (list id prog)))

  (printf "Starting ~a Herbie workers on ~a problems...\n" threads (length progs))
  (printf "Seed: ~a\n" seed)
  (for ([worker workers])
    (place-channel-put worker `(apply ,worker ,@(car work)))
    (set! work (cdr work)))

  (define outs
    (let loop ([out '()])
      (with-handlers ([exn:break?
                       (λ (_)
                         (printf "Terminating after ~a problem~a!\n"
                                 (length out) (if (= (length out) 1) "s" ""))
                         out)])
        (match-define `(done ,id ,more ,tr) (apply sync workers))

        (when (not (null? work))
          (place-channel-put more `(apply ,more ,@(car work)))
          (set! work (cdr work)))

        (define out* (cons (cons id tr) out))

        (printf "~a/~a\t" (~a (length out*) #:width 3 #:align 'right) (length progs))
        (print-test-result tr)

        (if (= (length out*) (length progs))
            out*
            (loop out*)))))

  (map place-kill workers)

  outs)

(define (run-nothreads progs #:seed seed #:profile profile?)
  (printf "Starting Herbie on ~a problems...\n" (length progs))
  (printf "Seed: ~a\n" seed)
  (define out '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (printf "Terminating after ~a problem~a!\n"
                             (length out) (if (= (length out) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test #:seed seed #:profile profile?))
      (printf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length progs))
      (print-test-result tr)
      (set! out (cons (cons i tr) out))))
  out)

(define (get-test-results progs #:threads [threads #f] #:seed seed #:profile [profile? #f])
  (when (and threads (> threads (length progs)))
    (set! threads (length progs)))

  (define outs
    (if threads
        (run-workers progs threads #:seed seed #:profile profile?)
        (run-nothreads progs #:seed seed #:profile profile?)))
  
  (define out (make-vector (length progs) #f))
  (for ([(idx result) (in-pairs outs)])
    (vector-set! out idx result))

  ; The use of > instead of < is a cleverness:
  ; the list of tests is accumulated in reverse, this reverses again.
  (vector->list out))
