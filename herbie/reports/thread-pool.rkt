#lang racket

(require profile)
(require racket/place)
(require racket/engine)
(require math/bigfloat)
(require unstable/sequence)
(require "../common.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../test.rkt")
(require "../alternative.rkt")
(require "../main.rkt")
(require "make-graph.rkt")
(require "datafile.rkt")
(require "../interface/interact.rkt")

(provide get-test-results get-test-result get-table-data *reeval-pts* *timeout* *seed*)

(define *reeval-pts* (make-parameter 8000))
(define *seed* (get-seed))
(define *timeout* (make-parameter (* 1000 60 10)))
(define *profile?* #f)

(define (get-test-result test rdir #:setup! [setup! (λ ()
						      (set-debug-level! #t #t)
						      (set-debug-level! 'backup-simplify #f))]
                         #:seed [seed #f])
  (define (file name) (build-path rdir name))
  (set-seed! (or seed *seed*))

  (define (get-p&es context)
    (call-with-values
	(λ ()
	   (for/lists (pts exs)
	       ([(pt ex) (in-pcontext context)])
	     (values pt ex)))
      list))

  (define (on-error e) `(error ,e ,(bf-precision)))

  (define (compute-result test)
    (call-with-output-file (file "debug.txt") #:exists 'replace
      (λ (p)
        (parameterize ([*debug-port* p])
          (setup!)
          (with-handlers ([(const #t) on-error])
            (match-define (list alt context)
                          (run-improve (test-program test)
                                       (*num-iterations*)
                                       #:get-context #t
                                       #:samplers (test-samplers test)))
            `(good ,(make-alt (test-program test)) ,alt ,context))))))

  (define (in-engine _)
    (if *profile?*
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
                        #f))]
      [`(error ,e ,bits)
       (test-failure test bits e (- (current-inexact-milliseconds) start-time) rdir)]
      [#f
       (test-timeout test (bf-precision) rdir)])))

(define (get-table-data result)
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
                    [(link) (path-element->string (last (explode-path (test-result-rdir result))))])
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
    (define link (path-element->string (last (explode-path (test-failure-rdir result)))))
    (match-define (test name vars sampling-expr input output _) (test-failure-test result))
    (table-row (test-name (test-failure-test result)) "crash"
               #f #f #f #f #f #f vars sampling-expr input #f
               (test-failure-time result) (test-failure-bits result) link)]
   [(test-timeout? result)
    (define link (path-element->string (last (explode-path (test-timeout-rdir result)))))
    (match-define (test name vars sampling-expr input output _) (test-timeout-test result))
    (table-row (test-name (test-timeout-test result)) "timeout"
               #f #f #f #f #f #f vars sampling-expr input #f
               (*timeout*) (test-timeout-bits result) link)]))

(define (make-graph-if-valid result tname index rdir)
  (let* ([dir (build-path report-output-path rdir)])
    (with-handlers ([(const #f) (λ _ #f)])
      (when (not (directory-exists? dir))
        (make-directory dir))

      (write-file (build-path dir "graph.html")
                  ((cond [(test-result? result) make-graph]
                         [(test-timeout? result) make-timeout]
                         [(test-failure? result) make-traceback])
                   result *profile?*)))))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/|'|\"" "")]
         [index-label (number->string index)])
    (string-append index-label stripped-tname)))

(define (run-test index test)
  (let* ([rdir (graph-folder-path (test-name test) index)]
         [rdir* (build-path report-output-path rdir)])

    (when (not (directory-exists? rdir*))
      (make-directory rdir*))

    (let ([result (get-test-result test rdir*)])
      (make-graph-if-valid result (test-name test) index rdir)
      (get-table-data result))))

(define (make-worker)
  (place ch
    (let loop ()
      (match (place-channel-get ch)
	[`(init
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           points ,points
           profile? ,profile?)

	 (set! *seed* vec)
         (set! *profile?* profile?)
	 (*flags* flag-table)
	 (*num-iterations* iterations)
         (*num-points* points)]
        [`(apply ,self ,id ,test)
         (let ([result (run-test id test)])
           (place-channel-put ch
             `(done ,id ,self ,result)))])
      (loop))))

(define (run-workers progs threads profile?)
  (define config
    `(init rand ,(get-seed)
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
        (match (table-row-status tr)
         ["crash"   (printf "[   CRASH   ]")]
         ["timeout" (printf "[  timeout  ]")]
         [_         (printf "[ ~ams]" (~a (table-row-time tr) #:width 8))])
        (printf "\t~a\n" (table-row-name tr))

        (if (= (length out*) (length progs))
            out*
            (loop out*)))))

  (map place-kill workers)

  outs)

(define (run-nothreads progs profile?)
  (set! *profile?* profile?)
  (printf "Starting Herbie on ~a problems...\n" (length progs))
  (define out '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (printf "Terminating after ~a problem~a!\n"
                             (length out) (if (= (length out) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test))
      (printf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length progs))
      (match (table-row-status tr)
        ["crash"   (printf "[   CRASH   ]")]
        ["timeout" (printf "[  timeout  ]")]
        [_         (printf "[ ~ams]" (~a (table-row-time tr) #:width 8))])
      (printf "\t~a\n" (table-row-name tr))
      (set! out (cons (cons i tr) out))))
  out)

(define (get-test-results progs #:threads [threads #f] #:profile [profile? #f])
  (when (and threads (> threads (length progs)))
    (set! threads (length progs)))

  (define outs
    (if threads
        (run-workers progs threads profile?)
        (run-nothreads progs profile?)))
  
  (define out (make-vector (length progs) #f))
  (for ([(idx result) (in-pairs outs)])
    (vector-set! out idx result))

  ; The use of > instead of < is a cleverness:
  ; the list of tests is accumulated in reverse, this reverses again.
  (vector->list out))
