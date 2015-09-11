#lang racket

(require profile)
(require racket/place)
(require racket/engine)
(require math/bigfloat)
(require "../common.rkt")
(require "../programs.rkt")
(require "../points.rkt")
(require "../test.rkt")
(require "../alternative.rkt")
(require "../main.rkt")
(require "../interface/interact.rkt")
(require "../loop-errors.rkt")
(require "make-graph.rkt")
(require "datafile.rkt")

(provide get-test-results)

(define *reeval-pts* 8)
(define *seed* #f)
(define *timeout* (* 1000 60 10))
(define *profile?* #f)
(define *make-graph?* #f)

(define (get-test-result test rdir)
  (define (file name) (build-path report-output-path rdir name))

  ; Reseed random number generator
  (current-pseudo-random-generator (vector->pseudo-random-generator *seed*))

  (define (get-p&es context)
    (call-with-values
	(λ ()
	   (for/lists (pts exs)
	       ([(pt ex) (in-pcontext context)])
	     (values pt ex)))
      list))

  (define (compute-result test)
    (define (close-debug-port x)
      (close-output-port (*debug-port*))
      x)

    (parameterize ([*debug-port* (open-output-file (file "debug.txt") #:exists 'replace)] [*debug* #t])
      (with-handlers ([(const #t)
                       (λ (e) (close-debug-port `(error ,e ,(bf-precision))))])
	(match-let ([`(,alt ,context) (run-improve (test-program test) (*num-iterations*)
					          #:get-context #t #:samplers (test-samplers test))])
	  (close-debug-port (list 'good (make-alt (test-program test)) alt context))))))

  (define (in-engine _)
    (if *profile?*
        (parameterize ([current-output-port (open-output-file (file "profile.txt") #:exists 'replace)])
          (profile (compute-result test)))
        (compute-result test)))

  (let* ([start-time (current-inexact-milliseconds)] [eng (engine in-engine)])
    (engine-run *timeout* eng)

    (match (engine-result eng)
      [`(good ,start ,end ,context)
       (define newcontext
         (parameterize ([*num-points* *reeval-pts*])
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
                    (loop-aware-errors (alt-program start) newcontext)
                    (loop-aware-errors (alt-program end) newcontext)
                    (if (test-output test)
                        (loop-aware-errors `(λ ,(test-vars test) ,(test-output test))
					   newcontext)
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

           [start-score   (loop-aware-error-score* start-errors)]
           [end-score     (loop-aware-error-score* end-errors)]
           [target-score (and target-errors
                              (loop-aware-error-score* target-errors))]
           [est-start-score (errors-score (test-result-start-est-error result))]
           [est-end-score (errors-score (test-result-end-est-error result))])

      (let*-values ([(reals infs) (partition ordinary-float?
                                             (for/list ([end-err-lst end-errors] [start-err-lst start-errors])
                                               (if (or (null? end-err-lst) (null? start-err-lst)) 0
                                                   (- (car (take-right end-err-lst 1))
                                                      (car (take-right start-err-lst 1))))))]
                    [(good-inf bad-inf) (partition positive? infs)])
        (table-row name
                   (if target-score
                       (cond
                        [(< end-score (- target-score 1)) "gt-target"]
                        [(< end-score (+ target-score 1)) "eq-target"]
                        [(> end-score (+ start-score 1)) "lt-start"]
                        [(> end-score (- start-score 1)) "eq-start"]
                        [(>= end-score (+ target-score 1)) "lt-target"])
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
                   (program-body (alt-program (test-result-start-alt result)))
                   (program-body (alt-program (test-result-end-alt result)))
                   (test-result-time result)
                   (test-result-bits result)
                   (test-result-rdir result))))]
   [(test-failure? result)
    (table-row (test-name (test-failure-test result)) "crash"
               #f #f #f #f #f #f #f (test-input (test-failure-test result)) #f
               (test-failure-time result) (test-failure-bits result) (test-failure-rdir result))]
   [(test-timeout? result)
    (table-row (test-name (test-timeout-test result)) "timeout"
               #f #f #f #f #f #f #f (test-input (test-timeout-test result)) #f
               *timeout* (test-timeout-bits result) (test-timeout-rdir result))]))

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

    (let ([result (get-test-result test rdir)])
      (make-graph-if-valid result (test-name test) index rdir)
      (get-table-data result))))

(define (make-worker)
  (place ch
    (let loop ()
      (match (place-channel-get ch)
	[`(init
	   wid ,worker-id
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

(define (make-manager)
  (place ch
    (define workers '())
    (define work '())
    (define next-wid 0)
    (let/ec abort
      (let loop ()
        ; Message handler
        (match (apply sync ch workers)
          ['make-worker
           (let ([new-worker (make-worker)])
             (place-channel-put new-worker
                                `(init wid ,(begin0 next-wid
                                              (set! next-wid (add1 next-wid)))
                                       rand ,(pseudo-random-generator->vector
                                              (current-pseudo-random-generator))
                                       flags ,(*flags*)
                                       num-iters ,(*num-iterations*)
                                       points ,(*num-points*)
                                       profile? ,*profile?*))
             (set! workers (cons new-worker workers)))]
          [`(init
             rand ,vec
             flags ,flag-table
             num-iters ,iterations
             points ,points
             profile? ,profile?)
           (vector->pseudo-random-generator!
            (current-pseudo-random-generator)
            vec)
           (set! *profile?* profile?)
           (*flags* flag-table)
           (*num-iterations* iterations)
           (*num-points* points)]
          [`(do ,id ,test)
           (set! work (cons `(,id ,test) work))]
          [`(done ,id ,more ,result*)
           (place-channel-put ch (cons id result*))
           (when (not (null? work))
             (place-channel-put more `(apply ,more ,@(car work)))
             (set! work (cdr work)))]
          ['go
           (let sloop ([work* work] [workers workers])
             (if (or (null? work*) (null? workers))
                 (set! work work*)
                 (begin
                   (place-channel-put (car workers)
                                      `(apply ,(car workers) ,@(car work*)))
                   (sloop (cdr work*) (cdr workers)))))]
          ['kill
           (for ([p workers])
             (place-kill p))
           (abort)])
        (loop)))))

(define (get-test-results progs
                          #:threads [threads (max (- (processor-count) 1) 1)]
                          #:profile [profile? #f])
  (define m (make-manager))
  (define cnt 0)
  (define total (length progs))

  (define config
    `(init rand ,(pseudo-random-generator->vector
                  (current-pseudo-random-generator))
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           points ,(*num-points*)
           profile? ,profile?))

  (place-channel-put m config)

  (for ([i (range (min threads (length progs)))])
    (place-channel-put m 'make-worker))
  (for ([prog progs] [i (range (length progs))])
    (place-channel-put m `(do ,i ,prog)))
  (println "Starting " threads " workers on " (length progs) " problems...")
  (place-channel-put m 'go)

  (define (abort)
    (println "Terminating after " cnt (if (= cnt 1) " problem!" " problems!"))
    (place-channel-put m 'kill))

  (define outs
    (let loop ([progs progs] [out '()])
      (with-handlers
          ([exn:break? (λ (_) (abort) out)])
        (match progs
          ['() out]
          [(cons _ progs*)
           (let* ([msg (place-channel-get m)] [id (car msg)] [tr (cdr msg)])
             (set! cnt (+ 1 cnt))
             (printf "~a/~a\t" (~a cnt #:width 3 #:align 'right) total)
             (cond
              [(equal? (table-row-status tr) "crash")   (printf "[   CRASH ms ]")]
              [(equal? (table-row-status tr) "timeout") (printf "[    timeout ]")]
              [else (printf "[ ~ams]" (~a (table-row-time tr) #:width 8))])
             (printf "\t~a\n" (table-row-name tr))
             (loop progs* (cons (cons id tr) out)))]))))

  ; The use of > instead of < is a cleverness:
  ; the list of tests is accumulated in reverse, this reverses again.
  (map cdr (sort outs > #:key car)))
