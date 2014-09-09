#lang racket

(require profile)
(require racket/place)
(require racket/engine)
(require math/bigfloat)
(require casio/common)
(require casio/programs)
(require casio/test)
(require casio/alternative)
(require casio/main)
(require reports/make-graph)

(provide (struct-out table-row) get-test-results)

(define *reeval-pts* 8000)
(define *seed* #f)
(define *dir* #f)
(define *timeout* (* 1000 60 5))
(define *profile?* #f)

(struct test-result
  (test rdir time bits
   start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error))
(struct test-failure (test bits exn time rdir))
(struct test-timeout (test bits rdir) #:prefab)

(define (get-test-result test iters rdir)
  (current-pseudo-random-generator (vector->pseudo-random-generator *seed*))
  (define (file name) (build-path *dir* rdir name))

  (define (run-casio _)
    (if *profile?*
        (parameterize ([current-output-port (open-output-file (file "profile.txt") #:exists 'replace)])
          (let ([res #f])
            (profile (set! res (compute-result))) ; Racket 5.3 workaround
            res))
        (compute-result)))

  (define (compute-result)
    (with-handlers ([(const #t) (λ (e) (list 'error e (bf-precision)))])
      (setup (test-program test) (test-samplers test)
             (λ (alt)
                  (parameterize ([*debug-port* (open-output-file (file "debug.txt") #:exists 'replace)])
                    (list 'good alt (improve alt (*num-iterations*)) (*points*) (*exacts*)))))))

  (let* ([start-time (current-inexact-milliseconds)] [eng (engine run-casio)])
    (engine-run *timeout* eng)

    (match (engine-result eng)
      [`(good ,start ,end ,points ,exacts)
       (define-values (newpoints newexacts)
         (parameterize ([*num-points* *reeval-pts*])
           (prepare-points (alt-program start) (test-samplers test))))
       (test-result test rdir
                    (- (current-inexact-milliseconds) start-time)
                    (bf-precision)
                    start end points exacts
                    (errors (alt-program start) points exacts)
                    (errors (alt-program end) points exacts)
                    newpoints newexacts
                    (errors (alt-program start) newpoints newexacts)
                    (errors (alt-program end) newpoints newexacts)
                    (if (test-output test)
                        (errors `(λ ,(test-vars test) ,(test-output test))
                                newpoints newexacts)
                        #f))]
      [`(error ,e ,bits)
       (test-failure test bits e (- (current-inexact-milliseconds) start-time) rdir)]
      [#f
       (test-timeout test (bf-precision) rdir)])))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/|'|\"" "")]
         [index-label (number->string index)])
    (string-append index-label stripped-tname)))

;; Returns #t if the graph was sucessfully made, #f is we had a crash during
;; the graph making process, or the test itself crashed.
(define (make-graph-if-valid result tname index rdir)
  (let* ([dir (build-path *dir* rdir)])
    (with-handlers ([(const #f) (λ _ #f)])
      (when (not (directory-exists? dir))
        (make-directory dir))

      (match result
       [(test-result test rdir time bits
                     start-alt end-alt points exacts
                     start-est-error end-est-error
                     newpoints newexacts start-error end-error
                     target-error)

        (write-file (build-path dir "graph.html")
          (make-graph test end-alt newpoints start-error end-error target-error bits dir *profile?*))
        (build-path rdir "graph.html")]

       [(test-timeout test bits rdir)
        (write-file (build-path dir "graph.html")
          (make-timeout test bits *profile?*))
        (build-path rdir "graph.html")]

       [(test-failure test bits exn time rdir)
        (write-file (build-path dir "graph.html")
          (make-traceback test exn bits *profile?*))
        (build-path rdir "graph.html")]))))

(struct table-row
  (name status start result target inf- inf+ result-est vars input output time bits link) #:prefab)

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
                    [(good-inf bad-inf) (partition positive? infs)])
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

(define (run-casio index test iters)
  (let* ([rdir (graph-folder-path (test-name test) index)]
         [rdir* (build-path *dir* rdir)])
    (when (not (directory-exists? rdir*))
      (make-directory rdir*))
    (let ([result (get-test-result test iters rdir)])
      (make-graph-if-valid result (test-name test) index rdir)
      (get-table-data result))))

(define (make-worker)
  (place ch
    (let loop ()
      (match (place-channel-get ch)
	[`(init
	   dir ,dir
	   wid ,worker-id
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           profile? ,profile?)

	 (when (not (directory-exists? dir))
           (make-directory dir))

         (set! *dir* dir)
	 (set! *seed* vec)
         (set! *profile?* profile?)
	 (*flags* flag-table)
	 (*num-iterations* iterations)]
        [`(,self ,id ,test ,iters)
         (let ([result (run-casio id test iters)])
           (place-channel-put ch
             `(done ,id ,self ,result)))])
      (loop))))

(define (make-manager)
  (place ch
    (define dir #f)
    (define workers '())
    (define work '())
    (define next-wid 0)
    (let loop ()
      ; Message handler
      (match (apply sync ch workers)
        ['make-worker
	 (let ([new-worker (make-worker)])
	   (place-channel-put new-worker
			      `(init dir ,dir
				     wid ,(begin0 next-wid
						(set! next-wid (add1 next-wid)))
				     rand ,(pseudo-random-generator->vector
					    (current-pseudo-random-generator))
				     flags ,(*flags*)
				     num-iters ,(*num-iterations*)
                                     profile? ,*profile?*))
	   (set! workers (cons new-worker workers)))]
	[`(init
	   dir ,dir*
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           profile? ,profile?)
	 (set! dir dir*)
	 (vector->pseudo-random-generator!
	  (current-pseudo-random-generator)
	  vec)
         (set! *profile?* profile?)
	 (*flags* flag-table)
	 (*num-iterations* iterations)]
        [`(do ,id ,test ,iters)
         (set! work (cons `(,id ,test ,iters) work))]
        [`(done ,id ,more ,result*)
         (place-channel-put ch (cons id result*))
         (when (not (null? work))
           (place-channel-put more (cons more (car work)))
           (set! work (cdr work)))]
        ['go
         (let sloop ([work* work] [workers workers])
           (if (or (null? work*) (null? workers))
               (set! work work*)
               (begin
                 (place-channel-put (car workers)
                                (cons (car workers) (car work*)))
                 (sloop (cdr work*) (cdr workers)))))])
      (loop))))

(define (get-test-results progs iters
                          #:threads [threads (max (- (processor-count) 1) 1)]
                          #:dir dir #:profile [profile? #f])
  (define m (make-manager))
  (define cnt 0)
  (define total (length progs))

  (define config
    `(init dir ,dir
           rand ,(pseudo-random-generator->vector
                  (current-pseudo-random-generator))
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           profile? ,profile?))

  (place-channel-put m config)

  (for ([i (range threads)])
    (place-channel-put m 'make-worker))
  (for ([prog progs] [i (range (length progs))])
    (place-channel-put m `(do ,i ,prog ,iters)))
  (place-channel-put m 'go)
  (define outs
    (for/list ([_ progs])
      (let* ([msg (place-channel-get m)] [id (car msg)] [tr (cdr msg)])
        (set! cnt (+ 1 cnt))
        (cond
         [(equal? (table-row-status tr) "crash")
          (println cnt "/" total "\t[ " (~a (table-row-time tr) #:width 8)"ms ]\t"
                   (table-row-name tr) " [CRASH]")]
         [(equal? (table-row-status tr) "timeout")
          (println cnt "/" total "\t[    timeout ]\t" (table-row-name tr))]
         [else
          (println cnt "/" total "\t[ " (~a (table-row-time tr) #:width 8)"ms ]\t"
           (table-row-name tr))])
        (cons id tr))))
  ; The use of > instead of < is a cleverness:
  ; the list of tests is accumulated in reverse, this reverses again.
  (map cdr (sort outs > #:key car)))
