#lang racket

(require racket/place)
(require racket/engine)
(require math/bigfloat)
(require (prefix-in srfi: srfi/48)) ;; avoid shadowing racket's format
(require casio/common)
(require casio/points)
(require casio/alternative)
(require casio/test)
(require casio/main)
(require casio/matcher)
(require reports/make-graph)

(provide (struct-out test-result) (struct-out test-failure)
         (struct-out test-timeout) get-test-results)

(define *reeval-pts* 8000)
(define *seed* #f)
(define *timeout* (* 1000 60 20))

(struct test-result
  (test rdir time bits
   start-alt end-alt points exacts start-est-error end-est-error
   newpoints newexacts start-error end-error target-error))
(struct test-failure (test exn time rdir))
(struct test-timeout (test) #:prefab)

(define (srcloc->string sl)
  (if sl
      (string-append
       (path->string (srcloc-source sl))
       ":"
       (number->string (srcloc-line sl))
       ":"
       (number->string (srcloc-column sl)))
      "???"))

(define (flatten-exn e)
  (list (exn-message e)
        (for/list ([tb (continuation-mark-set->context
                        (exn-continuation-marks e))])
          (list (car tb) (srcloc->string (cdr tb))))))

(define (get-test-result test iters rdir)
  (current-pseudo-random-generator (vector->pseudo-random-generator *seed*))

  (define (compute-result _)
    (let*-values ([(orig) (make-prog test)]
		  [(point-preparer) ((flag 'evaluate 'exponent-points)
				     prepare-points prepare-points-uniform)]
		  [(points exacts) (point-preparer orig)]
		  [(more-pts more-exs) (parameterize ([*num-points* 8192])
				       (point-preparer orig))])
      (parameterize ([*points* points] [*exacts* exacts]
		     [*more-points* more-pts] [*more-exacts* more-exs])
	(let* ([start-alt (make-alt orig)]
	       [end-alt (improve-alt start-alt (*num-iterations*))])
	  (list start-alt end-alt points exacts)))))

  (let* ([start-time (current-inexact-milliseconds)]
         [handle-crash
          (λ (exn)
             (test-failure test (flatten-exn exn) (- (current-inexact-milliseconds) start-time) rdir))]
         [eng (engine compute-result)])

    (with-handlers ([(const #t) handle-crash])
      (if (engine-run *timeout* eng)
          (match (engine-result eng)
            [`(,start ,end ,points ,exacts)
             (define-values (newpoints newexacts)
               (parameterize ([*num-points* *reeval-pts*])
                 (prepare-points (alt-program start))))
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
                              #f))])
          (test-timeout test)))))

(define (make-traceback err)
  (printf "<h2 id='error-message'>~a</h2>\n" (first err))
  (printf "<ol id='traceback'>\n")
  (for ([fn&loc (second err)])
    (printf "<li><code>~a</code> in <code>~a</code></li>\n"
            (first fn&loc) (second fn&loc)))
  (printf "</ol>\n"))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\(| |\\)|/|'|\"" "")]
         [index-label (number->string index)])
    (string-append index-label stripped-tname)))

;; Returns #t if the graph was sucessfully made, #f is we had a crash during
;; the graph making process, or the test itself crashed.
(define (make-graph-if-valid result tname index rdir)
  (let* ([dir (build-path "graphs" rdir)]) ; TODO : Hard-coded folder name
    (with-handlers ([(const #f) (λ _ #f)])
      (cond
       [(test-result? result)
        (when (not (directory-exists? dir))
          (make-directory dir))

        (make-graph (test-result-test result)
                    (test-result-end-alt result)
                    (test-result-newpoints result)
                    (test-result-start-error result)
                    (test-result-end-error result)
                    (test-result-target-error result)
		    (test-result-bits result)
                    dir)

        (build-path rdir "graph.html")]
       [(test-timeout? result)
        #f]
       [(test-failure? result)
        (when (not (directory-exists? dir))
          (make-directory dir))

        (write-file (build-path dir "graph.html")
          (printf "<html>\n")
          (printf "<head><meta charset='utf-8' /><title>Exception for ~a</title></head>\n"
                  (test-name (test-failure-test result)))
          (printf "<body>\n")
          (make-traceback (test-failure-exn result))
          (printf "</body>\n")
          (printf "</html>\n"))

        (build-path rdir "graph.html")]
       [else #f]))))

(define (run-casio index test iters)
  (let* ([rdir (graph-folder-path (test-name test) index)]
         [result (get-test-result test iters rdir)])
    (make-graph-if-valid result (test-name test) index rdir)
    result))

(define (marshal-test-result tr)
  `(test-result ,(test-result-test tr)
                ,(test-result-rdir tr)
                ,(test-result-time tr)
		,(test-result-bits tr)
                ,(marshal-alt (test-result-start-alt tr))
                ,(marshal-alt (test-result-end-alt tr))
                ,(test-result-points tr)
                ,(test-result-exacts tr)
                ,(test-result-start-est-error tr)
                ,(test-result-end-est-error tr)
                ,(test-result-newpoints tr)
                ,(test-result-newexacts tr)
                ,(test-result-start-error tr)
                ,(test-result-end-error tr)
                ,(test-result-target-error tr)))

(define (unmarshal-test-result tr*)
  (match tr*
    [`(test-result ,t ,rdir ,time ,bits ,start* ,end* ,pts ,exs ,estartE ,eendE ,pts* ,exs* ,startE ,endE ,targetE)
     (test-result t rdir time bits (unmarshal-alt start*) (unmarshal-alt end*) pts exs
                  estartE eendE pts* exs* startE endE targetE)]))

(define (marshal-test-failure tf)
  `(test-failure ,(test-failure-test tf)
                 ,(test-failure-exn tf)
                 ,(test-failure-time tf)
                 ,(test-failure-rdir tf)))

(define (unmarshal-test-failure tf*)
  (match tf*
    [`(test-failure ,test ,exn ,time ,rdir)
     (test-failure test exn time rdir)]))

(define (marshal-test-* t)
  (cond
   [(test-result? t) (marshal-test-result t)]
   [(test-failure? t) (marshal-test-failure t)]
   [(test-timeout? t) t]))

(define (unmarshal-test-* t*)
  (cond
   [(test-timeout? t*) t*]
   [(eq? (car t*) 'test-result) (unmarshal-test-result t*)]
   [(eq? (car t*) 'test-failure) (unmarshal-test-failure t*)]))

(define (marshal-alt a)
  (if a
      `(alt ,(alt-program a) ,(marshal-change (alt-change a)) ,(marshal-alt (alt-prev a))
            ,(alt-cycles a))
      #f))

(define (unmarshal-alt a*)
  (match a*
    [`(alt ,prog ,cng ,prev ,cyc)
     (alt prog (unmarshal-change cng) (unmarshal-alt prev) cyc)]
    [#f #f]))

(define (marshal-change c)
  (cond
   [(change*? c)
    `(change ,(marshal-rule (change-rule c)) ,(change-location c)
             ,(for/list ([b (change-bindings c)])
                ; Nasty, nasty hack to support regimes
                (if (eq? (rule-name (change-rule c)) 'regimes)
                    (if (eq? (car b) 'alt)
                        (cons (car b) (map marshal-alt (cdr b)))
                        b)
                    b))
             ,(change*-hardness c))]
   [(change? c)
    `(change ,(marshal-rule (change-rule c)) ,(change-location c)
             ,(for/list ([b (change-bindings c)])
                ; Nasty, nasty hack to support regimes
                (if (eq? (rule-name (change-rule c)) 'regimes)
		    (if (eq? (car b) 'alt)
			(cons (car b) (map marshal-alt (cdr b)))
			b)
		    b)))]
   [(not c) #f]
   [else (error "Unknown change type" c)]))

(define (unmarshal-change c*)
  (match c*
    [`(change ,rule* ,loc ,bind)
     (let ([rule (unmarshal-rule rule*)])
       (change rule loc
               (for/list ([b bind])
                 ; Nasty, nasty hack to support regimes
                 (if (eq? (rule-name rule) 'regimes)
                     (if (eq? (car b) 'alt)
                         (cons (car b) (map unmarshal-alt (cdr b)))
                         b)
                     b))))]
    [`(change ,rule* ,loc ,bind ,hard)
     (let ([rule (unmarshal-rule rule*)])
       (change* rule loc
                (for/list ([b bind])
                  ; Nasty, nasty hack to support regimes
                  (if (eq? (rule-name rule) 'regimes)
                      (if (eq? (car b) 'alt)
                          (cons (car b) (map unmarshal-alt (cdr b)))
                          b)
                      b))
                hard))]
    [#f #f]))

(define (marshal-rule r)
  `(rule ,(rule-name r) ,(rule-input r) ,(rule-output r)
         ,(rule-slocations r)))

(define (unmarshal-rule r*)
  (match r*
    [`(rule ,name ,inp ,out ,sloc)
     (rule name inp out sloc)]))

(define (make-worker)
  (place ch
    (let loop ()
      (match (place-channel-get ch)
	[`(init
	   log-dir ,log-directory
	   wid ,worker-id
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations)

	 (when (not (directory-exists? log-directory))
           (make-directory log-directory))
	 (let ([filename (format "~a/~a-~a.log" log-directory worker-id (current-seconds))])
           (*debug* (open-output-file filename #:exists 'replace)))

	 (set! *seed* vec)

	 (*flags* flag-table)
	 (*num-iterations* iterations)]
        [`(,self ,id ,test ,iters)
         (let ([result (run-casio id test iters)])
           (place-channel-put ch
             `(done ,id ,self ,(marshal-test-* result))))])
      (loop))))

(define (make-manager)
  (place ch
    (define log-dir #f)
    (define workers '())
    (define work '())
    (define next-wid 0)
    (let loop ()
      ; Message handler
      (match (apply sync ch workers)
        ['make-worker
	 (let ([new-worker (make-worker)])
	   (place-channel-put new-worker
			      `(init log-dir ,log-dir
				     wid ,(begin0 next-wid
						(set! next-wid (add1 next-wid)))
				     rand ,(pseudo-random-generator->vector
					    (current-pseudo-random-generator))
				     flags ,(*flags*)
				     num-iters ,(*num-iterations*)))
	   (set! workers (cons new-worker workers)))]
	[`(init
	   log-dir ,log-directory
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations)
	 (set! log-dir log-directory)
	 (vector->pseudo-random-generator!
	  (current-pseudo-random-generator)
	  vec)
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
                          #:log-dir log-dir)
  (define m (make-manager))
  (define cnt 0)
  (define total (length progs))

  (place-channel-put m `(init log-dir ,log-dir
			      rand ,(pseudo-random-generator->vector
				     (current-pseudo-random-generator))
			      flags ,(*flags*)
			      num-iters ,(*num-iterations*)))

  (for ([i (range threads)])
    (place-channel-put m 'make-worker))
  (for ([prog progs] [i (range (length progs))])
    (place-channel-put m `(do ,i ,prog ,iters)))
  (place-channel-put m 'go)
  (define outs
    (for/list ([_ progs])
      (let* ([msg (place-channel-get m)]
             [id (car msg)] [tr (unmarshal-test-* (cdr msg))])
        (set! cnt (+ 1 cnt))
        (cond
         [(test-result? tr)
          (println cnt "/" total "\t[ "
		   (srfi:format "~8,3F" (/ (test-result-time tr) 1000.0)) "s ]\t"
		   (test-name (test-result-test tr)))]
         [(test-failure? tr)
          (println cnt "/" total "\t[ "
		   (srfi:format "~8,3F" (/ (test-failure-time tr) 1000.0)) "s ]\t"
                   (test-name (test-failure-test tr)) " [CRASH]")]
         [(test-timeout? tr)
          (println cnt "/" total "\t[    timeout ]\t" (test-name (test-timeout-test tr)))]
         [else
          (error "Unknown test result type" tr)])
        (cons id tr))))
  ; The use of > instead of < is a cleverness:
  ; the list of tests is accumulated in reverse, this reverses again.
  (map cdr (sort outs > #:key car)))
