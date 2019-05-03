#lang racket

(require racket/place profile)
(require "../common.rkt" "../points.rkt" "../programs.rkt")
(require "../sandbox.rkt" "make-graph.rkt" "../formats/test.rkt" "../formats/datafile.rkt")

(provide get-test-results)

(define (graph-folder-path tname index)
  (format "~a-~a" index (string-prefix (string-replace tname #px"\\W+" "") 50)))

(define (run-test index test #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (cond
   [dir
    (define dirname
      (format "~a-~a" index (string-prefix (string-replace (test-name test) #px"\\W+" "") 50)))

    (define rdir  (build-path dir dirname))
    (when (not (directory-exists? rdir)) (make-directory rdir))

    (define result
      (call-with-output-files
       (list (build-path rdir "debug.txt") (and profile? (build-path rdir "profile.txt")))
       (λ (dp pp) (get-test-result test #:seed seed #:profile pp #:debug debug? #:debug-port dp #:debug-level (cons #t #t)))))

    (set-seed! seed)
    (for ([page (all-pages result)])
      (with-handlers ([exn:fail? (λ (e) ((error-display-handler) (format "Error when making ~a: ~a" page (exn-message e)) e))])
        (call-with-output-file (build-path rdir page)
          #:exists 'replace
          (λ (out) (make-page page out result #f)))))

    (get-table-data result dirname)]
   [else
    (define result (get-test-result test #:seed seed))
    (get-table-data result "")]))

(define (make-worker)
  (place ch
    (let loop ([seed #f] [profile? #f] [debug? #f] [dir #f])
      (match (place-channel-get ch)
	[`(init
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           points ,points
           profile? ,profile
           debug? ,debug
           dir ,path
           timeout ,timeout
           reeval ,reeval)

	 (set! seed vec)
         (set! profile? profile)
         (set! debug? debug)
         (set! dir path)
	 (*flags* flag-table)
	 (*num-iterations* iterations)
         (*num-points* points)
         (*timeout* timeout)
         (*reeval-pts* reeval)]
        [`(apply ,self ,id ,test)
         (let ([result (run-test id test #:seed seed #:profile profile? #:debug debug? #:dir dir)])
           (place-channel-put ch
             `(done ,id ,self ,result)))])
      (loop seed profile? debug? dir))))

(define (print-test-result data)
  (match (table-row-status data)
    ["error"  
     (eprintf "[   ERROR   ]\t~a\n" (table-row-name data))]
    ["crash"  
     (eprintf "[   CRASH   ]\t~a\n" (table-row-name data))]
    ["timeout"
     (eprintf "[  timeout  ]\t~a\n" (table-row-name data))]
    [_
     (eprintf "[ ~ams]\t(~a→~a)\t~a\n" (~a (table-row-time data) #:width 8)
              (~r (table-row-start data) #:min-width 2 #:precision 0)
              (~r (table-row-result data) #:min-width 2 #:precision 0)
              (table-row-name data))]))

(define (run-workers progs threads #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (define config
    `(init rand ,seed
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           points ,(*num-points*)
           profile? ,profile?
           debug? ,debug?
           dir ,dir
           timeout ,(*timeout*)
           reeval ,(*reeval-pts*)))

  (define workers
    (for/list ([wid (in-range threads)])
      (define worker (make-worker))
      (place-channel-put worker config)
      worker))

  (define work
    (for/list ([id (in-naturals)] [prog progs])
      (list id prog)))

  (eprintf "Starting ~a Herbie workers on ~a problems (seed: ~a)...\n" threads (length progs) seed)
  (for ([worker workers])
    (place-channel-put worker `(apply ,worker ,@(car work)))
    (set! work (cdr work)))

  (define outs
    (let loop ([out '()])
      (with-handlers ([exn:break?
                       (λ (_)
                         (eprintf "Terminating after ~a problem~a!\n"
                                  (length out) (if (= (length out) 1) ""  "s"))
                         out)])
        (match-define `(done ,id ,more ,tr) (apply sync workers))

        (when (not (null? work))
          (place-channel-put more `(apply ,more ,@(car work)))
          (set! work (cdr work)))

        (define out* (cons (cons id tr) out))

        (eprintf "~a/~a\t" (~a (length out*) #:width 3 #:align 'right) (length progs))
        (print-test-result tr)

        (if (= (length out*) (length progs))
            out*
            (loop out*)))))

  (map place-kill workers)

  outs)

(define (run-nothreads progs #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (eprintf "Starting Herbie on ~a problems (seed: ~a)...\n" (length progs) seed)
  (define out '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (eprintf "Terminating after ~a problem~a!\n"
                             (length out) (if (= (length out) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test #:seed seed #:profile profile? #:debug debug? #:dir dir))
      (eprintf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length progs))
      (print-test-result tr)
      (set! out (cons (cons i tr) out))))
  out)

(define/contract (get-test-results progs #:threads threads #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (-> (listof test?) #:threads (or/c #f natural-number/c)
      #:seed (or/c pseudo-random-generator-vector? (integer-in 0 (sub1 (expt 2 31))))
      #:profile boolean? #:debug boolean? #:dir (or/c #f path-string?)
      (listof (or/c #f table-row?)))
  (when (and threads (> threads (length progs)))
    (set! threads (length progs)))

  (define outs
    (if threads
        (run-workers progs threads #:seed seed #:profile profile? #:debug debug? #:dir dir)
        (run-nothreads progs #:seed seed #:profile profile? #:debug debug? #:dir dir)))
  
  (define out (make-vector (length progs) #f))
  (for ([(idx result) (in-dict outs)])
    (vector-set! out idx result))

  (vector->list out))
