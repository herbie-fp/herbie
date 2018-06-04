#lang racket

(require racket/place)
(require "../common.rkt")
(require "../formats/test.rkt")
(require "../points.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../sandbox.rkt")
(require "make-graph.rkt")
(require "../formats/datafile.rkt")

(provide get-test-results)

(define (make-graph-if-valid result tname index rdir #:profile profile? #:seed seed)
  (with-handlers ([(const #f) (λ _ #f)])
    (when (not (directory-exists? rdir))
      (make-directory rdir))

    (set-seed! seed)
    (write-file (build-path rdir "graph.html")
                ((cond [(test-result? result)
                        (λ args (apply make-graph args)
                                (apply make-plots args)
                                (apply make-interactive-js args))]
                       [(test-timeout? result) make-timeout]
                       [(test-failure? result) make-traceback])
                 result rdir profile?))))

(define (graph-folder-path tname index)
  (let* ([stripped-tname (string-replace tname #px"\\W+" "")]
         [index-label (number->string index)])
    (string-append index-label "-"
                   (if (> (string-length stripped-tname) 50)
                       (substring stripped-tname 0 50)
                       stripped-tname))))

(define (call-with-output-files names k)
  (let loop ([names names] [ps '()])
    (if (null? names)
        (apply k (reverse ps))
        (if (car names)
            (call-with-output-file
                (car names) #:exists 'replace
                (λ (p) (loop (cdr names) (cons p ps))))
            (loop (cdr names) (cons #f ps))))))

(define (run-test index test #:seed seed #:profile profile? #:dir dir)
  (cond
   [dir
    (let* ([rdir (graph-folder-path (test-name test) index)]
           [rdir* (build-path dir rdir)])
      (when (not (directory-exists? rdir*))
        (make-directory rdir*))

      (define result
        (call-with-output-files
         (list (build-path rdir* "debug.txt") (and profile? (build-path rdir* "profile.txt")))
         (λ (dp pp) (get-test-result test #:seed seed #:profile pp #:debug dp #:setup! (λ () (set-debug-level! #t #t))))))

      (make-graph-if-valid result (test-name test) index rdir* #:profile profile? #:seed seed)
      (get-table-data result rdir))]
   [else
    (define result (get-test-result test #:seed seed))
    (get-table-data result "")]))

(define (make-worker)
  (place ch
    (let loop ([seed #f] [profile? #f] [dir #f])
      (match (place-channel-get ch)
	[`(init
	   rand ,vec
	   flags ,flag-table
	   num-iters ,iterations
           points ,points
           profile? ,profile
           dir ,path
           timeout ,timeout
           reeval ,reeval)

	 (set! seed vec)
         (set! profile? profile)
         (set! dir path)
	 (*flags* flag-table)
	 (*num-iterations* iterations)
         (*num-points* points)
         (*timeout* timeout)
         (*reeval-pts* reeval)]
        [`(apply ,self ,id ,test)
         (let ([result (run-test id test #:seed seed #:profile profile? #:dir dir)])
           (place-channel-put ch
             `(done ,id ,self ,result)))])
      (loop seed profile? dir))))

(define (print-test-result data)
  (match-define (cons fpcore tr) data)
  (match (table-row-status tr)
    ["error"  
     (eprintf "[   ERROR   ]\t~a\n" (table-row-name tr))]
    ["crash"  
     (eprintf "[   CRASH   ]\t~a\n" (table-row-name tr))]
    ["timeout"
     (eprintf "[  timeout  ]\t~a\n" (table-row-name tr))]
    [_
     (eprintf "[ ~ams]\t(~a→~a)\t~a\n" (~a (table-row-time tr) #:width 8)
              (~r (table-row-start tr) #:min-width 2 #:precision 0)
              (~r (table-row-result tr) #:min-width 2 #:precision 0)
              (table-row-name tr))]))

(define (run-workers progs threads #:seed seed #:profile profile? #:dir dir)
  (define config
    `(init rand ,seed
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           points ,(*num-points*)
           profile? ,profile?
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

  (eprintf "Starting ~a Herbie workers on ~a problems...\n" threads (length progs))
  (eprintf "Seed: ~a\n" seed)
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

(define (run-nothreads progs #:seed seed #:profile profile? #:dir dir)
  (eprintf "Starting Herbie on ~a problems...\n" (length progs))
  (eprintf "Seed: ~a\n" seed)
  (define out '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (eprintf "Terminating after ~a problem~a!\n"
                             (length out) (if (= (length out) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test #:seed seed #:profile profile? #:dir dir))
      (eprintf "~a/~a\t" (~a (+ 1 i) #:width 3 #:align 'right) (length progs))
      (print-test-result tr)
      (set! out (cons (cons i tr) out))))
  out)

(define/contract (get-test-results progs #:threads threads #:seed seed #:profile profile? #:dir dir)
  (-> (listof test?) #:threads (or/c #f natural-number/c)
      #:seed pseudo-random-generator-vector? #:profile boolean? #:dir (or/c #f path-string?)
      (listof (or/c #f (cons/c expr? table-row?))))
  (when (and threads (> threads (length progs)))
    (set! threads (length progs)))

  (define outs
    (if threads
        (run-workers progs threads #:seed seed #:profile profile? #:dir dir)
        (run-nothreads progs #:seed seed #:profile profile? #:dir dir)))
  
  (define out (make-vector (length progs) #f))
  (for ([(idx result) (in-dict outs)])
    (vector-set! out idx result))

  (vector->list out))
