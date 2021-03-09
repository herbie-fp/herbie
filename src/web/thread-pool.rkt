#lang racket

(require racket/place)
(require "../common.rkt" "../sandbox.rkt" "../plugin.rkt" "pages.rkt"
         "../syntax/read.rkt" "../datafile.rkt")

(provide get-test-results)

(define (graph-folder-path tname index)
  (define replaced (string-replace tname #px"\\W+" ""))
  (format "~a-~a" index (substring replaced 0 (min (string-length replaced) 50))))

(define (run-test index test #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (cond
   [dir
    (define dirname (graph-folder-path (test-name test) index))
    (define rdir  (build-path dir dirname))
    (when (not (directory-exists? rdir)) (make-directory rdir))

    (define result
      (call-with-output-files
       (list (build-path rdir "debug.txt") (and profile? (build-path rdir "profile.json")))
       (λ (dp pp) (get-test-result test #:seed seed #:profile pp #:debug debug? #:debug-port dp #:debug-level (cons #t #t)))))

    (set-seed! seed)
    (define error? #f)
    (for ([page (all-pages result)])
      (with-handlers ([exn:fail? (λ (e) ((page-error-handler result page) e) (set! error? #t))])
        (call-with-output-file (build-path rdir page)
          #:exists 'replace
          (λ (out) (make-page page out result profile?)))))

    (define out (get-table-data result dirname))
    (if error? (struct-copy table-row out [status "crash"]) out)]
   [else
    (define result (get-test-result test #:seed seed))
    (get-table-data result "")]))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name (parameterize ([params fresh] ...) body ...))))]))

(define (make-worker seed profile? debug? dir)
  (place/context* ch
    #:parameters (*flags* *num-iterations* *num-points* *timeout* *reeval-pts* *node-limit*
                  *max-find-range-depth* *pareto-mode*)
    (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
      (load-herbie-plugins))
    (for ([_ (in-naturals)])
      (match-define (list 'apply self id test) (place-channel-get ch))
      (define result (run-test id test #:seed seed #:profile profile? #:debug debug? #:dir dir))
      (place-channel-put ch `(done ,id ,self ,result)))))

(define (print-test-result i n data)
  (eprintf "~a/~a\t" (~a i #:width 3 #:align 'right) n)
  (match (table-row-status data)
    ["error"  
     (eprintf "[  ERROR  ]\t\t~a\n" (table-row-name data))]
    ["crash"  
     (eprintf "[  CRASH  ]\t\t~a\n" (table-row-name data))]
    ["timeout"
     (eprintf "[  TIMEOUT]\t\t~a\n" (table-row-name data))]
    [_
     (eprintf "[ ~as]   ~a→~a\t~a\n"
              (~r (/ (table-row-time data) 1000) #:min-width 7 #:precision '(= 3))
              (~r (table-row-start data) #:min-width 2 #:precision 0)
              (~r (table-row-result data) #:min-width 2 #:precision 0)
              (table-row-name data))]))

(define (run-workers progs threads #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (define workers
    (for/list ([wid (in-range threads)])
      (make-worker seed profile? debug? dir)))

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

        (print-test-result (length out*) (length progs) tr)

        (if (= (length out*) (length progs))
            out*
            (loop out*)))))

  ;; 9/29/20: warfa segfaults when killing worker threads. Can't recreate locally.
  ;; Cause unknown. Seems to disappear in a later branch. Weird stuff
  ;; TODO: Check on this later.
  (for-each place-kill workers)

  (map cdr (sort outs < #:key car)))

(define (run-nothreads progs #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (eprintf "Starting Herbie on ~a problems (seed: ~a)...\n" (length progs) seed)
  (define outs '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (eprintf "Terminating after ~a problem~a!\n"
                             (length outs) (if (= (length outs) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test #:seed seed #:profile profile? #:debug debug? #:dir dir))
      (print-test-result (+ 1 i) (length progs) tr)
      (set! outs (cons tr outs))))
  (reverse outs))

(define/contract (get-test-results progs #:threads threads #:seed seed #:profile profile? #:debug debug? #:dir dir)
  (-> (listof test?) #:threads (or/c #f natural-number/c)
      #:seed (or/c pseudo-random-generator-vector? (integer-in 0 (sub1 (expt 2 31))))
      #:profile boolean? #:debug boolean? #:dir (or/c #f path-string?)
      (listof (or/c #f table-row?)))

  (if threads
      (run-workers progs (min threads (length progs))
                   #:seed seed #:profile profile? #:debug debug? #:dir dir)
      (run-nothreads progs #:seed seed #:profile profile? #:debug debug? #:dir dir)))
