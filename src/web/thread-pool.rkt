#lang racket

(require racket/place)
(require "../common.rkt" "../sandbox.rkt" "../load-plugin.rkt" "pages.rkt"
         "../syntax/read.rkt" "../syntax/types.rkt" "../datafile.rkt")

(provide get-test-results)

(define (graph-folder-path tname index)
  (define replaced (string-replace tname #px"\\W+" ""))
  (format "~a-~a" index (substring replaced 0 (min (string-length replaced) 50))))

(define (run-test index test #:seed seed #:profile profile? #:dir dir)
  (cond
   [dir
    (define dirname (graph-folder-path (test-name test) index))
    (define rdir  (build-path dir dirname))
    (when (not (directory-exists? rdir)) (make-directory rdir))

    (define result
      (cond
       [profile?
        (call-with-output-file
          (build-path rdir "profile.json") #:exists 'replace
          (λ (pp) (run-herbie 'improve test #:seed seed #:profile? pp)))]
       [else
        (run-herbie test 'improve #:seed seed)]))

    (set-seed! seed)
    (define error? #f)
    (for ([page (all-pages result)])
      (call-with-output-file (build-path rdir page)
        #:exists 'replace
        (λ (out)
          (with-handlers ([exn:fail? (λ (e) ((page-error-handler result page out) e) (set! error? #t))])
            (make-page page out result #t profile?)))))

    (define out (get-table-data result dirname))
    (if error? (struct-copy table-row out [status "crash"]) out)]
   [else
    (define result (run-herbie 'improve test #:seed seed))
    (get-table-data result "")]))

(define-syntax (place/context* stx)
  (syntax-case stx ()
    [(_ name #:parameters (params ...) body ...)
     (with-syntax ([(fresh ...) (generate-temporaries #'(params ...))])
       #'(let ([fresh (params)] ...)
           (place/context name (parameterize ([params fresh] ...) body ...))))]))

(define (make-worker seed profile? dir)
  (place/context* ch
    #:parameters (*flags* *num-iterations* *num-points* *timeout* *reeval-pts* *node-limit*
                  *max-find-range-depth* *pareto-mode* *platform-name* *loose-plugins*)
    (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
      (load-herbie-plugins))
    (for ([_ (in-naturals)])
      (match-define (list 'apply self id test) (place-channel-get ch))
      (define result (run-test id test #:seed seed #:profile profile? #:dir dir))
      (place-channel-put ch `(done ,id ,self ,result)))))

(define (print-test-result i n data)
  (eprintf "~a/~a\t" (~a i #:width 3 #:align 'right) n)
  (define bits (representation-total-bits (get-representation (table-row-precision data))))
  (match (table-row-status data)
    ["error"  
     (eprintf "[ ERROR ]\t\t~a\n" (table-row-name data))]
    ["crash"  
     (eprintf "[ CRASH ]\t\t~a\n" (table-row-name data))]
    ["timeout"
     (eprintf "[TIMEOUT]\t\t~a\n" (table-row-name data))]
    [_
     (eprintf "[~as]  ~a% → ~a%\t~a\n"
              (~r (/ (table-row-time data) 1000) #:min-width 6 #:precision '(= 1))
              (~r (* 100 (- 1 (/ (table-row-start data) bits))) #:min-width 3 #:precision 0)
              (~r (* 100 (- 1 (/ (table-row-result data) bits))) #:min-width 3 #:precision 0)
              (table-row-name data))]))

(define (run-workers progs threads #:seed seed #:profile profile? #:dir dir)
  (define workers
    (for/list ([wid (in-range threads)])
      (make-worker seed profile? dir)))
  (define workers-dead
    (for/list ([worker workers])
      (place-dead-evt worker)))

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
        (match (apply sync (append workers workers-dead))
          [`(done ,id ,more ,tr)
           (when (not (null? work))
             (place-channel-put more `(apply ,more ,@(car work)))
             (set! work (cdr work)))
           (define out* (cons (cons id tr) out))
           (print-test-result (length out*) (length progs) tr)
           (if (= (length out*) (length progs))
               out*
               (loop out*))]
          [(? evt?) ; In this case it is a place-dead-event
           (error "Thread crashed. Unrecoverable. Terminating immediately.")]))))
  (for-each place-kill workers)
  (map cdr (sort outs < #:key car)))

(define (run-nothreads progs #:seed seed #:profile profile? #:dir dir)
  (eprintf "Starting Herbie on ~a problems (seed: ~a)...\n" (length progs) seed)
  (define outs '())
  (with-handlers ([exn:break?
                   (λ (_)
                     (eprintf "Terminating after ~a problem~a!\n"
                             (length outs) (if (= (length outs) 1) "s" "")))])
    (for ([test progs] [i (in-naturals)])
      (define tr (run-test i test #:seed seed #:profile profile? #:dir dir))
      (print-test-result (+ 1 i) (length progs) tr)
      (set! outs (cons tr outs))))
  (reverse outs))

(define/contract (get-test-results progs #:threads threads #:seed seed #:profile profile? #:dir dir)
  (-> (listof test?) #:threads (or/c #f natural-number/c)
      #:seed (or/c pseudo-random-generator-vector? (integer-in 0 (sub1 (expt 2 31))))
      #:profile boolean? #:dir (or/c #f path-string?)
      (listof (or/c #f table-row?)))

  (if threads
      (run-workers progs (min threads (length progs)) #:seed seed #:profile profile? #:dir dir)
      (run-nothreads progs #:seed seed #:profile profile? #:dir dir)))
