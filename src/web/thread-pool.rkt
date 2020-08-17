#lang racket

(require racket/place (only-in ffi/unsafe cpointer? cpointer-tag))
(require "../common.rkt" "../sandbox.rkt" "../plugin.rkt" "pages.rkt"
         "../syntax/read.rkt" "../datafile.rkt" "../interface.rkt")

(provide get-test-results)

;; Place messages do not allow C-pointers, but documentation says they're supported (?).
;; Assume the C-pointers are values in a representation and convert them to a 
;; prefab struct
(struct place-cpointer (value repr) #:prefab)

(define (expr->place-expr expr) 
  (match expr
    [(list subexprs ...) (map expr->place-expr subexprs)]
    [_
      (if (and (cpointer? expr) expr) ; #f is considered a C-pointer
          (let ([prec (first (cpointer-tag expr))]) ; assuming #<cpointer:prec>
            (place-cpointer (repr->real expr (get-representation prec))
                            prec))
          expr)]))

(define (place-expr->expr expr)
  (match expr
    [(list subexprs ...) (map place-expr->expr subexprs)]
    [(? place-cpointer?) 
     (real->repr (place-cpointer-value expr) 
                 (get-representation (place-cpointer-repr expr)))]
    [_ expr]))

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
  (place/context* ch #:parameters (*flags* *num-iterations* *num-points* *timeout* *reeval-pts*)
    (parameterize ([current-error-port (open-output-nowhere)]) ; hide output
      (load-herbie-plugins))
    (for ([_ (in-naturals)])
      (match-define (list 'apply self id prog) (place-channel-get ch))
      (define test*
        (struct-copy test prog
          [input     (place-expr->expr (test-input prog))]
          [output    (place-expr->expr (test-output prog))]
          [pre       (place-expr->expr (test-pre prog))]
          [spec      (place-expr->expr (test-spec prog))]))
      (define result (run-test id test* #:seed seed #:profile profile? #:debug debug? #:dir dir))
      (define result*
        (struct-copy table-row result
          [input     (expr->place-expr (table-row-input result))]
          [output    (expr->place-expr (table-row-output result))]
          [pre       (expr->place-expr (table-row-pre result))]
          [spec      (expr->place-expr (table-row-spec result))]
          [target    (expr->place-expr (table-row-target result))]))
      (place-channel-put ch `(done ,id ,self ,result*)))))

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
      (list 
        id
        (struct-copy test prog
          [input     (expr->place-expr (test-input prog))]
          [output    (expr->place-expr (test-output prog))]
          [pre       (expr->place-expr (test-pre prog))]
          [spec      (expr->place-expr (test-spec prog))]))))

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

        (define tr*
          (struct-copy table-row tr
          [input     (place-expr->expr (table-row-input tr))]
          [output    (place-expr->expr (table-row-output tr))]
          [pre       (place-expr->expr (table-row-pre tr))]
          [spec      (place-expr->expr (table-row-spec tr))]
          [target    (place-expr->expr (table-row-target tr))]))

        (define out* (cons (cons id tr*) out))
        (print-test-result (length out*) (length progs) tr*)
        (if (= (length out*) (length progs))
            out*
            (loop out*)))))

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
