#lang racket
(require openssl/md5)
(require xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require web-server/configuration/responders)
(require "../sandbox.rkt")
(require "../formats/datafile.rkt" "../reports/make-graph.rkt" "../reports/make-report.rkt" "../reports/thread-pool.rkt")
(require "../formats/tex.rkt")
(require "../common.rkt" "../config.rkt" "../programs.rkt" "../formats/test.rkt")
(require "../web/common.rkt")

(define/page (demo)
  (when (not (directory-exists? demo-output-path))
    (make-directory demo-output-path))

  (response/xexpr
   #:headers (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
   (herbie-page
    #:title "Herbie web demo"
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "/demo.js")
    `(p "Enter a formula below, hit " (kbd "Enter") ", and Herbie will try to improve it.")
    `(form ([action ,(embed/url improve)] [method "post"] [id "formula"]
            [data-progress ,(embed/url improve-start)])
           (input ([name "formula"] [autofocus "true"] [placeholder "(lambda (x) (- (sqrt (+ x 1)) (sqrt x)))"]))
           (ul ([id "errors"]))
           (pre ([id "progress"] [style "display: none;"])))

    `(p "To handle the high volume of requests, web requests are queued; "
        "there are " (span ([id "num-jobs"]) ,(~a (hash-count *jobs*))) " jobs in the queue right now. "
        "Web demo requests may also time out and cap the number of improvement iterations. "
        "To avoid these limitations, " (a ([href "/installing.html"]) "install Herbie")
        " on your own computer.")

    `(p ([id "lisp-instructions"])
        "Please enter formulas as Scheme expressions with a top-level " (code "lambda") " form, "
        "using only the following supported functions:")
    `(p ([id "mathjs-instructions"] [style "display: none;"])
        "You can write ordinary mathematical expressions (parsed with "
        (a ([href "https://mathjs.org"]) "math.js") ") using only the following supported functions:")
    `(dl ([class "function-list"])
      (dt ,@(list-join (for/list ([i '(+ - * / abs)]) `(code ,(~a i))) '(", ")))
      (dd "The usual arithmetic functions")
      (dt ,@(list-join (for/list ([i '(sqrt sqr)]) `(code ,(~a i))) '(", ")))
      (dd "Squares and square roots")
      (dt ,@(list-join (for/list ([i '(exp log)]) `(code ,(~a i))) '(", ")))
      (dd "Natural exponent and natural log")
      (dt ,@(list-join (for/list ([i '(expt)]) `(code ,(~a i))) '(", ")))
      (dd "Raising a value to a power (also called " (code "pow") ")")
      (dt ,@(list-join (for/list ([i '(sin cos tan cot)]) `(code ,(~a i))) '(", ")))
      (dd "The trigonometric functions")
      (dt ,@(list-join (for/list ([i '(asin acos atan)]) `(code ,(~a i))) '(", ")))
      (dd "The inverse trigonometric functions")
      (dt ,@(list-join (for/list ([i '(sinh cosh tanh)]) `(code ,(~a i))) '(", ")))
      (dd "The hyperbolic trigonometric functions"))
    `(p "You can also use the constants " (code "PI") " and " (code "E") ".")

    `(p (em "Note") ": all formulas submitted to the Herbie web demo are logged "
        "and made publicly accessible. See what formulas other users submitted "
        (a ([href "./report.html"]) "here") ".")
    )))

(define *jobs* (make-hash))

(define *fixed-seed* #(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636))

(define *worker-thread*
  (thread
   (λ ()
     (let loop ()
       (match-define (list name hash formula sema) (thread-receive))
       (define dir (build-path demo-output-path hash))

       (match (directory-exists? dir)
         [#t (semaphore-post sema)]
         [#f
          (make-directory dir)
          (printf "Job ~a started for ~a\n" hash name)
          (match-define (list (or 'λ 'lambda) vars body) formula)

          (define result
            (parameterize ([*timeout* (* 1000 60)] [*reeval-pts* 1000])
              (call-with-output-file
                  (build-path dir "debug.txt") #:exists 'replace
                  (λ (p)
                    (get-test-result
                     #:seed *fixed-seed*
                     #:setup! (λ () (set-debug-level! 'progress '(3 4)))
                     #:debug p
                     (test name vars (map (const 'default) vars) body #f #f))))))

          (define make-page
            (cond [(test-result? result) make-graph]
                  [(test-timeout? result) make-timeout]
                  [(test-failure? result) make-traceback]))
          (with-output-to-file (build-path dir "graph.html")
            (λ () (make-page result dir #f)))

          (define data (get-table-data result dir))

          ; Save new report data
          (define info
            (if (file-exists? (build-path demo-output-path "results.json"))
                (let ([info (read-datafile (build-path demo-output-path "results.json"))])
                  (set-report-info-tests! info (cons data (report-info-tests info)))
                  info)
                (make-report-info (list data) #:note "Web demo results")))
          (write-datafile (build-path demo-output-path "results.json") info)
          (make-report-page (build-path demo-output-path "report.html") info)

          (printf "Job ~a complete for ~a\n" hash name)
          (hash-remove! *jobs* hash)
          (semaphore-post sema)])
       (loop)))))

(define (run-improve name hash formula)
  (hash-set! *jobs* hash #t)
  (define sema (make-semaphore))
  (thread-send *worker-thread* (list name hash formula sema))
  sema)

(define (already-computed? name hash formula)
  (and (not (hash-has-key? *jobs* hash))
       (directory-exists? (build-path demo-output-path hash))))

(define (improve-common body go-back)
  (match (get-bindings 'formula)
    [(list formula-str)
     (define formula
       (with-handlers ([exn:fail? (λ (e) #f)])
         (read (open-input-string formula-str))))
     (cond
      [(valid-program? formula)
       (define hash (md5 (open-input-string formula-str)))
       (define name
         (match (get-bindings 'formula-math)
           [(list formula-math) formula-math]
           [_ formula-str]))

       (body name hash formula)]
      [else
       (response/error "Demo Error"
                       `(p "Invalid formula " (code ,formula-str) ". "
                           "Formula must be a valid list function using only the supported functions. "
                           "Please " (a ([href ,go-back]) "go back") " and try again."))])]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified serveral). "
                         "Please " (a ([href ,go-back]) "go back") " and try again."))]))

(define/page (improve-start)
  (improve-common
   (λ (name hash formula)
     (unless (already-computed? name hash formula)
       (run-improve name hash formula))
     (response/full 201 #"Job started" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (embed/url (curryr check-status hash))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '(#"")))
   (embed/url demo)))

(define/page (check-status hash)
  (if (hash-has-key? *jobs* hash)
      (response 202 #"Job in progress" (current-seconds) #"text/plain"
                (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                (λ (out)
                  (when (file-exists? (build-path demo-output-path hash "debug.txt"))
                    (call-with-input-file (build-path demo-output-path hash "debug.txt")
                      (λ (in) (copy-port in out))))))
     (response/full 201 #"Job complete" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (format "/demo/~a/graph.html" hash)))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '())))

(define/page (improve)
  (improve-common
   (λ (name hash formula)
     (unless (already-computed? name hash formula)
       (semaphore-wait (run-improve name hash formula)))

     (redirect-to (format "/demo/~a/graph.html" hash) see-other))
   (embed/url demo)))

(define (response/error title body)
  (response/full 400 #"Bad Request" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (list (string->bytes/utf-8 (xexpr->string (herbie-page #:title title body))))))

(define (go [command-line #f])
  (eprintf "Server loaded and starting up.\n")
  (serve/servlet
   demo
   #:log-file (build-path demo-output-path "../../demo.log")
   #:file-not-found-responder
   (gen-file-not-found-responder
    (build-path demo-output-path "../404.html"))
   #:listen-ip #f
   #:command-line? command-line
   #:banner? (not command-line)
   #:servlets-root (build-path demo-output-path "../..")
   #:server-root-path (build-path demo-output-path "..")
   #:servlet-path "/demo/"
   #:extra-files-paths (list (build-path demo-output-path ".."))))

(module+ main
  (go #t))
