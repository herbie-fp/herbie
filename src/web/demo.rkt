#lang racket
(require openssl/md5 xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require web-server/configuration/responders)
(require "../sandbox.rkt")
(require "../formats/datafile.rkt" "../reports/make-graph.rkt" "../reports/make-report.rkt" "../reports/thread-pool.rkt")
(require "../formats/tex.rkt")
(require "../common.rkt" "../config.rkt" "../programs.rkt" "../formats/test.rkt" "../errors.rkt")
(require "../web/common.rkt")

(define *demo* (make-parameter #f))

(define (function-list . fn-classes)
  (define (fn->html fn)
    `(code ,(~a fn)))

  (define (fn-class class)
    (match-define (list fns description ...) class)
    `((dt () ,@(add-between (map fn->html fn) ", "))
      (dd () ,@description)))

  `(dl ([class "function-list"])
     ,@(append-map fn-class fn-classes)))

(define/page (demo)
  (when (not (directory-exists? demo-output-path))
    (make-directory demo-output-path))

  (response/xexpr
   #:headers (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
   (herbie-page
    #:title (if (*demo*) "Herbie web demo" "Herbie")
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "/demo.js")
    `(p "Enter a formula below, hit " (kbd "Enter") ", and Herbie will try to improve it.")
    `(form ([action ,(embed/url improve)] [method "post"] [id "formula"]
            [data-progress ,(embed/url improve-start)])
           (input ([name "formula"] [autofocus "true"] [placeholder "(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))"]))
           (ul ([id "errors"]))
           (pre ([id "progress"] [style "display: none;"])))

    (if (*demo*)
        `(p "To handle the high volume of requests, web requests are queued; "
            "there are " (span ([id "num-jobs"]) ,(~a (hash-count *jobs*))) " jobs in the queue right now. "
            "Web demo requests may also time out and cap the number of improvement iterations. "
            "To avoid these limitations, " (a ([href "/installing.html"]) "install Herbie")
            " on your own computer.")
        "")

    `(p ([id "lisp-instructions"])
        "Please enter formulas as" (a ([href "http://fpbench.org/spec/fpcore-1.0.html"]) "FPCore")
        "expressions, including the top-level " (code "FPCore") " form, "
        "using only the following supported functions:")
    `(p ([id "mathjs-instructions"] [style "display: none;"])
        "You can write ordinary mathematical expressions (parsed with "
        (a ([href "https://mathjs.org"]) "math.js") ") using only the following supported functions:")

    (function-list
     '((+ - * / abs) "The usual arithmetic functions")
     '((sqrt sqr) "Squares and square roots")
     '((exp log) "Natural exponent and natural log")
     '((expt) "Raising a value to an exponent (also called " (code "pow") ")")
     '((sin cos tan) "The trigonometric functions")
     '((asin acos atan) "The inverse trigonometric functions")
     '((sinh cosh tanh) "The hyperbolic trigonometric functions"))
    `(p "You can also use the constants " (code "PI") " and " (code "E") ".")

    `(p (em "Note") ": all formulas submitted to the Herbie web demo are logged "
        "and made publicly accessible. See what formulas other users submitted "
        (a ([href "./report.html"]) "here") ".")
    )))

(define *completed-jobs* (make-hash))
(define *jobs* (make-hash))

(define *fixed-seed* #(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636))

(define *worker-thread*
  (thread
   (λ ()
     (let loop ([seed #f])
       (match (thread-receive)
         [`(init rand ,vec flags ,flag-table num-iters ,iterations points ,points)
          (set! seed vec)
          (*flags* flag-table)
          (*num-iterations* iterations)
          (*num-points* points)]
         [(list 'improve hash formula sema)
          (cond
           [(hash-has-key? *completed-jobs* hash)
            (semaphore-post sema)]
           [(directory-exists? (build-path demo-output-path hash))
            (semaphore-post sema)]
           [(directory-exists?
             (build-path demo-output-path (format "~a.crash.~a" hash *herbie-commit*)))
            (semaphore-post sema)]
           [else
            (eprintf "Job ~a started..." hash)

            (define result
              (parameterize ([*timeout* (* 1000 60)] [*reeval-pts* 1000])
                (get-test-result
                 #:seed seed
                 #:setup! (λ () (set-debug-level! 'progress '(3 4)))
                 #:debug (hash-ref *jobs* hash)
                 (parse-test formula))))

            (hash-set! *completed-jobs* result)

            ;; Output results

            (define dir
              (cond
               [(test-result? result) hash]
               [(test-timeout? result) hash]
               [(test-failure? result) (format "~a.crash.~a" hash *herbie-commit*)]))
            (make-directory (build-path demo-output-path dir))
            (define make-page
              (cond [(test-result? result) make-graph]
                    [(test-timeout? result) make-timeout]
                    [(test-failure? result) make-traceback]))
            (with-output-to-file (build-path demo-output-path dir "graph.html")
              (λ () (make-page result (build-path demo-output-path dir) #f)))

            (define data (get-table-data result dir))
            (define info
              (if (file-exists? (build-path demo-output-path "results.json"))
                  (let ([info (read-datafile (build-path demo-output-path "results.json"))])
                    (set-report-info-tests! info (cons data (report-info-tests info)))
                    info)
                  (make-report-info (list data) #:seed seed
                                    #:note (if (*demo*) "Web demo results" "Herbie web results"))))
            (write-datafile (build-path demo-output-path "results.json") info)
            (make-report-page (build-path demo-output-path "report.html") info)

            (eprintf " complete\n")
            (hash-remove! *jobs* hash)
            (semaphore-post sema)])])
       (loop seed)))))

(define (run-improve hash formula)
  (hash-set! *jobs* hash (open-output-string))
  (define sema (make-semaphore))
  (thread-send *worker-thread* (list 'improve hash formula sema))
  sema)

(define (already-computed? hash formula)
  (or (hash-has-key? *completed-jobs* hash)
      (directory-exists? (build-path demo-output-path hash))
      (directory-exists? (build-path demo-output-path (format "~a.crash.~a" hash *herbie-commit*)))))

(define (improve-common body go-back)
  (match (get-bindings 'formula)
    [(list formula-str)
     (define formula
       (with-handlers ([exn:fail? (λ (e) #f)])
         (read-syntax 'web (open-input-string formula-str))))
     (with-handlers
         ([exn:fail:user:herbie?
           (λ (e)
             (response/error
              "Demo Error"
              `(p "Invalid formula " (code ,formula-str) ". "
                  "Formula must be a valid program using only the supported functions. "
                  "Please " (a ([href ,go-back]) "go back") " and try again.")))])

       (assert-program! formula)
       (define hash (md5 (open-input-string formula-str)))
       (body hash formula))]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified serveral). "
                         "Please " (a ([href ,go-back]) "go back") " and try again."))]))

(define/page (improve-start)
  (improve-common
   (λ (hash formula)
     (unless (already-computed? hash formula)
       (run-improve hash formula))
     (response/full 201 #"Job started" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (embed/url (curryr check-status hash))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '(#"")))
   (embed/url demo)))

(define/page (check-status hash)
  (match (hash-ref *jobs* hash #f)
    [(? output-port? progress)
     (response 202 #"Job in progress" (current-seconds) #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
               (λ (out) (display (get-output-string progress) out)))]
    [#f
     (response/full 201 #"Job complete" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (format "/demo/~a/graph.html" hash)))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '())]))

(define/page (improve)
  (improve-common
   (λ (hash formula)
     (unless (already-computed? hash formula)
       (semaphore-wait (run-improve hash formula)))

     (redirect-to (format "/demo/~a/graph.html" hash) see-other))
   (embed/url demo)))

(define (response/error title body)
  (response/full 400 #"Bad Request" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (list (string->bytes/utf-8 (xexpr->string (herbie-page #:title title body))))))

(define (go [command-line #f])
  (eprintf "Server loaded and starting up.\n")

  (define config
    `(init rand ,(get-seed)
           flags ,(*flags*)
           num-iters ,(*num-iterations*)
           points ,(*num-points*)))
  (thread-send *worker-thread* config)

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
