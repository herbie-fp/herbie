#lang racket
(require openssl/sha1 xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch
         web-server/dispatchers/dispatch web-server/dispatch/extend
         web-server/http/bindings web-server/configuration/responders
         web-server/managers/none)
(require "../sandbox.rkt")
(require "../formats/datafile.rkt" "../reports/make-graph.rkt" "../reports/make-report.rkt" "../reports/thread-pool.rkt")
(require "../formats/tex.rkt")
(require "../common.rkt" "../config.rkt" "../programs.rkt" "../formats/test.rkt" "../errors.rkt")
(require "../web/common.rkt")

(provide run-demo)

(define *demo?* (make-parameter false))
(define *demo-prefix* (make-parameter "/"))
(define *demo-output* (make-parameter false))
(define *demo-log* (make-parameter false))

(define (add-prefix url)
  (string-replace (string-append (*demo-prefix*) url) "//" "/"))

(define-coercion-match-expander hash-arg/m
  (λ (x)
    (and (not (*demo-output*))
         (or
          (and (regexp-match #rx"^[0-9a-f]+$" x) (hash-has-key? *completed-jobs* x))
          (let ([m (regexp-match #rx"^([0-9a-f]+).crash.[0-9a-f]+" x)])
            (and m (hash-has-key? *completed-jobs* (second m)))))))
  (λ (x)
    (let ([m (regexp-match #rx"^([0-9a-f]+).crash.[0-9a-f]+" x)])
      (hash-ref *completed-jobs* (if m (second m) x)))))

(define-bidi-match-expander hash-arg hash-arg/m hash-arg/m)

(define-values (dispatch url*)
  (dispatch-rules
   [("") main]
   [("improve-start") #:method "post" improve-start]
   [("improve") #:method "post" improve]
   [("check-status" (string-arg)) check-status]
   [((hash-arg) "graph.html") generate-report]
   [((hash-arg) "debug.log") generate-debug]
   [((hash-arg) (string-arg)) generate-plot]))

(define url (compose add-prefix url*))

(define (function-list . fn-classes)
  (define (fn->html fn)
    `(code ,(~a fn)))

  (define (fn-class class)
    (match-define (list fns description ...) class)
    `((dt () ,@(add-between (map fn->html fns) ", "))
      (dd () ,@description)))

  `(dl ([class "function-list"])
     ,@(append-map fn-class fn-classes)))

(define (main req)
  (when (and (*demo-output*) (not (directory-exists? (*demo-output*))))
    (make-directory (*demo-output*)))

  (response/xexpr
   #:headers (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
   (herbie-page
    #:title (if (*demo?*) "Herbie web demo" "Herbie")
    #:show-title (*demo?*)
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "demo.js")
    `(p "Enter a formula below, hit " (kbd "Enter") ", and Herbie will try to improve it.")
    `(form ([action ,(url improve)] [method "post"] [id "formula"]
            [data-progress ,(url improve-start)])
           (input ([name "formula"] [autofocus "true"] [placeholder "(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))"]))
           (ul ([id "errors"]))
           (pre ([id "progress"] [style "display: none;"])))

    (if (*demo?*)
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
        (a ([href "https://mathjs.org"]) "math.js") ") using:")

    (function-list
     '((+ - * / abs) "The usual arithmetic functions")
     '((sqrt sqr) "Squares and square roots")
     '((exp log) "Natural exponent and natural log")
     '((expt) "Raising a value to an exponent (also called " (code "pow") ")")
     '((sin cos tan) "The trigonometric functions")
     '((asin acos atan) "The inverse trigonometric functions")
     '((sinh cosh tanh) "The hyperbolic trigonometric functions")
     '((PI E) "The mathematical constants"))

    `(p (em "Note") ": "
        ,@(cond
           [(not (*demo-output*))
            '("formulas submitted here are not logged.")]
           [(*demo?*)
            `("all formulas submitted here are logged and made public."
              (a ([href "./results.html"])" See what formulas other users submitted."))]
           [else
            `("all formulas submitted here are " (a ([href "./results.html"]) "logged") ".")])))))

(define *completed-jobs* (make-hash))
(define *jobs* (make-hash))

(define *fixed-seed* #(2775764126 3555076145 3898259844 1891440260 2599947619 1948460636))

(define *worker-thread*
  (thread
   (λ ()
     (let loop ([seed #f])
       (match (thread-receive)
         [`(init rand ,vec flags ,flag-table num-iters ,iterations points ,points
                 timeout ,timeout output-dir ,output reeval ,reeval demo? ,demo?)
          (set! seed vec)
          (*flags* flag-table)
          (*num-iterations* iterations)
          (*num-points* points)
          (*timeout* timeout)
          (*demo-output* output)
          (*reeval-pts* reeval)
          (*demo?* demo?)]
         [(list 'improve hash formula sema)
          (cond
           [(hash-has-key? *completed-jobs* hash)
            (semaphore-post sema)]
           [(and (*demo-output*) (directory-exists? (build-path (*demo-output*) hash)))
            (semaphore-post sema)]
           [(and
             (*demo-output*)
             (directory-exists?
              (build-path (*demo-output*) (format "~a.crash.~a" hash *herbie-commit*))))
            (semaphore-post sema)]
           [else
            (eprintf "Job ~a started..." hash)

            (define result
              (get-test-result
               #:seed seed
               #:setup! (λ () (set-debug-level! 'progress '(3 4)))
               #:debug (hash-ref *jobs* hash)
               (parse-test formula)))

            (hash-set! *completed-jobs* hash (cons result (get-output-string (hash-ref *jobs* hash))))

            (when (*demo-output*)
              ;; Output results
              (define dir
                (cond
                 [(test-result? result) hash]
                 [(test-timeout? result) hash]
                 [(test-failure? result) (format "~a.crash.~a" hash *herbie-commit*)]))
              (make-directory (build-path (*demo-output*) dir))
              (define make-page
                (cond [(test-result? result) (λ args (apply make-graph args) (apply make-plots args))]
                      [(test-timeout? result) make-timeout]
                      [(test-failure? result) make-traceback]))
              (with-output-to-file (build-path (*demo-output*) dir "graph.html")
                (λ () (make-page result (build-path (*demo-output*) dir) #f)))

              (update-report result dir seed
                             (build-path (*demo-output*) "results.json")
                             (build-path (*demo-output*) "results.html")))

            (eprintf " complete\n")
            (hash-remove! *jobs* hash)
            (semaphore-post sema)])])
       (loop seed)))))

(define (update-report result dir seed data-file html-file)
  (define data (get-table-data result dir))
  (define info
    (if (file-exists? data-file)
        (let ([info (read-datafile data-file)])
          (struct-copy report-info info [tests (cons data (report-info-tests info))]))
        (make-report-info (list data) #:seed seed #:note (if (*demo?*) "Web demo results" ""))))
  (write-datafile data-file info)
  (make-report-page html-file info))

(define (run-improve hash formula)
  (hash-set! *jobs* hash (open-output-string))
  (define sema (make-semaphore))
  (thread-send *worker-thread* (list 'improve hash formula sema))
  sema)

(define (already-computed? hash formula)
  (or (hash-has-key? *completed-jobs* hash)
      (and (*demo-output*)
           (or
            (directory-exists? (build-path (*demo-output*) hash))
            (directory-exists? (build-path (*demo-output*) (format "~a.crash.~a" hash *herbie-commit*)))))))

(define (improve-common req body go-back)
  (match (extract-bindings 'formula (request-bindings req))
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
       (define hash (sha1 (open-input-string formula-str)))
       (body hash formula))]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified serveral). "
                         "Please " (a ([href ,go-back]) "go back") " and try again."))]))

(define (improve-start req)
  (improve-common
   req
   (λ (hash formula)
     (unless (already-computed? hash formula)
       (run-improve hash formula))
     (response/full 201 #"Job started" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (url check-status hash)))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '(#"")))
   (url main)))

(define (check-status req hash)
  (match (hash-ref *jobs* hash #f)
    [(? output-port? progress)
     (response 202 #"Job in progress" (current-seconds) #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
               (λ (out) (display (get-output-string progress) out)))]
    [#f
     (response/full 201 #"Job complete" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (add-prefix (format "~a/graph.html" hash))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '())]))

(define (improve req)
  (improve-common
   req
   (λ (hash formula)
     (unless (already-computed? hash formula)
       (semaphore-wait (run-improve hash formula)))

     (redirect-to (add-prefix (format "~a/graph.html" hash)) see-other))
   (url main)))

(define (generate-report req results)
  (match-define (cons result debug) results)

  (response 200 #"OK" (current-seconds) #"text/html"
            (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
            (λ (out)
              (parameterize ([current-output-port out])
                (make-graph result hash #f)))))

(define (generate-plot req results plotname)
  (match-define (cons result debug) results)

  (define responder
    (match (regexp-match #rx"^plot-([0-9]+)([rbg]?).png$" plotname)
      [#f (next-dispatcher)]
      [(list _ (app string->number idx) "")
       ;; TODO: rdir?
       (curry make-axis-plot result idx)]
      [(list _ (app string->number idx) (and (or "r" "g" "b") (app string->symbol letter)))
       (curry make-points-plot result idx letter)]))
  (response 200 #"OK" (current-seconds) #"text/html"
            (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
            responder))

(define (generate-debug req results)
  (match-define (cons result debug) results)

  (response 200 #"OK" (current-seconds) #"text/html"
            (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
            (λ (out) (display debug out))))

(define (response/error title body)
  (response/full 400 #"Bad Request" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (list (string->bytes/utf-8 (xexpr->string (herbie-page #:title title body))))))

(define (run-demo #:quiet [quiet? #f] #:output output #:demo? demo? #:prefix prefix #:log log #:port port)
  (*demo?* demo?)
  (*demo-output* output)
  (*demo-prefix* prefix)
  (*demo-log* log)

  (define config
    `(init rand ,(get-seed) flags ,(*flags*) num-iters ,(*num-iterations*) points ,(*num-points*)
           timeout ,(*timeout*) output-dir ,(*demo-output*) reeval ,(*reeval-pts*) demo? ,(*demo?*)))
  (thread-send *worker-thread* config)

  (serve/servlet
   dispatch
   #:listen-ip (if (*demo?*) #f "127.0.0.1")
   #:port port
   #:servlet-current-directory (current-directory)
   #:manager (create-none-manager #f)

   #:command-line? true
   #:launch-browser? (not quiet?)
   #:banner? true
   #:servlets-root web-resource-path
   #:server-root-path web-resource-path
   #:servlet-path "/"
   #:servlet-regexp #rx""
   #:extra-files-paths
   (if (*demo-output*)
       (list web-resource-path (*demo-output*))
       (list web-resource-path))

   #:log-file (*demo-log*)
   #:file-not-found-responder
   (gen-file-not-found-responder
    (build-path web-resource-path "404.html"))))

(module+ main
  (run-demo #t))
