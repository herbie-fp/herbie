#lang racket

(require json)
(require racket/exn)
(require openssl/sha1 (rename-in xml [location? xml-location?]))
(require web-server/servlet web-server/servlet-env web-server/dispatch
         web-server/dispatchers/dispatch web-server/dispatch/extend
         web-server/http/bindings web-server/configuration/responders
         web-server/managers/none)

(require "../common.rkt" "../config.rkt" "../syntax/read.rkt" "../errors.rkt")
(require "../syntax/syntax-check.rkt" "../syntax/type-check.rkt" "../syntax/types.rkt"
         "../syntax/sugar.rkt" "../alternative.rkt" "../points.rkt"
         "../programs.rkt" "../sandbox.rkt" "../float.rkt")
(require "../datafile.rkt" "pages.rkt" "make-report.rkt"
         "common.rkt" "core2mathjs.rkt" "history.rkt" "plot.rkt")
(require (submod "../timeline.rkt" debug))

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
         (let ([m (regexp-match #rx"^([0-9a-f]+)\\.[0-9a-f.]+" x)])
           (and m (hash-has-key? *completed-jobs* (second m))))))
  (λ (x)
    (let ([m (regexp-match #rx"^([0-9a-f]+)\\.[0-9a-f.]+" x)])
      (hash-ref *completed-jobs* (if m (second m) x)))))

(define-bidi-match-expander hash-arg hash-arg/m hash-arg/m)

(define-values (dispatch url*)
  (dispatch-rules
   [("") main]
   [("improve-start") #:method "post" improve-start]
   [("improve") #:method (or "post" "get" "put") improve]
   [("check-status" (string-arg)) check-status]
   [("up") check-up]
   [("api" "sample") #:method "post" sample-endpoint]
   [("api" "analyze") #:method "post" analyze-endpoint]
   [("api" "localerror") #:method "post" local-error-endpoint]
   [("api" "alternatives") #:method "post" alternatives-endpoint]
   [("api" "exacts") #:method "post" exacts-endpoint]
   [("api" "calculate") #:method "post" calculate-endpoint]
   [("api" "cost") #:method "post" cost-endpoint]
   [("api" "mathjs") #:method "post" ->mathjs-endpoint]
   [((hash-arg) (string-arg)) generate-page]
   [("results.json") generate-report]))

(define (generate-page req results page)
  (match-define result results)
  (cond
   [(set-member? (all-pages result) page)
    (response 200 #"OK" (current-seconds) #"text"
              (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
              (λ (out)
                (with-handlers ([exn:fail? (page-error-handler result page out)])
                  (make-page page out result (*demo-output*) #f))))]
   [else
    (next-dispatcher)]))

(define (generate-report req)
  (define data
    (for/list ([(k v) (in-hash *completed-jobs*)])
      (get-table-data v (format "~a.~a" hash *herbie-commit*))))
  (define info (make-report-info data #:seed (get-seed) #:note (if (*demo?*) "Web demo results" "Herbie results")))
  (response 200 #"OK" (current-seconds) #"text"
            (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
            (λ (out) (write-datafile out info))))

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

(define (herbie-page #:title title #:show-title [title? true]
                     #:scripts [scripts '()] #:styles [styles '()] #:head-include [other-include-head '()] . body)
  `(html
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     ,@other-include-head
     ,@(for/list ([script scripts])
         `(script ([src ,script] [type "text/javascript"])))
     (link ([rel "stylesheet"] [type "text/css"] [href "main.css"]))
     ,@(for/list ([style styles])
         `(link ([rel "stylesheet"] [type "text/css"] [href ,style]))))
    (body
     (header
      (img ([class "logo"] [src "/logo.png"]))
      ,@(if title? `((h1 ,title)) '()))
     ,@body)))

(define (main req)
  (when (and (*demo-output*) (not (directory-exists? (*demo-output*))))
    (make-directory (*demo-output*)))

  (response/xexpr
   #:headers (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
   (herbie-page
    #:title (if (*demo?*) "Herbie web demo" "Herbie")
    #:show-title (*demo?*)
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "demo.js")
    `(p "Write a formula below, and Herbie will try to improve it. Enter approximate ranges for inputs.")
    `(p ([id "options"])
       (a ([id "show-example"]) "Show an example")
       " | "
       (a ([id "use-fpcore"]) "Use FPCore")
       )
    (cond
      [(thread-running? *worker-thread*)
       `(form ([action ,(url improve)] [method "post"] [id "formula"]
               [data-progress ,(url improve-start)])
          (textarea ([name "formula"] [autofocus "true"]
                     [placeholder "(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))"]))
          (input ([name "formula-math"] [placeholder "sqrt(x + 1) - sqrt(x)"]))
          (table ([id "input-ranges"]))
          (ul ([id "errors"]))
          (ul ([id "warnings"]))
          (button ([id "run_herbie"] [type "submit"] [tabindex "-1"]) "Improve with Herbie")
          (pre ([id "progress"] [style "display: none;"])))]
      [(*demo?*)
       `(p ([id "crashed"])
           "Unfortunately, the online Herbie demo has crashed. The maintainers "
           "have been notified and will restart Herbie when they've got a moment, "
           "but if it's been a few days, it can help to "
           (a ([href "https://github.com/herbie-fp/herbie/issues/new"]
               [title "File a Herbie issue on Github"])
              "file an issue") ".")]
      [else
       `(p ([id "crashed"])
            "Unfortunately Herbie has crashed. You'll need to restart Herbie to "
            "continue using it. Please also "
            (a ([href "https://github.com/herbie-fp/herbie/issues/new"]
                [title "File a Herbie issue on Github"])
               "file a bug report")
            " with any error messages you find in your terminal.")])

    (if (*demo?*)
        `(p "To handle the high volume of requests, web requests are queued; "
            "there are " (span ([id "num-jobs"]) ,(~a (hash-count *jobs*))) " jobs in the queue right now. "
            "Web demo requests may also time out and cap the number of improvement iterations. "
            "To avoid these limitations, " (a ([href "/doc/latest/installing.html"]) "install Herbie")
            " on your own computer.")
        "")

    `(p ([id "lisp-instructions"])
        "Please enter formulas as " (a ([href "https://fpbench.org/spec/fpcore-1.0.html"]) "FPCore")
        " expressions, including the top-level " (code "FPCore") " form, "
        "using only the following supported functions:")
    `(p ([id "mathjs-instructions"] [style "display: none;"])
        "Use ordinary mathematical syntax (parsed by "
        (a ([href "https://mathjs.org"]) "math.js") ")"
        " and "
        (a ([href ,(format "https://herbie.uwplse.org/doc/~a/input.html" *herbie-version*)])
           "standard functions")
        " like:")
    
    (function-list
     '((+ - * / abs) "The usual arithmetic functions")
     '((and or) "Logical connectives (for preconditions)")
     '((pow) "Raising a value to a power")
     '((exp log) "Natural exponent and natural log")
     '((sin cos tan) "The trigonometric functions")
     '((asin acos atan) "The inverse trigonometric functions")
     '((sqrt cbrt) "Square and cube roots")
     '((PI E) "The mathematical constants"))

    `(p (em "Note") ": "
        ,@(cond
           [(not (*demo-output*))
            '("formulas submitted here are not logged.")]
           [(*demo?*)
            `("all formulas submitted here are logged and made public."
              (a ([href "./index.html"])" See what formulas other users submitted."))]
           [else
            `("all formulas submitted here are " (a ([href "./index.html"]) "logged") ".")])))))

(define *completed-jobs* (make-hash))
(define *jobs* (make-hash))

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
          (define path (format "~a.~a" hash *herbie-commit*))
          (cond
           [(hash-has-key? *completed-jobs* hash)
            (semaphore-post sema)]
           [(and (*demo-output*) (directory-exists? (build-path (*demo-output*) path)))
            (semaphore-post sema)]
           [else
            (eprintf "Job ~a started:\n  improve ~a...\n" hash (syntax->datum formula))

            (define result (run-herbie 'improve (parse-test formula) #:seed seed))

            (hash-set! *completed-jobs* hash result)

            (when (*demo-output*)
              ;; Output results
              (make-directory (build-path (*demo-output*) path))
              (for ([page (all-pages result)])
                (call-with-output-file (build-path (*demo-output*) path page)
                  (λ (out) 
                    (with-handlers ([exn:fail? (page-error-handler result page out)])
                      (make-page page out result (*demo-output*) #f)))))
              (update-report result path seed
                             (build-path (*demo-output*) "results.json")
                             (build-path (*demo-output*) "index.html")))

            (eprintf "Job ~a complete\n" hash)
            (hash-remove! *jobs* hash)
            (semaphore-post sema)])])
       (loop seed)))))

(define (update-report result dir seed data-file html-file)
  (define link (path-element->string (last (explode-path dir))))
  (define data (get-table-data result link))
  (define info
    (if (file-exists? data-file)
        (let ([info (read-datafile data-file)])
          (struct-copy report-info info [tests (cons data (report-info-tests info))]))
        (make-report-info (list data) #:seed seed #:note (if (*demo?*) "Web demo results" ""))))
  (define tmp-file (build-path (*demo-output*) "results.tmp"))
  (write-datafile tmp-file info)
  (rename-file-or-directory tmp-file data-file #t)
  (call-with-output-file html-file #:exists 'replace (curryr make-report-page info #f)))

(define (run-improve hash formula)
  (hash-set! *jobs* hash (*timeline*))
  (define sema (make-semaphore))
  (thread-send *worker-thread* (list 'improve hash formula sema))
  sema)

(define (already-computed? hash formula)
  (or (hash-has-key? *completed-jobs* hash)
      (and (*demo-output*)
           (directory-exists? (build-path (*demo-output*) (format "~a.~a" hash *herbie-commit*))))))

(define (post-with-json-response fn)
  (lambda (req)
    (define post-body (request-post-data/raw req))
    (define post-data (cond (post-body (bytes->jsexpr post-body)) (#t #f)))
    (define resp (with-handlers ([exn:fail? (λ (e) (hash 'error (exn->string e)))]) (fn post-data)))
    (if (hash-has-key? resp 'error)
        (response 500
                  #"Bad Request"
                  (current-seconds)
                  APPLICATION/JSON-MIME-TYPE
                  (list (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                  (λ (op) (write-json resp op)))
        (response 200
                  #"OK"
                  (current-seconds)
                  APPLICATION/JSON-MIME-TYPE
                  (list (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                  (λ (op) (write-json resp op))))))

(define (response/error title body)
  (response/full 400
                 #"Bad Request"
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 '()
                 (list (string->bytes/utf-8 (xexpr->string (herbie-page #:title title body))))))

(define (improve-common req body go-back)
  (match (extract-bindings 'formula (request-bindings req))
    [(list formula-str)
     (define formula
       (with-handlers ([exn:fail? (λ (e) #f)])
         (read-syntax 'web (open-input-string formula-str))))
     (unless formula
      (raise-herbie-error "bad input: did you include special characters like `#`?"))
     (with-handlers
         ([exn:fail:user:herbie?
           (λ (e)
             (response/error
              "Demo Error"
              `(div
                (h1 "Invalid formula")
                (pre ,(herbie-error->string e))
                (p
                  "Formula must be a valid program using only the supported functions. "
                  "Please " (a ([href ,go-back]) "go back") " and try again."))))])
       (when (eof-object? formula)
         (raise-herbie-error "no formula specified"))
       (parse-test formula)
       (define hash (sha1 (open-input-string formula-str)))
       (body hash formula))]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified several). "
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
                    '()))
   (url main)))

(define (check-status req hash)
  (match (hash-ref *jobs* hash #f)
    [(? box? timeline)
     (response 202 #"Job in progress" (current-seconds) #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
               (λ (out) (display (apply string-append
                                        (for/list ([entry (reverse (unbox timeline))])
                                          (format "Doing ~a\n" (hash-ref entry 'type))))
                                 out)))]
    [#f
     (response/full 201 #"Job complete" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (add-prefix (format "~a.~a/graph.html" hash *herbie-commit*))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '())]))

(define (check-up req)
  (response/full (if (thread-running? *worker-thread*) 200 500)
                 (if (thread-running? *worker-thread*) #"Up" #"Down")
                 (current-seconds) #"text/plain"
                 (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*))))
                       (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                 '()))

(define (improve req)
  (improve-common
   req
   (λ (hash formula)
     (unless (already-computed? hash formula)
       (semaphore-wait (run-improve hash formula)))

     (redirect-to (add-prefix (format "~a.~a/graph.html" hash *herbie-commit*)) see-other))
   (url main)))

; /api/sample endpoint: test in console on demo page:
;; (await fetch('/api/sample', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", seed: 5})})).json()
(define sample-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define seed (hash-ref post-data 'seed))
      (eprintf "Sampling job started on ~a..." formula)

      (define test (parse-test formula))
      (define result (run-herbie 'sample test #:seed seed #:profile? #f #:timeline-disabled? #t))
      (define pctx (job-result-backend result))

      (eprintf " complete\n")
      (hasheq 'points (pcontext->json pctx (context-repr (test-context test)))))))

(define analyze-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (eprintf "Analyze job started on ~a..." formula)

      (define test (parse-test formula))
      (define pcontext (json->pcontext sample (test-context test)))
      (define result (run-herbie 'errors test #:seed seed #:pcontext pcontext
                                 #:profile? #f #:timeline-disabled? #t))
      (define errs (job-result-backend result))

      (eprintf " complete\n")
      (hasheq 'points errs))))

;; (await fetch('/api/exacts', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", points: [[1, 1]]})})).json()
(define exacts-endpoint 
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (eprintf "Ground truth job started on ~a..." formula)

      (define test (parse-test formula))
      (define pcontext (json->pcontext sample (test-context test)))
      (define result (run-herbie 'exacts test #:seed seed #:pcontext pcontext
                                 #:profile? #f #:timeline-disabled? #t))
      (define exacts (job-result-backend result))

      (eprintf " complete\n")
      (hasheq 'points exacts))))

(define calculate-endpoint 
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (eprintf "Evaluation job started on ~a..." formula)

      (define test (parse-test formula))
      (define pcontext (json->pcontext sample (test-context test)))
      (define result (run-herbie 'evaluate test #:seed seed #:pcontext pcontext
                                 #:profile? #f #:timeline-disabled? #t))
      (define approx (job-result-backend result))

      (eprintf " complete\n")
      (hasheq 'points approx))))

(define local-error-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (eprintf "Local error job started on ~a..." formula)

      (define test (parse-test formula))
      (define expr (prog->fpcore (test-input test) (test-output-repr test)))
      (define pcontext (json->pcontext sample (test-context test)))
      (define result (run-herbie 'local-error test #:seed seed #:pcontext pcontext
                                 #:profile? #f #:timeline-disabled? #t))
      (define local-error (job-result-backend result))
      
      ;; TODO: potentially unsafe if resugaring changes the AST
      (define tree
        (let loop ([expr expr] [err local-error])
          (match expr
            [(list op args ...)
             ;; err => (List (listof Integer) List ...)
             (hasheq
              'e (~a op)
              'avg-error (format-bits (errors-score (first err)))
              'children (map loop args (rest err)))]
            [_
             ;; err => (List (listof Integer))
             (hasheq
              'e (~a expr)
              'avg-error (format-bits (errors-score (first err)))
              'children '())])))

      (eprintf " complete\n")
      (hasheq 'tree tree))))

(define alternatives-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (eprintf "Alternatives job started on ~a..." formula)

      (define test (parse-test formula))
      (define vars (test-vars test))
      (define repr (test-output-repr test))
      (define pcontext (json->pcontext sample (test-context test)))
      (define result (run-herbie 'alternatives test #:seed seed #:pcontext pcontext
                                 #:profile? #f #:timeline-disabled? #t))
      (match-define (list altns test-pcontext processed-pcontext) (job-result-backend result))
      
      (define splitpoints
        (for/list ([alt altns]) 
          (for/list ([var vars])
            (define split-var? (equal? var (regime-var alt)))
            (if split-var?
                (for/list ([val (regime-splitpoints alt)])
                  (real->ordinal (repr->real val repr) repr))
                '()))))

      (define fpcores
        (for/list ([altn altns])
          (~a (program->fpcore (alt-expr altn) (test-context test)))))
  
      (define histories
        (for/list ([altn altns])
          (let ([os (open-output-string)])
            (parameterize ([current-output-port os])
              (write-xexpr
                `(div ([id "history"])
                  (ol ,@(render-history altn
                                        processed-pcontext
                                        test-pcontext
                                        (test-context test)))))
              (get-output-string os)))))
      (define derivations 
        (for/list ([altn altns])
                  (render-json altn
                                        processed-pcontext
                                        test-pcontext
                                        (test-context test))))

      (eprintf " complete\n")
      (hasheq 'alternatives fpcores
              'histories histories
              'derivations derivations
              'splitpoints splitpoints))))

(define ->mathjs-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (eprintf "Converting to Math.js ~a..." formula)

      (define result (core->mathjs (syntax->datum formula)))
      (eprintf " complete\n")
      (hasheq 'mathjs result))))

(define cost-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (eprintf "Computing cost of ~a..." formula)
      
      (define test (parse-test formula))
      (define result (run-herbie 'cost test #:profile? #f #:timeline-disabled? #t))
      (define cost (job-result-backend result))

      (eprintf " complete\n")
      (hasheq 'cost cost))))

(define (run-demo #:quiet [quiet? #f] #:output output #:demo? demo? #:prefix prefix #:log log #:port port #:public? public)
  (*demo?* demo?)
  (*demo-output* output)
  (*demo-prefix* prefix)
  (*demo-log* log)

  (define config
    `(init rand ,(get-seed) flags ,(*flags*) num-iters ,(*num-iterations*) points ,(*num-points*)
           timeout ,(*timeout*) output-dir ,(*demo-output*) reeval ,(*reeval-pts*) demo? ,(*demo?*)))
  (thread-send *worker-thread* config)

  (eprintf "Herbie ~a with seed ~a\n" *herbie-version* (get-seed))
  (eprintf "Find help on https://herbie.uwplse.org/, exit with Ctrl-C\n")

  (serve/servlet
   dispatch
   #:listen-ip (if public #f "127.0.0.1")
   #:port port
   #:servlet-current-directory (current-directory)
   #:manager (create-none-manager #f)

   #:command-line? true
   #:launch-browser? (not quiet?)
   #:banner? true
   #:servlets-root (web-resource)
   #:server-root-path (web-resource)
   #:servlet-path "/"
   #:servlet-regexp #rx""
   #:extra-files-paths
   (filter identity (list (web-resource) (*demo-output*)))

   #:log-file (*demo-log*)
   #:file-not-found-responder
   (gen-file-not-found-responder (web-resource "404.html"))))

(module+ main
  (run-demo #t))
