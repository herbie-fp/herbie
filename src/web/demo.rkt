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
   [("api" "translate") #:method "post" translate-endpoint]
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
    (for/list ([(k v) (in-hash *completed-jobs*)]
      #:when (equal? (job-result-command v) 'improve))
       (get-table-data v (format "~a.~a" k *herbie-commit*))))
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
         [job-info (run-job job-info)])
       (loop seed)))))

(struct run-herbie-command 
 (command formula seed pcontext profile? timeline-disabled?))

(define (wrapper-run-herbie cmd job-id after-job-work)
  (print-job-message (run-herbie-command-command cmd) job-id (syntax->datum (run-herbie-command-formula cmd)))
  (define result (run-herbie 
   (run-herbie-command-command cmd)
   (parse-test (run-herbie-command-formula cmd))
   #:seed (run-herbie-command-seed cmd)
   #:pcontext (run-herbie-command-pcontext cmd)
   #:profile? (run-herbie-command-profile? cmd)
   #:timeline-disabled? (run-herbie-command-timeline-disabled? cmd)))
  (hash-set! *completed-jobs* job-id result)
  (after-job-work result job-id (run-herbie-command-seed cmd)))

(define (after-improve result job-id seed)
 (define path (format "~a.~a" job-id *herbie-commit*))
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
                    (build-path (*demo-output*) "index.html"))))

; A place holder helper function that should be used when no work needs to be
; done on the finished result
(define (default-after result job-id seed) empty)

(define (run-job job-info)
 (match-define (work job-id info sema) job-info)
 (define path (format "~a.~a" job-id *herbie-commit*))
 (cond ;; Check caches if job as already been completed
  [(hash-has-key? *completed-jobs* job-id)
   (semaphore-post sema)]
  [(and (*demo-output*) (directory-exists? (build-path (*demo-output*) path)))
   (semaphore-post sema)]
  [else (match info
   [(list 'improve formula)
    (wrapper-run-herbie 
     (run-herbie-command 'improve formula (get-seed) #f #f #f) 
      job-id after-improve)]
   [(list 'sample formula seed*)
    (wrapper-run-herbie 
     (run-herbie-command 'sample formula seed* #f #f #t) 
      job-id default-after)]
   [(list 'errors formula seed* pcontext)
    (wrapper-run-herbie 
     (run-herbie-command 'errors formula seed* pcontext #f #t) 
      job-id default-after)]
   [(list 'exacts formula seed* pcontext)
    (wrapper-run-herbie 
     (run-herbie-command 'exacts formula seed* pcontext #f #t) 
      job-id default-after)]
   [(list 'evaluate formula seed* pcontext)
    (wrapper-run-herbie 
     (run-herbie-command 'evaluate formula seed* pcontext #f #t) 
      job-id default-after)]
   [(list 'local-error formula seed* pcontext)
    (wrapper-run-herbie 
     (run-herbie-command 'local-error formula seed* pcontext #f #t) 
      job-id default-after)]
   [(list 'alternatives formula seed* pcontext)
    (wrapper-run-herbie 
     (run-herbie-command 'alternatives formula seed* pcontext #f #t) 
      job-id default-after)]
   [(list 'cost formula)
    (wrapper-run-herbie 
     (run-herbie-command 'cost formula #f #f #f #t) 
      job-id default-after)])
   (eprintf "Job ~a complete\n" job-id)
   (hash-remove! *jobs* job-id)
   (semaphore-post sema)]))

; Handles semaphore and async part of a job
(struct work (id info sema))

; Encapsulates semaphores and async part of jobs.
(define (run-work #:sync? [sync-job? #t] job)
 (define job-id (compute-job-id job))
 (hash-set! *jobs* job-id (*timeline*))
 (define sema (make-semaphore))
  (thread-send *worker-thread* (work job-id job sema))
 (when sync-job?
  (semaphore-wait sema)))

(define (print-job-message command job-id job-str)
  (define job-label
    (match command 
      ['alternatives "Alternatives"]
      ['evaluate "Evaluation"]
      ['cost "Computing"]
      ['errors "Analyze"]
      ['exacts "Ground truth"]
      ['improve "Improve"]
      ['local-error "Local error"]
      ['sample "Sampling"]
      [_ (error 'compute-result "unknown command ~a" command)]))
  (eprintf "~a Job ~a started:\n  ~a ~a...\n" job-label (symbol->string command) job-id job-str))

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

(define (already-computed? job-id formula)
  (or (hash-has-key? *completed-jobs* job-id)
      (and (*demo-output*)
           (directory-exists? (build-path (*demo-output*) (format "~a.~a" job-id *herbie-commit*))))))

(define (post-with-json-response fn)
  (lambda (req)
    (define post-body (request-post-data/raw req))
    (define post-data (cond (post-body (bytes->jsexpr post-body)) (#t #f)))
    (define resp (with-handlers ([exn:fail? (λ (e) (hash 'error (exn->string e)))]) (fn post-data)))
    (if (hash-has-key? resp 'error)
        (eprintf "Error handling request: ~a\n" (hash-ref resp 'error))
        (eprintf "Success handling request\n")
    )
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
       (body formula))]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified several). "
                         "Please " (a ([href ,go-back]) "go back") " and try again."))]))

(define (improve-start req)
  (improve-common
   req
   (λ (formula)
     (define job (list 'improve formula))
     (define job-id (compute-job-id job))
     (unless (already-computed? job-id formula)
      (run-work #:sync? #f job))
     (response/full 201 #"Job started" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (url check-status job-id)))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '()))
   (url main)))

(define (check-status req job-id)
  (match (hash-ref *jobs* job-id #f)
    [(? box? timeline)
     (response 202 #"Job in progress" (current-seconds) #"text/plain"
               (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
               (λ (out) (display (apply string-append
                                        (for/list ([entry (reverse (unbox timeline))])
                                          (format "Doing ~a\n" (hash-ref entry 'type))))
                                 out)))]
    [#f
     (response/full 201 #"Job complete" (current-seconds) #"text/plain"
                    (list (header #"Location" (string->bytes/utf-8 (add-prefix (format "~a.~a/graph.html" job-id *herbie-commit*))))
                          (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*)))))
                    '())]))

(define (check-up req)
  (response/full (if (thread-running? *worker-thread*) 200 500)
                 (if (thread-running? *worker-thread*) #"Up" #"Down")
                 (current-seconds) #"text/plain"
                 (list (header #"X-Job-Count" (string->bytes/utf-8 (~a (hash-count *jobs*))))
                       (header #"Access-Control-Allow-Origin" (string->bytes/utf-8 "*")))
                 '()))

(define (compute-job-id job-info)
 (sha1 (open-input-string (~s job-info))))

(define (improve req)
  (improve-common
   req
   (λ (formula)
     (define job (list 'improve formula))
     (define job-id (compute-job-id job))
     (unless (already-computed? job-id formula)
      (run-work #:sync? #t job))
     (redirect-to (add-prefix (format "~a.~a/graph.html" job-id *herbie-commit*)) see-other))
   (url main)))

; /api/sample endpoint: test in console on demo page:
;; (await fetch('/api/sample', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", seed: 5})})).json()
(define sample-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula-str (hash-ref post-data 'formula))
      (define formula (read-syntax 'web (open-input-string formula-str)))
      (define seed* (hash-ref post-data 'seed))
      (define command (list 'sample formula seed*))
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (define result (hash-ref *completed-jobs* job-id))
      (define pctx (job-result-backend result))
      (define test (parse-test formula))
      (hasheq 'points (pcontext->json pctx 
       (context-repr (test-context test)))))))

(define analyze-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula-str (hash-ref post-data 'formula))
      (define formula (read-syntax 'web (open-input-string formula-str)))
      (define sample (hash-ref post-data 'sample))
      (define seed (hash-ref post-data 'seed #f))
      (define pcontext (json->pcontext sample 
       (test-context (parse-test formula))))     
      (define command (list 'errors formula seed pcontext))      
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (define errs
        (for/list ([pt&err (job-result-backend (hash-ref *completed-jobs* job-id))])
          (define pt (first pt&err))
          (define err (second pt&err))
          (list pt (format-bits (ulps->bits err)))))
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
      (define command (list 'evaluate formula seed pcontext))
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (define approx (job-result-backend (hash-ref *completed-jobs* job-id)))
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
      (define command (list 'local-error formula seed pcontext))
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (define local-error (job-result-backend 
       (hash-ref *completed-jobs* job-id)))
      
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
      (define command (list 'alternatives formula seed pcontext))
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (match-define (list altns test-pcontext processed-pcontext) (job-result-backend (hash-ref *completed-jobs* job-id)))
      
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
      (hasheq 'mathjs result))))

(define cost-endpoint
  (post-with-json-response
    (lambda (post-data)
      (define formula (read-syntax 'web (open-input-string (hash-ref post-data 'formula))))
      (eprintf "Computing cost of ~a..." formula)
      
      (define test (parse-test formula))
      (define command (list 'cost formula))
      (define job-id (compute-job-id command))
      (run-work #:sync? #t command)
      (define cost (job-result-backend (hash-ref *completed-jobs* job-id)))
      (hasheq 'cost cost))))

(define translate-endpoint
  (post-with-json-response
    (lambda (post-data)
      ; FPCore formula and target language
      (define formula (read (open-input-string (hash-ref post-data 'formula))))
      (eprintf "Translating formula: ~a...\n" formula)
      (define target-lang (hash-ref post-data 'language))
      (eprintf "Target language: ~a...\n" target-lang)
      ; Select the appropriate conversion function
      (define lang-converter (case target-lang
        [("python") core->python]
        [("c") core->c]
        [("fortran") core->fortran]
        [("java") core->java]
        [("julia") core->julia]
        [("matlab") core->matlab]
        [("wls") core->wls]
        [("tex") core->tex]
        [("js") core->js]
        [else (error "Unsupported target language:" target-lang)]))

      ; convert the expression
      (define converted (lang-converter formula "expr"))
      (eprintf "Converted Expression: \n~a...\n" converted)
      (hasheq 'result converted
              'language target-lang))))

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
