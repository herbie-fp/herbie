#lang racket
(require openssl/md5)
(require xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require "thread-pool.rkt" "datafile.rkt" "make-graph.rkt" "make-report.rkt")
(require "../compile/tex.rkt")
(require "../common.rkt" "../config.rkt" "../programs.rkt" "../test.rkt")

(define/page (demo)
  (when (not (directory-exists? demo-output-path))
    (make-directory demo-output-path))

  (response/xexpr
   (herbie-page
    #:title "Herbie web demo"
    `(p "Enter a formula below, and Herbie will try to improve it.")
    `(form ([action ,(embed/url improve)] [method "post"] [id "formula"])
           (input ((name "formula"))))
    `(p "Note: all formulas submitted to the Herbie web demo are logged "
        "and made publicly accessible. See what formulas other users submitted "
        (a ([href "./report.html"]) "here") ".")
    `(p "Supported functions:")
    `(dl ([class "function-list"])
      (dt ,@(list-join (for/list ([i '(+ - * / abs)]) `(code ,(~a i))) '(", ")))
      (dd "The usual arithmetic functions")
      (dt ,@(list-join (for/list ([i '(sqrt sqr)]) `(code ,(~a i))) '(", ")))
      (dd "Squares and square roots")
      (dt ,@(list-join (for/list ([i '(exp log)]) `(code ,(~a i))) '(", ")))
      (dd "Natural exponent and natural log")
      (dt ,@(list-join (for/list ([i '(expt)]) `(code ,(~a i))) '(", ")))
      (dd "Raising a value to a power")
      (dt ,@(list-join (for/list ([i '(sin cos tan cot)]) `(code ,(~a i))) '(", ")))
      (dd "The trigonometric functions")
      (dt ,@(list-join (for/list ([i '(asin acos atan)]) `(code ,(~a i))) '(", ")))
      (dd "The inverse trigonometric functions")
      (dt ,@(list-join (for/list ([i '(sinh cosh tanh)]) `(code ,(~a i))) '(", ")))
      (dd "The hyperbolic trigonometric functions")))))

(define (herbie-page #:title title #:scripts [scripts '()] . body)
  `(html
    (head
     (meta ([charset "utf-8"]))
     (title ,title)
     ,@(for/list ([script scripts])
         `(script ([src ,script] [type "text/javascript"])))
     (link ([rel "stylesheet"] [type "text/css"] [href "/main.css"])))
    (body
     (header
      (img ([class "logo"] [src "/logo.png"]))
      (h1 ,title)
      (p "See " (a ([href "/index.html"]) "the main page") " for more info on Herbie."))
     ,@body)))

(define/page (improve)
  (match (get-bindings 'formula)
    [(list formula-str)
     (match-define `(lambda ,vars ,body) (read (open-input-string formula-str)))
     (define hash (md5 (open-input-string formula-str)))
     (define dir (build-path demo-output-path hash))

     (when (not (directory-exists? dir))
       (make-directory dir)
       (define result
         (parameterize ([*timeout* (* 1000 60)] [*reeval-pts* 1000])
           (get-test-result (test "User test" vars (map (const 'default) vars) body #f) dir)))
       (define make-page
         (cond [(test-result? result) make-graph]
               [(test-timeout? result) make-timeout]
               [(test-failure? result) make-traceback]))
       (with-output-to-file (build-path dir "graph.html")
         (λ () (make-page result #f)))

       (define data (get-table-data result))

       ; Save new report data
       (define info 
         (if (file-exists? (build-path demo-output-path "results.json"))
             (let ([info (read-datafile (build-path demo-output-path "results.json"))])
               (set-report-info-tests! info (cons data (report-info-tests info)))
               info)
             (make-report-info (list data) #:note "Web demo results")))
       (write-datafile (build-path demo-output-path "results.json") info)
       (make-report-page (build-path demo-output-path "report.html") info))

     (redirect-to (format "/demo/~a/graph.html" hash) see-other)]
    [_
     (response/error "Demo Error"
                     `(p "You didn't specify a formula (or you specified serveral). "
                         "Please " (a ([href ,(embed/url demo)]) "go back") " and try again."))]))

(define (response/error title body)
  (response/full 400 #"Bad Request" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (xexpr->string (herbie-page #:title title body))))

(define (go)
  (serve/servlet
   demo
   #:listen-ip #f
   #:banner? #f
   #:servlets-root (build-path demo-output-path "../..")
   #:servlet-path "/demo/"
   #:extra-files-paths (list (build-path demo-output-path ".."))))

(module+ main
  (go))
