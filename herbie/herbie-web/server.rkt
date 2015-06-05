#lang racket

(require openssl/md5)
(require xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require web-server/configuration/responders)
(require json)

(require "main.rkt")
(require "../config.rkt")
(require "../web-common.rkt")

(define *frontend-path* (make-parameter #f))
(define *style-path* (make-parameter #f))

(define/page (start-page)
  (when (not (and (*frontend-path*) (*style-path*)))
    (error "You didn't pass a javascript frontend and a stylesheet!"))
  (when (not (directory-exists? viz-output-path))
    (make-directory viz-output-path))
  (copy-file (*frontend-path*) (build-path viz-output-path "viz.js") #t)
  (copy-file (*style-path*) (build-path viz-output-path "style.css") #t)
  
  (response/xexpr
   (herbie-page
    #:title "Herbie Visual Shell"
    `(script ([type "text/x-mathjax-config"])
             "MathJax.Hub.Config({ TeX: { extensions: [\"enclose.js\"] } });")
    `(p "Enter a formula to explore it.")
    `(form ([action ,(embed/url interact)] [method "post"] [id "formula"])
           (input ([name "formula"] [autofocus "true"] [placeholder "(λ (a b c) (/ (- (- b) (sqrt (- (sqr b) (* 4 (* a c))))) (* 2 a)))"]))))))

(define/page (interact)
  (match-let ([(list formula) (get-bindings 'formula)])
    (let-values ([(content session-data) (start-session (read (open-input-string formula)))])
      (response/xexpr
       (herbie-page
        #:title "Herbie Visual Shell"
        #:scripts `("//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"
                    "http://d3js.org/d3.v3.min.js")
        #:styles '("style.css")
        `(div ([class "placeholder"]
               [data-json
                ,(embed/url (curryr serve-json (hash-ref content "start.json")))]))
        `(p "Work in progress!")
        `(script ([type "text/javascript"] [src "viz.js"])))))))

(define/page (serve-json json)
  (response/full 200 #"OK" (current-seconds) #"application/json" '() (jsexpr->bytes json)))

(define (start-server frontend-path style-path)
  (parameterize ([*frontend-path* frontend-path] [*style-path* style-path])
    (printf "Starting server\n")
    (serve/servlet
     start-page
     #:file-not-found-responder
     (gen-file-not-found-responder
      (build-path viz-output-path "../404.html"))
     #:port 3234
     #:listen-ip #f
     #:command-line? #t
     #:servlets-root (build-path viz-output-path "../..")
     #:server-root-path (build-path viz-output-path "..")
     #:servlet-path "/viz/"
     #:extra-files-paths (list (build-path viz-output-path "..")))))

(command-line
 #:program "herbie-viz"
 #:args arguments
 (apply start-server arguments))
