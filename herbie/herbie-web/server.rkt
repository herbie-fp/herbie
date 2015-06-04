#lang racket

(require openssl/md5)
(require xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require web-server/configuration/responders)

(require "main.rkt")

(define *frontend-url* (make-parameter #f))
(define *style-url* (make-parameter #f))

(define/page (start-page)
  (when (not (and (*frontend-url*) (*style-url*)))
    (error "You didn't pass a javascript frontend and a stylesheet!"))
  (when (not (directory-exists? viz-output-path))
    (make-directory viz-output-path))
  
  (response/xexpr
   (herbie-page
    #:title "Herbie Visual Shell"
    `(script ([type "text/x-mathjax-config"])
             "MathJax.Hub.Config({ TeX: { extensions: [\"enclose.js\"] } });")
    `(p "Enter a formula to explore it.")
    `(form ([action ,(embed/url interact)] [method "post"] [id "formula"])
           (input ([name "formula"] [autofocus "true"] [placeholder "(λ (a b c) (/ (- (- b) (sqrt (- (sqr b) (* 4 (* a c))))) (* 2 a)))"]))))))

(define/page (interact)
  (response/xexpr
   (herbie-page
    #:title "Herbie Visual Shell"
    #:scripts `("//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"
                "http://d3js.org/d3.v3.min.js"
                ,(*frontend-url*))
    `(p "Work in progress!"))))

(define (start-server frontend-url style-url)
  (parameterize ([*frontend-url* frontend-url] [*style-url* style-url])
    (serve/servelet
     start-page
     #:file-not-found-responder
     (gen-file-not-found-responder
      (build-path viz-output-path "../404.html"))
     #:command-line? #t
     #:banner? #f
     #:servlets-root (build-path viz-output-path "../..")
     #:server-root-path (build-path viz-output-path "..")
     #:servlet-path "/viz/")))

(command-line
 #:program "herbie-viz"
 #:args arguments
 (apply start-server arguments))
