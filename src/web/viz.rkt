#lang racket

(require openssl/md5)
(require xml)
(require web-server/servlet web-server/servlet-env web-server/dispatch web-server/page)
(require web-server/configuration/responders)
(require json)

(require "../config.rkt")
(require "../formats/tex.rkt")
(require "common.rkt")
(require "session.rkt")

(define *frontend-path* (make-parameter #f))
(define *style-path* (make-parameter #f))
(define *input-path* (make-parameter "src/viz/input.js"))

(define/page (start-page)
  (when (not (and (*frontend-path*) (*style-path*)))
    (error "You didn't pass a javascript frontend and a stylesheet!"))
  (define imagedir (build-path viz-output-path "images"))
  (when (not (directory-exists? viz-output-path))
    (make-directory viz-output-path)
    (make-directory imagedir))
  (for ([file (directory-list imagedir)])
    (delete-file (build-path imagedir file)))
  (copy-file (*frontend-path*) (build-path viz-output-path "viz.js") #t)
  (copy-file (*style-path*) (build-path viz-output-path "style.css") #t)
  (copy-file (*input-path*) (build-path viz-output-path "input.js") #t)
  
  (response/xexpr
   (herbie-page
    #:title "Herbie Visual Shell"
    #:scripts '("//cdnjs.cloudflare.com/ajax/libs/mathjs/1.6.0/math.min.js" "input.js")
    `(p "Enter a formula to explore it.")
    `(form ([action ,(embed/url interact)] [method "post"] [id "formula"])
           (input ([name "formula"] [autofocus "true"]
		   [placeholder "(λ (x) (+ 1 x))"]))
	   (ul ([id "errors"]))))))

(define/page (interact)
  (let ([formula (get-binding 'lisp_formula)])
    (let-values ([(response image-funcs session-data)
		  (start-session (read (open-input-string formula)))])
      (response/xexpr
       (herbie-page
        #:title "Herbie Visual Shell"
	#:head-include
	`((script ([type "text/x-mathjax-config"])
		 "MathJax.Hub.Config({ TeX: { extensions: [\"enclose.js\"] } });"))
        #:scripts (list mathjax-url
                    "http://d3js.org/d3.v3.min.js")
        #:styles '("style.css")
        `(div ([class "placeholder"]
               [data-json
                ,(embed/url
		  (curryr serve-json
			  (hash-set response 'next_link
				    (embed/url (curryr choose-children-page session-data)))
			  image-funcs))]))
        `(script ([type "text/javascript"] [src "viz.js"])))))))

(define (splice-image-urls response image-funcs)
  (for/fold ([response* response])
      ([image-func image-funcs] [idx (in-naturals)])
    (let* ([filename (symbol->string (gensym "image"))]
	   [path (build-path "images" filename)]
	   [full-path (build-path viz-output-path path)])
      (image-func full-path)
      (string-replace response* (format "&embedimage{~a}" idx)
		      (path->string path)))))

(define/page (choose-children-page data)
  (match-let ([(list location-idx) (get-bindings 'location-idx)])
    (let-values ([(response image-funcs session-data)
		  (select-location
		   data (string->number location-idx))])
      (json-response (hash-set response
			       'next_link
			       (embed/url (curryr pick-next-page session-data)))
		     image-funcs))))

(define/page (pick-next-page data)
  (match-let ([chosen-idxs (get-bindings 'chosen-idx)])
    (let-values ([(response image-funcs session-data)
		  (choose-children
		   data (map string->number chosen-idxs))])
      (json-response (hash-set*
                      response
                      'next_link (embed/url (curryr interact-more-page
                                                    session-data))
                      'done_link (embed/url (curryr done-page session-data)))
		     image-funcs))))

(define/page (done-page data)
  (let-values ([(response image-funcs session-data)
                (finish data)])
    (response/xexpr
     (herbie-page
      #:title "Herbie Visual Shell"
      #:scripts (list mathjax-url)
      `(h1 "Here's the final result:")
      `(p ,(hash-ref response 'formula))
      (let* ([filename (symbol->string (gensym "image"))]
             [path (build-path "images" filename)]
             [full-path (build-path viz-output-path path)])
        ((car image-funcs) full-path)
        `(img ([src ,(path->string path)])))))))

(define/page (interact-more-page data)
  (let ([cand-idx (get-binding 'cand-idx)])
    (let-values ([(response image-funcs session-data)
		  (pick-next data (string->number cand-idx))])
      (json-response (hash-set*
		      response
		      'next_link (embed/url (curryr choose-children-page session-data))
		      'repick_link (embed/url (curryr interact-more-page session-data)))
		     image-funcs))))

(define (json-response json images)
  (define parsed (string->bytes/utf-8 (splice-image-urls (jsexpr->string json) images)))
  (call-with-output-file (build-path viz-output-path "last-json.tmp") #:exists 'replace
    (curry write-bytes parsed))
  (response/full 200 #"OK" (current-seconds) #"application/json" '()
		 (list parsed)))


(define/page (serve-json json images)
  (json-response json images))

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

(module+ main
(command-line
 #:program "herbie-viz"
 #:args arguments
 (apply start-server arguments)))
