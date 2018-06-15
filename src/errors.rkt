#lang racket
(require "config.rkt")
(provide raise-herbie-error raise-herbie-syntax-error
         herbie-error->string herbie-error-url
         (struct-out exn:fail:user:herbie)
         (struct-out exn:fail:user:herbie:syntax))

(struct exn:fail:user:herbie exn:fail:user (url)
        #:extra-constructor-name make-exn:fail:user:herbie)

(struct exn:fail:user:herbie:syntax exn:fail:user:herbie (locations)
        #:extra-constructor-name make-exn:fail:user:herbie:syntax)

(define (raise-herbie-error message #:url [url #f] . args)
  (raise (make-exn:fail:user:herbie
          (apply format message args) (current-continuation-marks) url)))

(define (raise-herbie-syntax-error message #:url [url "faq.html#invalid-syntax"] #:locations [locations '()] . args)
  (raise (make-exn:fail:user:herbie:syntax
          (apply format message args) (current-continuation-marks) url locations)))

(define (herbie-error-url exn)
  (format "https://herbie.uwplse.org/doc/~a/~a"
          *herbie-version* (exn:fail:user:herbie-url exn)))

(define (herbie-error->string err)
  (with-output-to-string
    (λ ()
      (match err
        [(exn:fail:user:herbie:syntax message marks url locations)
         (eprintf "~a\n" message)
         (for ([(stx message) (in-dict locations)])
           (define file
             (if (path? (syntax-source stx))
                 (let-values ([(base name dir?) (split-path (syntax-source stx))])
                   (path->string name))
                 (syntax-source stx)))
           (eprintf "  ~a:~a:~a: ~a\n" file (or (syntax-line stx) "")
                    (or (syntax-column stx) (syntax-position stx)) message))
         (when url
           (eprintf "See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url))]
        [(exn:fail:user:herbie message marks url)
         (eprintf "~a\n" message)
         (when url
           (eprintf "See <https://herbie.uwplse.org/doc/~a/~a> for more.\n" *herbie-version* url))]))))

(define old-error-display-handler (error-display-handler))
(error-display-handler
 (λ (message err)
   (cond
    [(exn:fail:user:herbie? err)
     (display (herbie-error->string err) (current-error-port))]
    [(exn:fail:read? err)
     (printf "Invalid syntax\n  ~a\nSee <https://herbie.uwplse.org/doc/~a/input.html> for more.\n"
             (exn-message err) *herbie-version*)]
    [else
     (old-error-display-handler message err)])))
