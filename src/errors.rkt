#lang racket
(require "common.rkt")
(provide raise-herbie-error herbie-error->string exn:fail:user:herbie?)

(struct exn:fail:user:herbie exn:fail:user (url location)
        #:extra-constructor-name make-exn:fail:user:herbie)

(define (raise-herbie-error message #:location [location #f] #:url [url #f] . args)
  
  (raise (make-exn:fail:user:herbie
          (apply format message args) (current-continuation-marks) url location)))

(define (herbie-error->string err)
  (match-define (exn:fail:user:herbie message marks url location) err)
  (with-output-to-string
    (λ ()
      (when location
        (eprintf "~a: " (string-join (map ~a location))))
      (eprintf "~a\n" message)
      (when url
        (eprintf "See <https://herbie.uwplse.org/~a/~a> for more." *herbie-version* url)))))
