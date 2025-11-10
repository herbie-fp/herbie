#lang racket
(require racket/runtime-path)
(require "../config.rkt"
         "../utils/errors.rkt"
         "platform.rkt")
(provide activate-platform!)

(define-runtime-module-path herbie10-platform "../platforms/herbie10.rkt")
(define-runtime-module-path herbie20-platform "../platforms/herbie20.rkt")
(define-runtime-module-path c-platform "../platforms/c.rkt")
(define-runtime-module-path c-windows-platform "../platforms/c-windows.rkt")
(define-runtime-module-path racket-platform "../platforms/racket.rkt")
(define-runtime-module-path math-platform "../platforms/math.rkt")
(define-runtime-module-path rival-platform "../platforms/rival.rkt")
(define-runtime-module-path growlibm-platform "../../growlibm/growlibm.rkt")
(define-runtime-module-path vanilla-platform "../../growlibm/vanilla.rkt")
(define-runtime-module-path herbie20plus-platform "../../growlibm/herbie20plus.rkt")

(define default-platforms
  (hash "herbie10"
        herbie10-platform
        "herbie20"
        herbie20-platform
        "c"
        c-platform
        "c-windows"
        c-windows-platform
        "racket"
        racket-platform
        "math"
        math-platform
        "rival"
        rival-platform
        "growlibm"
        growlibm-platform
        "vanilla"
        vanilla-platform
        "herbie20plus"
        herbie20plus-platform))

(define platforms (make-hash))

(define (activate-platform! name)
  (define path (hash-ref default-platforms name (string->path name)))
  (define platform (hash-ref! platforms name (lambda () (dynamic-require path 'platform))))

  (unless platform
    (raise-herbie-error "unknown platform `~a`, found (~a)"
                        name
                        (string-join (map ~a (hash-keys platforms)) ", ")))

  (*platform-name* name)
  (*active-platform* platform))
