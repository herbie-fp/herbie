#lang racket
(require setup/getinfo racket/runtime-path)
(provide load-herbie-plugins)

(define-runtime-module-path bool-plugin "reprs/bool.rkt")
(define-runtime-module-path binary32-plugin "reprs/binary32.rkt")
(define-runtime-module-path binary64-plugin "reprs/binary64.rkt")
(define-runtime-module-path fallback-plugin "reprs/fallback.rkt")

(define (load-herbie-builtins)
  (dynamic-require bool-plugin #f)
  (dynamic-require binary64-plugin #f)
  (dynamic-require binary32-plugin #f)
  (dynamic-require fallback-plugin #f))

(define (load-herbie-plugins)
  (load-herbie-builtins)    ; automatically load default representations
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when value
      (with-handlers ([exn:fail:filesystem:missing-module? void])
        (dynamic-require value #f)))))
