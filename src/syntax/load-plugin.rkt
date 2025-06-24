#lang racket
(require racket/runtime-path
         setup/getinfo)
(require "../config.rkt"
         "platform.rkt"
         "types.rkt")

(provide load-herbie-builtins
         load-herbie-plugins
         make-debug-context)

;; Builtin platforms
(define-runtime-module-path herbie10-platform "../platforms/herbie10.rkt")
(define-runtime-module-path herbie20-platform "../platforms/herbie20.rkt")
(define-runtime-module-path c-platform "../platforms/c.rkt")
(define-runtime-module-path racket-platform "../platforms/racket.rkt")
(define-runtime-module-path math-platform "../platforms/math.rkt")
(define-runtime-module-path rival-platform "../platforms/rival.rkt")
(define-runtime-module-path softposit-platform "../platforms/softposit.rkt")

; Automatically loads default platforms
(define (load-herbie-builtins)
  ;; Load all platforms
  (dynamic-require herbie10-platform #f)
  (dynamic-require herbie20-platform #f)
  (dynamic-require c-platform #f)
  (dynamic-require racket-platform #f)
  (dynamic-require math-platform #f)
  (dynamic-require rival-platform #f)
  (dynamic-require softposit-platform #f)
  ; activate the required platform
  (activate-platform! (*platform-name*)))

(define (load-herbie-plugins)
  (load-herbie-builtins)
  ; search packages for herbie plugins
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when value
      (with-handlers ([exn:fail:filesystem:missing-module? void])
        (dynamic-require value #f))))
  ; load in "loose" plugins
  (for ([path (in-list (*loose-plugins*))])
    (dynamic-require path #f))
  ; activate the actual requred platform
  (activate-platform! (*platform-name*)))

(define (make-debug-context vars)
  (load-herbie-builtins)
  (define repr (get-representation 'binary64))
  (context vars repr (map (const repr) vars)))
