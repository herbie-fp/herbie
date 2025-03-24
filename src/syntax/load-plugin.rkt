#lang racket
(require racket/runtime-path
         setup/getinfo)
(require "../config.rkt"
         "platform.rkt"
         "types.rkt")

(provide load-herbie-builtins
         load-herbie-plugins
         make-debug-context)

;; Builtin plugins
(define-runtime-module-path bool-plugin "../platforms/bool.rkt")
(define-runtime-module-path binary32-plugin "../platforms/binary32.rkt")
(define-runtime-module-path binary64-plugin "../platforms/binary64.rkt")
(define-runtime-module-path fallback-plugin "../platforms/fallback.rkt")

;; Builtin platforms
(define-runtime-module-path c-platform "../platforms/libm.rkt")
(define-runtime-module-path default-platform "../platforms/default.rkt")
(define-runtime-module-path math-platform "../platforms/math.rkt")
(define-runtime-module-path flopoco-accelerators-platform "../platforms/flopoco-accelerators.rkt")
(define-runtime-module-path arith-platform "../platforms/arith.rkt")
(define-runtime-module-path no-accel-platform "../platforms/new-accelerators.rkt")
(define-runtime-module-path new-accel-platform "../platforms/new-accelerators.rkt")
(define-runtime-module-path cubic-platform "../platforms/cubic.rkt")

; Automatically loads default representations and platforms
(define (load-herbie-builtins)
  ;; Load in all plugins
  ;; Warning: the order here is important!
  (dynamic-require bool-plugin #f)
  (dynamic-require binary64-plugin #f)
  (dynamic-require binary32-plugin #f)
  (dynamic-require fallback-plugin #f)
  ;; Load all platforms
  (dynamic-require c-platform #f)
  (dynamic-require default-platform #f)
  (dynamic-require math-platform #f)
  (dynamic-require arith-platform #f)
  (dynamic-require flopoco-accelerators-platform #f)
  (dynamic-require no-accel-platform #f)
  (dynamic-require new-accel-platform #f)
  (dynamic-require cubic-platform #f)

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
