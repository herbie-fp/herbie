#lang racket
(require setup/getinfo racket/runtime-path)
(require "config.rkt" "platform.rkt" "syntax/types.rkt")

(provide load-herbie-builtins load-herbie-plugins make-debug-context)

;; Builtin plugins
(define-runtime-module-path bool-plugin     "reprs/bool.rkt")
(define-runtime-module-path binary32-plugin "reprs/binary32.rkt")
(define-runtime-module-path binary64-plugin "reprs/binary64.rkt")
(define-runtime-module-path fallback-plugin "reprs/fallback.rkt")

;; Builtin platforms
(define-runtime-module-path arith-platform "platforms/arith.rkt")
(define-runtime-module-path arith-fma-platform "platforms/arith-fma.rkt")
(define-runtime-module-path c-platform "platforms/libm.rkt")
(define-runtime-module-path default-platform "platforms/default.rkt")
(define-runtime-module-path math-platform "platforms/math.rkt")
(define-runtime-module-path mkl-platform "platforms/mkl.rkt")
(define-runtime-module-path python-platform "platforms/python3-10.rkt")
(define-runtime-module-path julia-platform "platforms/julia.rkt")
(define-runtime-module-path avx-platform "platforms/avx.rkt")
(define-runtime-module-path numpy-platform "platforms/numpy.rkt")

; Automatically loads default representations and platforms
(define (load-herbie-builtins)
  ;; Load in all plugins
  ;; Warning: the order here is important!
  (dynamic-require bool-plugin #f)
  (dynamic-require binary64-plugin #f)
  (dynamic-require binary32-plugin #f)
  (dynamic-require fallback-plugin #f)
  ;; Load all platforms
  (dynamic-require arith-platform #f)
  (dynamic-require arith-fma-platform #f)
  (dynamic-require c-platform #f)
  (dynamic-require default-platform #f)
  (dynamic-require math-platform #f)
  (dynamic-require mkl-platform #f)
  (dynamic-require python-platform #f)
  (dynamic-require avx-platform #f)
  (dynamic-require numpy-platform #f)
  (dynamic-require julia-platform #f)
  ;; activate the default platform
  (*active-platform* (get-platform (*platform-name*)))
  (activate-platform! (*active-platform*)))

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
  ; (eprintf "activating platform `~a`\n" (*platform-name*))
  (*active-platform* (get-platform (*platform-name*)))
  (activate-platform! (*active-platform*)))

(define (make-debug-context vars)
  (load-herbie-builtins)
  (define repr (get-representation 'binary64))
  (context vars repr (map (const repr) vars)))
