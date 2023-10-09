#lang racket

(require setup/getinfo racket/runtime-path)
(require "syntax/types.rkt" (submod "syntax/types.rkt" internals))
(provide load-herbie-plugins load-herbie-builtins make-debug-context)

(define-runtime-module-path default-platform  "platforms/default.rkt")
(define-runtime-module-path fallback-platform "platforms/fallback.rkt")
(define-runtime-module-path float-platform    "platforms/float.rkt")
(define-runtime-module-path math-platform     "platforms/math.rkt")
(define-runtime-module-path hardware-platform "platforms/hardware.rkt")

(define *default-platform* (make-parameter float-platform))
(define *active-platforms* (make-parameter '()))

(define (add-platform! key)
  (unless (member key (*active-platforms*))
    (eprintf "Using platform at ~a\n" key)
    (dynamic-require key #f)
    (*active-platforms* (cons key (*active-platforms*)))))

(define (load-herbie-builtins)
  (add-platform! (*default-platform*)))

;; loads builtin representations as needed
;; usually if 'load-herbie-plugins' has not been called
(define (generate-builtins name)
  (match name
    ['bool     (begin (add-platform! (*default-platform*))  #t)]
    ['binary64 (begin (add-platform! (*default-platform*))  #t)]
    ['binary32 (begin (add-platform! (*default-platform*))  #t)]
    ['racket   (begin (add-platform! fallback-platform) #t)]
    [_ #f]))

(define (load-herbie-plugins)
  (load-herbie-builtins)    ; automatically load default representations
  (for ([dir (find-relevant-directories '(herbie-plugin))])
    (define info
      (with-handlers ([exn:fail:filesystem? (const false)])
        (get-info/full dir)))
    (define value (info 'herbie-plugin (const false)))
    (when value
      (with-handlers ([exn:fail:filesystem:missing-module? void])
        (add-platform! value)))))

;; requiring "load-plugin.rkt" automatically registers
;; all built-in representation but does not load them
(register-generator! generate-builtins)

(define (make-debug-context vars)
  (load-herbie-builtins)
  (define repr (get-representation 'binary64))
  (context vars repr (map (const repr) vars)))
