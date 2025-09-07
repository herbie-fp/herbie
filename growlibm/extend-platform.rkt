#lang racket
(require json)
(require racket/random)
(require
  "../src/api/sandbox.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/types.rkt"
  "../src/core/points.rkt"
  "../src/core/rules.rkt"
  "../src/config.rkt"
  "../src/core/batch.rkt"
  "../src/core/egg-herbie.rkt"
  "../src/syntax/read.rkt"
  "../src/syntax/load-platform.rkt"
  "../src/syntax/platform.rkt"
  "../src/syntax/sugar.rkt"
  "../src/core/programs.rkt"
  "../src/syntax/syntax.rkt"
  "../src/reports/common.rkt"
  "../src/syntax/platform-language.rkt")


(activate-platform! "grow")

;;; (define spec '(+ x y))
;;; (define ctx (context (free-variables spec) (get-representation 'binary64) (make-list (length (free-variables spec)) (get-representation 'binary64))))

;;; (define operatorImpl (create-operator-impl! (string->symbol "test") ctx spec #:impl (from-rival) #:cost 1000))
;;; (platform-register-implementation! (*active-platform*) operatorImpl)

;;; (define expr '(test x y))

;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; (displayln (cost-proc expr (get-representation 'binary64)))

(define filename (vector-ref (current-command-line-arguments) 0))

(define json (string->jsexpr (first (file->lines filename))))
(define tests (hash-ref json 'tests))
(define pairs (for/list ([t tests])
                (cons (hash-ref t 'spec)
                      (hash-ref t 'end))))
(define sorted-pairs (sort pairs (lambda (p1 p2) (> (cdr p1) (cdr p2)))))
(define spec (with-input-from-string (car (first sorted-pairs)) read))
(define ctx (context (free-variables spec) (get-representation 'binary64) (make-list (length (free-variables spec)) (get-representation 'binary64))))
;;; (define operatorImpl (create-operator-impl! (string->symbol (format "!~a!" spec)) ctx spec #:impl (from-rival) #:cost 1000))
(define (render-var var) (format "[~a <binary64>]" var))
(define (random-string n #:alphabet [alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"])
  (list->string
   (for/list ([i n])
     (string-ref alphabet (random (string-length alphabet))))))

(define op-name (random-string 8))
(define operatorStr (format "(define-operation (~a ~a) <binary64> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary64 (~a ~a)) #:cost 1000)"  op-name (string-join (map render-var (free-variables spec))) spec op-name (string-join (map symbol->string (free-variables spec)))))
(displayln operatorStr)

;;; (platform-register-implementation! (*active-platform*) operatorImpl)

;;; (define ops (platform-implementations (*active-platform*)))
;;; (for-each
;;;  (lambda (pair)
;;;    (printf "~a: ~a\n" (car pair) (cdr pair)))
;;;  (hash->list ops))

;;; (define-operation (hypot.f64 [x <binary64>] [y <binary64>]) <binary64>
;;;    #:spec (sqrt (+ (* x x) (* y y))) #:impl (from-rival)
;;;    #:cost 0)