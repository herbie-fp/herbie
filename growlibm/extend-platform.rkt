#lang racket
(require json)
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

(define count-list (call-with-input-file "reports/counts.rkt" read))

(define json (string->jsexpr (first (file->lines filename))))
(define tests (hash-ref json 'tests))
;;; (define pairs (for/list ([t tests])
;;;                 (list (hash-ref t 'input)
;;;                       (hash-ref t 'link)
;;;                       (hash-ref t 'end)
;;;                       (hash-ref t 'input) '())))
(define scored-pairs
  (for/list ([t tests])
    (define input-str (hash-ref t 'input))
    (define link (hash-ref t 'link))
    (define end-val (hash-ref t 'end))

    (define found-count (assoc (with-input-from-string input-str read) count-list))
    (define count (if found-count
                      (cdr found-count)
                      (begin
                        (displayln (format "~a not found" input-str))
                        0)))
    (list input-str link end-val (* end-val count))))

(define sorted-pairs (sort scored-pairs > #:key fourth))
;;; (displayln json)

;;; (displayln count-list)
;;; (displayln (second (first pairs)))
;;; (displayln (first ))
;;; (displayln (fourth (first pairs)))
;;; (displayln count-list)
;;; (displayln (assoc (with-input-from-string (fourth (first pairs)) read) count-list))

(define fpcore (with-input-from-string (first (first sorted-pairs)) read))
(define link (second (first sorted-pairs)))
(define ctx (context (free-variables fpcore) 
                     (get-representation 'binary64) 
                     (make-list (length (free-variables fpcore)) 
                                (get-representation 'binary64))))

(define prog (fpcore->prog fpcore ctx))
(define spec (prog->spec prog))
;;; (define operatorImpl (create-operator-impl! (string->symbol (format "!~a!" spec)) ctx spec #:impl (from-rival) #:cost 1000))
(define (render-var var) (format "[~a <binary64>]" var))
;;; (define cost-proc (platform-cost-proc (*active-platform*)))
;;; (displayln (cost-proc expr (get-representation 'binary64)))

(define operatorStr (format "(define-operation (~a ~a) <binary64> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary64 (~a ~a)) #:cost 1000)"
                            link
                            (string-join (map render-var (free-variables spec)))
                            spec
                            link
                            (string-join (map symbol->string (free-variables spec)))))

(with-output-to-file "src/platforms/grow.rkt"
  (lambda ()
   (displayln operatorStr))
  #:exists 'append)