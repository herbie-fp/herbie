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

(define filename (vector-ref (current-command-line-arguments) 0))

(define count-list (call-with-input-file "reports/counts.rkt" read))

(define json (string->jsexpr (first (file->lines filename))))
(define tests (hash-ref json 'tests))
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
    (define score (if (number? end-val)
                      (* end-val count)
                    0))
    (list input-str link end-val score)))

(define sorted-pairs (sort scored-pairs > #:key fourth))

(define fpcore (with-input-from-string (first (first sorted-pairs)) read))
(define link (second (first sorted-pairs)))
(define ctx (context (free-variables fpcore) 
                     (get-representation 'binary64) 
                     (make-list (length (free-variables fpcore)) 
                                (get-representation 'binary64))))

(define prog (fpcore->prog fpcore ctx))
(define spec (prog->spec prog))
(define (render-var-f64 var) (format "[~a <binary64>]" var))
(define (render-var-f32 var) (format "[~a <binary32>]" var))

(define operator-strf64 (format "(define-operation (~a.f64 ~a) <binary64> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary64 (~a ~a)) #:cost 1000)"
                            link
                            (string-join (map render-var-f64 (free-variables spec)))
                            spec
                            link
                            (string-join (map symbol->string (free-variables spec)))))

(define operator-strf32 (format "(define-operation (~a.f32 ~a) <binary32> #:spec ~a #:impl (from-rival) #:fpcore (! :precision binary32 (~a ~a)) #:cost 1000)"
                            link
                            (string-join (map render-var-f32 (free-variables spec)))
                            spec
                            link
                            (string-join (map symbol->string (free-variables spec)))))


(with-output-to-file "src/platforms/grow.rkt"
  (lambda ()
   (displayln operator-strf64)
   (displayln operator-strf32))
  #:exists 'append)

(with-output-to-file "reports/report_info.txt"
  (lambda ()
    (displayln (format "~a, ~a" link spec)))
  #:exists 'append)

(displayln (format "adding accelerator ~a, with spec: ~a" link spec))