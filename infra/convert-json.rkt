#lang racket

(require json)
(require "../src/programs.rkt")

(define (fix-expr expr pre-fpcore?)
  (let loop ([expr expr])
    (match* (pre-fpcore? expr)
      [(_ (? string?))
       (or (string->number expr) (error 'fix-expr "string that is not a num: ~a" expr))]
      [(#t (list 'expt (app loop a) (app loop b))) (list 'pow a b)]
      [(#t (list 'cube (app loop a))) (list 'pow a 3)]
      [(#t (list 'cot (app loop a))) (list '/ 1 (list 'tan a))]
      [(#t (list 'sqr (app loop a))) (list '* a a)]
      [(#t (list 'abs (app loop a))) (list 'fabs a)]
      [(#t (list 'mod (app loop a))) (list 'fmod a)]
      [(#t 'e) 'E]
      [(#t 'pi) 'PI]
      [(_ (list op (app loop args) ...)) (list* op args)]
      [(_ _) expr])))

(define (make-fpcore test pre-fpcore?)
  (define expr (fix-expr (call-with-input-string (hash-ref test 'input) read) pre-fpcore?))
  (define vars (map string->symbol (hash-ref test 'vars (λ () (map ~a (free-variables expr))))))
  (define spec (fix-expr (call-with-input-string (hash-ref test 'spec (~s expr)) read) pre-fpcore?))
  (define pre (fix-expr (call-with-input-string (hash-ref test 'pre "TRUE") read) pre-fpcore?))
  `(FPCore ,vars
    ,@(if (hash-has-key? test 'name) (list ':name (hash-ref test 'name)) '())
    ,@(if (not (equal? pre "TRUE")) (list ':pre pre) '())
    ,@(if (not (equal? spec expr)) (list ':spec spec) '())
    ,@(if (hash-has-key? test 'prec) (list ':precision (string->symbol (hash-ref test 'prec))) '())
    ,expr))

(define (convert-files json-files pre-fpcore?)
  (define seen (mutable-set))
  (for ([json-file (in-list json-files)])
    (define data (call-with-input-file json-file read-json))
    (for ([test (hash-ref data 'tests)])
      (define expr (make-fpcore test pre-fpcore?))
      (unless (set-member? seen expr)
        (set-add! seen expr)
        (pretty-print expr (current-output-port) 1)
        (newline)))))

(module+ main
  (define pre-fpcore? #f)
  (command-line 
   #:program "convert-json"
   #:once-each
   [("--pre-fpcore") "The demo file dates from before Herbie 1.0"
    (set! pre-fpcore? #t)]
   #:args json-files
   (convert-files json-files pre-fpcore?)))

