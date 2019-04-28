#lang racket
(require racket/random)
(require "../src/syntax/syntax.rkt" (submod "../src/syntax/syntax.rkt" internals))
(require "../src/common.rkt" "../src/float.rkt")

(define (parse-range N)
  (if (string-contains? N "-")
      (match-let ([(list min max) (string-split N "-")])
        (cons (string->number min) (string->number max)))
      (cons (string->number N) (string->number N))))


(define (generate-fpcore i size nvars)
  (define name (format "Random test ~a" i))
  (define vars (take '(a b c d e f g h i j k l m n o p q r s t u v) nvars))
  (define expr (generate-expr vars size 'real))
  `(FPCore ,vars :name ,name ,expr))

(define (generate-expr vars fuel type)
  (cond
   [(<= fuel 1)
    (define valid-consts
      (for/list ([const (in-hash-keys (cdr constants))]
                 #:when (equal? (constant-info const 'type) type))
        const))
    (match type
      ['real
       (if (or (null? vars) (< (random) 0.2))
           (if (< (random) 0.1)
               (random-ref valid-consts)
               (sample-double))
           (random-ref vars))]
      [else
       (random-ref valid-consts)])]
   [(and (<= fuel 3) (equal? type 'complex) (< (random) 0.9))
    ;; Force the "complex" call to create good constants
    `(complex ,(generate-expr vars 1 'real) ,(generate-expr vars 1 'real))]
   [else
    (define valid-ops
      (for/list ([(op _) (in-hash (cdr operators))]
                 #:when true
                 [argnum (operator-info op 'args)]
                 #:when (< (match argnum ['* 2] [n n]) fuel)
                 #:when (equal? (last (car (hash-ref (operator-info op 'type) argnum))) type))
        (define atypes
          (match argnum
            ['*
             (define reptype (second (car (car (hash-ref (operator-info op 'type) '*)))))
             (list reptype reptype)]
            [n
             (car (car (hash-ref (operator-info op 'type) n)))]))
        (cons op atypes)))
    (match-define (cons op atypes) (random-ref valid-ops))
    (define subfuels (random-ref (filter (λ (x) (= (length x) (length atypes)))
                                         (all-partitions (- fuel 1)))))
    `(,op ,@(map (curry generate-expr vars) subfuels atypes))]))

(module+ main
  (define size (cons 1 1))
  (define vars (cons 1 1))
  (define tests 1)

  (command-line
   #:once-each
   [("--size") N "Size of expressions to generate (default 1)"
    (set! size (parse-range N))]
   [("--vars") N "Number of variables in generated expressions (default 1)"
    (set! vars (parse-range N))]
   [("--tests") N "Number of tests to generate (default 1)"
    (set! tests (string->number N))]
   #:args ()
   (for ([i (in-range tests)])
     (define s (random (car size) (+ 1 (cdr size))))
     (define v (random (car vars) (+ 1 (cdr vars))))
     (pretty-print (generate-fpcore i s v) (current-output-port) 1)
     (newline))))
