#lang racket
(require "./rules.rkt")
(provide herbie-pattern->rust-pattern constant?)

(module+ test (require rackunit))

(define constants
  '(E LOG2E LOG10E LN2 LN10
      PI PI_2 PI_4 1_PI 2_PI 2_SQRTPI
      SQRT2 SQRT1_2 MAXFLOAT HUGE_VAL
      TRUE FALSE))

(define (constant? x)
  (set-member? constants x))

(define (herbie-pattern->rust-pattern datum)
  (cond
    [(list? datum)
     (string-join
      (cons
       (symbol->string (first datum))
       (map (lambda (sub-expr) (herbie-pattern->rust-pattern sub-expr))
            (rest datum)))
      " "
      #:before-first "("
      #:after-last ")")]
    [(symbol? datum)
     (format (if (constant? datum) "~a" "?~a") datum)]
    [(number? datum)
     (number->string datum)]
    [else
     (error "expected list, number, or symbol")]))

(module+ test
  (check-equal? (herbie-pattern->rust-pattern `(+ a b)) "(+ ?a ?b)")
  (check-equal? (herbie-pattern->rust-pattern `(/ c (- 2 a)) "(/ ?c (- 2 ?a))")))

;; use as a command line tool  to generate rules in the egg rust syntax
;; this is good for direct use on the rust side (testing)
(module+ main
  (define (rule-syntax->rust-string stx)
    (define top (syntax->list stx))
    (define name (symbol->string (syntax->datum (second top))))

    (define groups
      (for/list ([rule-stx (syntax->list (third top))])
        (syntax->datum rule-stx)))
    (define is-groups-valid
      (and (member 'simplify groups) (not (member 'numerics groups))))
    
    (cond
      [(not is-groups-valid)
       ""]
      [else
       (define rules
         (rest (rest (rest (rest (rest top))))))
       (define rule-strings
         (for/list ([rule rules])
           (define rule-list (syntax->list rule))
           (define rule-name (symbol->string (syntax->datum (first rule-list))))
           (define first-expr (herbie-pattern->rust-pattern (syntax->datum (second rule-list))))
           (define second-expr (herbie-pattern->rust-pattern (syntax->datum (third rule-list))))
           (string-append "(\"" rule-name "\",\"" first-expr "\",\"" second-expr "\")")))
       (define rules-together
         (foldr (lambda (next acc)
                  (string-append next "," acc))
                ""
                rule-strings))
       
       (string-append "add("
                      "\"" name "\","
                      "&[" rules-together "],);")]))

  (define port (open-input-file "./rules.rkt"))
  (define (read-once rust-list)
    (define stx (read-syntax "./testrules.rkt" port))
    (if
     (eof-object? stx)
     rust-list
     (syntax-case stx (define-ruleset)
       [(define-ruleset name groups [rname input output] ...)
        (read-once (cons (rule-syntax->rust-string
                          #'(define-ruleset name groups #:type () [rname input output] ...)) rust-list))]
       [(define-ruleset name groups #:type types [rname input output] ...)
        (read-once (cons (rule-syntax->rust-string stx)
                         rust-list))]
       [else (read-once rust-list)])))


  (define output (open-output-file "./rustgenerated.txt" #:exists 'replace))
  (read-line port) ; read the #lang so that it doesn't appear
  (for-each (lambda (rust-str)
              (display rust-str output))
            (read-once (list))))
