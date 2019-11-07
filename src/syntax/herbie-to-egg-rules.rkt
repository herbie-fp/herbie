#lang racket


(require "./rules.rkt")
(require "../syntax/herbie-to-egg-pattern.rkt")


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
          (read-once (list)))
