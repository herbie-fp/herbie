#lang racket
(require (only-in xml write-xexpr xexpr?) (only-in fpbench fpcore? core->c))
(require "../common.rkt" "../formats/test.rkt" "tex.rkt")
(provide render-menu render-warnings render-large render-program program->fpcore)

(define (program->fpcore prog)
  (match-define (list _ args expr) prog)
  (list 'FPCore args expr))

(define/contract (render-menu sections links)
  (-> (listof (cons/c string? string?)) (listof (cons/c string? string?)) xexpr?)
  `(nav ([id "links"])
    (div
     ,@(for/list ([(text url) (in-dict links)])
         `(a ([href ,url]) ,text)))
    (div
     ,@(for/list ([(text url) (in-dict sections)])
         `(a ([href ,url]) ,text)))))

(define/contract (render-warnings warnings)
  (-> (listof (list/c symbol? string? (listof any/c) (or/c string? #f) (listof string?))) xexpr?)
  (if (null? warnings)
      ""
      `(ul ([class "warnings"])
           ,@(for/list ([warning warnings])
               (match-define (list type message args url extra) warning)
               `(li (h2 ,(apply format message args)
                        ,(if url `(a ([href ,url]) " (more)") ""))
                    ,(if (null? extra)
                         ""
                         `(ol ([class "extra"])
                              ,@(for/list ([line extra])
                                  `(li ,line)))))))))

(define (render-large #:title [title #f] name . values)
  `(div ,name ": " (span ([class "number"]
                          ,@(if title `([title ,title]) '()))
                         ,@values)))

(define languages
  `(("TeX" . ,texify-prog)
    ;; TODO(interface): currently program->c doesn't take the repr into account
    ("C" . ,(λ (prog repr) (core->c prog "code")))))

(define (render-program #:to [result #f] test)
  (define output-prec (test-output-prec test))

  (define versions
    (reap [sow]
      (for ([(lang converter) (in-dict languages)])
        (define in-prog (program->fpcore (test-program test)))
        (define out-prog (and result (program->fpcore result)))
        (when (and (fpcore? in-prog) (or (not out-prog) (fpcore? out-prog)))
          (sow (cons lang (cons (converter in-prog output-prec)
                                (and out-prog (converter out-prog output-prec)))))))))

  `(section ([id "program"])
     ,(if (equal? (test-precondition test) 'TRUE)
          ""
          `(div ([id "precondition"])
             (div ([class "program math"])
                  "\\[" ,(texify-expr (test-precondition test) output-prec) "\\]")))
     (select ([id "language"])
       (option "Math")
       ,@(for/list ([lang (in-dict-keys versions)])
           `(option ,lang)))
     (div ([class "implementation"] [data-language "Math"])
       (div ([class "program math"]) "\\[" ,(texify-prog
                                              (test-program test)
                                              output-prec) "\\]")
       ,@(if result
             `((div ([class "arrow"]) "↓")
               (div ([class "program math"]) "\\[" ,(texify-prog
                                                      result
                                                      output-prec) "\\]"))
             `()))
     ,@(for/list ([(lang outs) (in-dict versions)])
         (match-define (cons out-input out-output) outs)
         `(div ([class "implementation"] [data-language ,lang])
            (pre ([class "program"]) ,out-input)
            ,@(if out-output
                  `((div ([class "arrow"]) "↓")
                    (pre ([class "program"]) ,out-output))
                  `())))))
