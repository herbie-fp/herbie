#lang racket

(require "common.rkt" "interface.rkt" "syntax/rules.rkt" "syntax/syntax.rkt"
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide generate-conversions)

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (generate-conversion-1way iprec oprec)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define iprec* (string->symbol (string-replace* (~a iprec) replace-table))) ; fixed point workaround
  (define oprec* (string->symbol (string-replace* (~a oprec) replace-table)))
  (define conv (sym-append iprec* '-> oprec*))
  (define-values (irepr orepr)
    (with-handlers 
      ([exn:fail? (λ (e) (error 'generate-conversion-1way
                                "Invalid repr(s): ~a, ~a.\n" iprec oprec))])
        (values (get-representation iprec) (get-representation oprec))))

  (unless (hash-has-key? parametric-operators conv)
    (define impl (compose (representation-bf->repr orepr) (representation-repr->bf irepr)))
    (eprintf "~a not found, falling back to default implementation... \n" conv)
    (register-operator! conv conv (list iprec) oprec  ; fallback implementation
      (list (cons 'fl impl) (cons 'bf identity) (cons 'ival identity)
            (cons 'nonffi impl))))

  (define repr-rewrite (sym-append '<- iprec*))
  (unless (hash-has-key? parametric-operators repr-rewrite)
    (register-operator! repr-rewrite repr-rewrite (list iprec) iprec
      (list (cons 'fl identity) (cons 'bf identity) (cons 'ival identity)
            (cons 'nonffi identity))))

  (define rulename (sym-append 'rewrite '- oprec* '/ iprec*))
  (unless (ormap (λ (rs) (equal? (rule-name (caar rs)) rulename)) (*rulesets*))
    (register-ruleset! rulename '(arithmetic) (list (cons 'a oprec))
      (list
        (list rulename 'a  `(,conv (,repr-rewrite a)))))))

(define (generate-conversions convs)
  (define reprs
    (for/fold ([reprs '()]) ([conv convs])
      (define prec1 (first conv))
      (define prec2 (last conv))
      (generate-conversion-1way prec1 prec2)
      (generate-conversion-1way prec2 prec1)
      (set-union reprs (list (get-representation prec1) (get-representation prec2)))))
  (*needed-reprs* (set-union reprs (*needed-reprs*))))