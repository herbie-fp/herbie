#lang racket

(require "common.rkt" "interface.rkt" "errors.rkt"
         "syntax/rules.rkt" "syntax/syntax.rkt"
         (submod "syntax/rules.rkt" internals) (submod "syntax/syntax.rkt" internals))
(provide generate-conversions generate-prec-rewrites get-rewrite-operator *conversions*)

(define *conversions* (make-parameter (hash)))

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (get-rewrite-operator prec)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define prec* (string->symbol (string-replace* (~a prec) replace-table)))
  (define rewrite (sym-append '<- prec*))
  (get-parametric-operator rewrite prec))

;; Generates conversion, repr-rewrite operators for prec1 and prec2
(define (generate-conversion-ops prec1 prec2)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define prec1* (string->symbol (string-replace* (~a prec1) replace-table))) ; fixed point workaround
  (define prec2* (string->symbol (string-replace* (~a prec2) replace-table)))
  (define-values (repr1 repr2) (values (get-representation prec1) (get-representation prec2)))

  ;; Repr conversions, e.g. repr1->repr2
  (define conv1 (sym-append prec1* '-> prec2*))
  (define conv2 (sym-append prec2* '-> prec1*))

  (unless (hash-has-key? parametric-operators-reverse conv1)
    (define impl (compose (representation-bf->repr repr2) (representation-repr->bf repr1)))
    (register-operator-impl! 'cast conv1 (list prec1) prec2  ; fallback implementation
      (list (cons 'fl impl))))
  
  (unless (hash-has-key? parametric-operators-reverse conv2)
    (define impl (compose (representation-bf->repr repr1) (representation-repr->bf repr2)))
    (register-operator-impl! 'cast conv2 (list prec2) prec1  ; fallback implementation
      (list (cons 'fl impl))))

  ;; Repr rewrites, e.g. <-repr
  (define repr-rewrite1 (sym-append '<- prec1*))
  (define repr-rewrite2 (sym-append '<- prec2*))

  (unless (hash-has-key? parametric-operators repr-rewrite1)
    (register-operator! repr-rewrite1 (list 'real) 'real
      (list (cons 'bf identity) (cons 'ival identity)))
    (register-operator-impl! repr-rewrite1 repr-rewrite1 (list prec1) prec1
      (list (cons 'fl identity))))

  (unless (hash-has-key? parametric-operators repr-rewrite2)
    (register-operator! repr-rewrite2 (list 'real) 'real
      (list (cons 'bf identity) (cons 'ival identity)))
    (register-operator-impl! repr-rewrite2 repr-rewrite2 (list prec2) prec2
      (list (cons 'fl identity)))))

;; creates precision rewrite: prec1 <==> prec2
;; assumes generate-conversion-ops has been invoked for these precisions
(define (generate-prec-rewrite prec1 prec2)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (define prec1* (string->symbol (string-replace* (~a prec1) replace-table))) ; fixed point workaround
  (define prec2* (string->symbol (string-replace* (~a prec2) replace-table)))

  ;; Repr conversions, e.g. repr1->repr2
  (define conv1 (sym-append prec1* '-> prec2*))
  (define conv2 (sym-append prec2* '-> prec1*))

  ;; Repr rewrites, e.g. <-repr
  (define repr-rewrite1 (sym-append '<- prec1*))
  (define repr-rewrite2 (sym-append '<- prec2*))

  ;; if missing, try generating them
  (unless (and (hash-has-key? parametric-operators-reverse conv1)
               (hash-has-key? parametric-operators-reverse conv2)
               (hash-has-key? parametric-operators repr-rewrite1)
               (hash-has-key? parametric-operators repr-rewrite2))
    (generate-conversion-ops prec1 prec2))

  ;; Repr rewrite/conversion rules
  (define rulename1 (sym-append 'rewrite '- prec2* '/ prec1*))
  (define rulename2 (sym-append 'rewrite '- prec1* '/ prec2*))
  (define rulename3 (sym-append rulename1 '-simplify))
  (define rulename4 (sym-append rulename2 '-simplify))

  (register-ruleset! rulename1 '(arithmetic) (list (cons 'a prec2))
    (list (list rulename1 'a `(,conv1 (,repr-rewrite1 a)))))

  (register-ruleset! rulename2 '(arithmetic) (list (cons 'a prec1))
    (list (list rulename2 'a `(,conv2 (,repr-rewrite2 a)))))

  (register-ruleset! rulename3 '(arithmetic simplify) (list (cons 'a prec1))
    (list (list rulename3 `(,conv2 (,conv1 a)) 'a)))

  (register-ruleset! rulename4 '(arithmetic simplify) (list (cons 'a prec2))
    (list (list rulename4 `(,conv1 (,conv2 a)) 'a))))

;; generate conversions, precision rewrites, etc.
(define (generate-prec-rewrites convs)
  (define reprs
    (for/fold ([reprs '()]) ([conv convs])
      (define prec1 (first conv))
      (define prec2 (last conv))
      (*conversions* (hash-update (*conversions*) prec1 (λ (x) (cons prec2 x)) '()))
      (*conversions* (hash-update (*conversions*) prec2 (λ (x) (cons prec1 x)) '()))
      (generate-prec-rewrite prec1 prec2)
      (set-union reprs (list (get-representation prec1) (get-representation prec2)))))
  (*needed-reprs* (set-union reprs (*needed-reprs*))))

;; invoked before desugaring
(define (generate-conversions convs)
  (define convs* (mutable-set))
  (for ([conv convs])
    (let ([prec1 (first conv)] [prec2 (last conv)])
      (generate-conversion-ops prec1 prec2)
      (when (set-member? convs* (cons prec1 prec2))
        (warn 'conversions "Duplicate conversion (~a ~a)\n" prec1 prec2))
      (set-add! convs* (cons prec1 prec2)))))


;; try built in reprs
(module+ test
  (define convs (list (list 'binary64 'binary32)))
  (generate-conversions convs)
  (generate-prec-rewrites convs))
