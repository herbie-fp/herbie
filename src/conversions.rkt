#lang racket

(require (submod "syntax/rules.rkt" internals)
         (submod "syntax/syntax.rkt" internals)
         "common.rkt" "syntax/types.rkt" "errors.rkt"
         "syntax/syntax.rkt")

(provide generate-conversions generate-prec-rewrites *conversions* apply-repr-change-expr)

(define *conversions* (make-parameter (hash)))

(define/contract (string-replace* str changes)
  (-> string? (listof (cons/c string? string?)) string?)
  (let loop ([str str] [changes changes])
    (match changes
      [(? null?) str]
      [_ (let ([change (car changes)])
           (loop (string-replace str (car change) (cdr change)) (cdr changes)))])))

(define (repr->symbol repr)
  (define replace-table `((" " . "_") ("(" . "") (")" . "")))
  (string->symbol (string-replace* (~a (representation-name repr)) replace-table)))

;; Generates conversion, repr-rewrite operators for prec1 and prec2
(define (generate-conversion-ops repr1 repr2)
  (define prec1* (repr->symbol repr1))
  (define prec2* (repr->symbol repr2))

  ;; Repr conversions, e.g. repr1->repr2
  (define conv1 (sym-append prec1* '-> prec2*))
  (define conv2 (sym-append prec2* '-> prec1*))

  ;; Try generating a user-defined implementation
  (generate-conversion-impl! conv1 conv2 repr1 repr2)

  (unless (impl-exists? conv1)
    (define impl (compose (representation-bf->repr repr2) (representation-repr->bf repr1)))
    (register-operator-impl! 'cast conv1 (list repr1) repr2  ; fallback implementation
      (list (cons 'fl impl))))
  
  (unless (impl-exists? conv2)
    (define impl (compose (representation-bf->repr repr1) (representation-repr->bf repr2)))
    (register-operator-impl! 'cast conv2 (list repr2) repr1  ; fallback implementation
      (list (cons 'fl impl))))

  ;; Repr rewrites, e.g. <-repr
  (define repr-rewrite1 (sym-append '<- prec1*))
  (define repr-rewrite2 (sym-append '<- prec2*))

  (unless (impl-exists? repr-rewrite1)
    (register-operator-impl! 'convert repr-rewrite1 (list repr1) repr1
      (list (cons 'fl identity))))

  (unless (impl-exists? repr-rewrite2)
    (register-operator-impl! 'convert repr-rewrite2 (list repr2) repr2
      (list (cons 'fl identity)))))

;; creates precision rewrite: prec1 <==> prec2
;; assumes generate-conversion-ops has been invoked for these precisions
(define (generate-prec-rewrite repr1 repr2)
  (define prec1* (repr->symbol repr1)) ; fixed point workaround
  (define prec2* (repr->symbol repr2))

  ;; Repr conversions, e.g. repr1->repr2
  (define conv1 (sym-append prec1* '-> prec2*))
  (define conv2 (sym-append prec2* '-> prec1*))

  ;; Repr rewrites, e.g. <-repr
  (define repr-rewrite1 (sym-append '<- prec1*))
  (define repr-rewrite2 (sym-append '<- prec2*))

  ;; if missing, try generating them
  (unless (and (impl-exists? conv1)
               (impl-exists? conv2)
               (operator-exists? repr-rewrite1)
               (operator-exists? repr-rewrite2))
    (generate-conversion-ops repr1 repr2))

  ;; Repr rewrite/conversion rules
  (define rulename1 (sym-append 'rewrite '- prec2* '/ prec1*))
  (define rulename2 (sym-append 'rewrite '- prec1* '/ prec2*))
  (define rulename3 (sym-append rulename1 '-simplify))
  (define rulename4 (sym-append rulename2 '-simplify))

  (register-ruleset! rulename1 '(arithmetic) (list (cons 'a repr2))
    (list (list rulename1 'a `(,conv1 (,repr-rewrite1 a)))))

  (register-ruleset! rulename2 '(arithmetic) (list (cons 'a repr1))
    (list (list rulename2 'a `(,conv2 (,repr-rewrite2 a)))))

  (register-ruleset! rulename3 '(arithmetic simplify) (list (cons 'a repr1))
    (list (list rulename3 `(,conv2 (,conv1 a)) 'a)))

  (register-ruleset! rulename4 '(arithmetic simplify) (list (cons 'a repr2))
    (list (list rulename4 `(,conv1 (,conv2 a)) 'a))))

;; generate conversions, precision rewrites, etc.
(define (generate-prec-rewrites convs)
  (for ([conv convs])
    (define repr1 (first conv))
    (define repr2 (last conv))
    (*conversions* (hash-update (*conversions*) repr1 (curry cons repr2) '()))
    (*conversions* (hash-update (*conversions*) repr2 (curry cons repr1) '()))
    (generate-prec-rewrite repr1 repr2)
    (*needed-reprs* (set-union (*needed-reprs*) (list repr1 repr2)))))

;; invoked before desugaring
(define (generate-conversions convs)
  (define convs* (mutable-set))
  (for ([conv convs])
    (define repr1 (first conv))
    (define repr2 (last conv))
    (generate-conversion-ops repr1 repr2)
    (when (set-member? convs* (cons repr1 repr2))
      (warn 'conversions "Duplicate conversion (~a ~a)\n"
            (representation-name repr1) (representation-name repr2)))
    (set-add! convs* (cons repr1 repr2))))

;; try built in reprs
(module+ test
  (require "load-plugin.rkt")
  (load-herbie-plugins)
  
  (define convs (list (map get-representation '(binary64 binary32))))
  (generate-conversions convs)
  (generate-prec-rewrites convs))

; Updates the repr of an expression if needed
(define (apply-repr-change-expr expr ctx)
  (let loop ([expr expr] [repr #f])
    (match expr
     [(list (? repr-conv? op) body)
      (define irepr (first (operator-info op 'itype)))
      (define orepr (operator-info op 'otype))
      (define repr* (or repr orepr))
      (define body* (loop body irepr))
      (cond
       [(not body*) #f] ; propagate failed repr-change
       [else
        (define new-conv (get-repr-conv irepr repr*)) ; try to find a single conversion
        (if new-conv
            (list new-conv body*)
            (let ([second-conv (get-repr-conv orepr repr*)]) ; try a two-step conversion
              (and second-conv (list second-conv (list op body*)))))])]
     [(list (? rewrite-repr-op? rr) (list (? repr-conv? op) body))  ; repr change on a conversion
      (define irepr (first (operator-info op 'itype)))
      (define repr* (operator-info rr 'otype))
      (if (equal? repr* irepr)
          (if repr
              (loop body irepr) ; if the conversions are inverses and not the top
              (list op (loop body irepr)))
          (if repr
              (loop (list op body) repr*)
              (let* ([conv (get-repr-conv repr* (context-repr ctx))]
                     [body* (loop body repr*)])
                (and conv body* (list conv body*)))))]
     [(list (? rewrite-repr-op? op) body)
      (define irepr (operator-info op 'otype))
      (define orepr (or repr (context-repr ctx)))
      (cond
       [(equal? irepr orepr)
        (loop body irepr)]
       [else
        (define conv (get-repr-conv irepr orepr))
        (define body* (loop body irepr))
        (and conv body* (list conv body*))])]
     [(list 'if con ift iff)
      (define repr* (or repr (context-repr ctx)))
      (define con*
        (let loop2 ([con con])
          (cond
           [(set-member? '((TRUE) (FALSE)) con)
            con]
           [else
            (match-define (list op args ...) con)
            (define args*
              (for/list ([arg args] [atype (operator-info op 'itype)])
                (if (equal? (representation-type atype) 'bool)
                    (loop2 arg)
                    (loop arg atype))))
            (cons op args*)])))
      (define ift* (loop ift repr*))
      (define iff* (loop iff repr*))
      (and ift* iff* `(if ,con* ,ift* ,iff*))]
     [(list (? operator? op) args ...) 
      (define orepr (operator-info op 'otype))
      (define repr* (or repr orepr))
      (if (equal? orepr repr*)
          (let ([args* (map loop args (operator-info op 'itype))])
            (and (andmap identity args*) (cons op args*)))
          (with-handlers ([exn:fail:user:herbie:missing? (const #f)])
            (let ([op* (apply get-parametric-operator
                            (impl->operator op)
                            (make-list (length args) repr*))]
                  [args* (map (curryr loop repr*) args)])
            (and (andmap identity args*) (cons op* args*)))))]
     [(? variable?)
      (define var-repr (context-lookup ctx expr))
      (cond
       [(equal? var-repr repr) expr]
       [else ; insert a cast if the variable precision is not the same
        (define cast (get-repr-conv var-repr repr))
        (and cast (list cast expr))])]
     [_ expr])))

