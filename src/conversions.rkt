#lang racket

(require "syntax/types.rkt" "syntax/syntax.rkt" "errors.rkt")

(provide apply-repr-change-expr)

; Updates the repr of an expression if needed
(define (apply-repr-change-expr expr ctx)
  (let loop ([expr expr] [repr #f])
    (match expr
     [(list (? repr-conv? op) body)
      (define irepr (first (impl-info op 'itype)))
      (define orepr (impl-info op 'otype))
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
      (define irepr (first (impl-info op 'itype)))
      (define repr* (impl-info rr 'otype))
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
      (define irepr (impl-info op 'otype))
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
              (for/list ([arg args] [atype (impl-info op 'itype)])
                (if (equal? (representation-type atype) 'bool)
                    (loop2 arg)
                    (loop arg atype))))
            (cons op args*)])))
      (define ift* (loop ift repr*))
      (define iff* (loop iff repr*))
      (and ift* iff* `(if ,con* ,ift* ,iff*))]
     [(list (? operator? op) args ...) 
      (define orepr (impl-info op 'otype))
      (define repr* (or repr orepr))
      (if (equal? orepr repr*)
          (let ([args* (map loop args (impl-info op 'itype))])
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
     [(? literal?)
      (if repr
          (literal (literal-value expr) (representation-name repr))
          expr)]
     [_ expr])))

