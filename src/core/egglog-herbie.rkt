#lang racket

(require "programs.rkt"
         "rules.rkt"
         "../syntax/matcher.rkt"
         "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../config.rkt"
         "../syntax/load-plugin.rkt"
         "../utils/timeline.rkt"
         "batch.rkt")

(provide prelude)

(module+ test
  (require rackunit)
  (require "../syntax/load-plugin.rkt")
  (load-herbie-builtins))

(define op-string-names
  (hash '+ 'Add '- 'Sub '* 'Mul '/ 'Div '== 'Eq '!= 'Neq '> 'Gt '< 'Lt '>= 'Gte '<= 'Lte))

(define (id->egglog)
  (make-hash))
(define (egglog->id)
  (make-hash))

;; [Copied from egg-herbie.rkt] Returns all representatations (and their types) in the current platform.
(define (all-repr-names [pform (*active-platform*)])
  (remove-duplicates (map (lambda (repr) (representation-name repr)) (platform-reprs pform))))

(define (prelude #:mixed-egraph? [mixed-egraph? #t])
  (load-herbie-builtins)
  (define pform (*active-platform*))
  (define spec-egraph
    `(datatype M
               (Num Rational :cost 4294967295)
               (Var String :cost 4294967295)
               (If M M M :cost 4294967295)
               ,@(platform-spec-nodes)
               ,@(platform-untyped-nodes pform)))
  (define typed-graph
    `(datatype MTy
               ,@(num-typed-nodes pform)
               ,@(var-typed-nodes pform)
               (IfTy MTy
                     MTy
                     MTy
                     :cost
                     ,(match (platform-impl-cost pform 'if)
                        [`(max ,n) n] ; Not quite right (copied from egg-herbie.rkt)
                        [`(sum ,n) n]))
               ,@(platform-typed-nodes pform)))
  (define proj-fn `(function typed-id (M String) MTy))
  (define impl-rules (impl-proj-rules pform))
  (define num-rules (num-proj-rules))
  (define if-rules (if-proj-rules))
  (printf "~s\n" spec-egraph)
  (printf "~s\n" typed-graph)
  (printf "~s\n" proj-fn)
  (for ([rule (in-list impl-rules)])
    (printf "~s\n" rule))
  (for ([rule (in-list num-rules)])
    (printf "~s\n" rule))
  (for ([rule (in-list if-rules)])
    (printf "~s\n" rule)))

(define (platform-spec-nodes)
  (for/list ([op (in-list (all-operators))])
    (define arity (length (operator-info op 'itype)))
    `(,(serialize-op op) ,@(for/list ([i (in-range arity)])
                             'M)
                         :cost
                         4294967295)))

(define (platform-untyped-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))]
             #:when (string-contains? (symbol->string impl) "."))
    (define arity (length (impl-info impl 'itype)))
    `(,(serialize-impl impl) ,@(for/list ([i (in-range arity)])
                                 'M)
                             :cost
                             4294967295)))

(define (platform-typed-nodes pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    `(,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
      ,@(for/list ([i (in-range arity)])
          'MTy)
      :cost
      ,(platform-impl-cost pform impl))))

(define (num-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(,(string->symbol (string-append "Num" (symbol->string repr)))
      Rational
      :cost
      ,(platform-repr-cost pform (get-representation repr)))))

(define (var-typed-nodes pform)
  (for/list ([repr (in-list (all-repr-names))])
    `(,(string->symbol (string-append "Var" (symbol->string repr)))
      String
      :cost
      ,(platform-repr-cost pform (get-representation repr)))))

(define (num-proj-rules)
  (for/list ([repr (in-list (all-repr-names))]
             #:when (not (eq? repr 'bool)))
    `(rule ((= e (Num n)))
           ((let tx ,(symbol->string repr)
              )
            (let etx (,(string->symbol (string-append "Num" (symbol->string repr)))
                      n)
              )
            (union (typed-id e tx) etx)))))

(define (if-proj-rules)
  (for/list ([repr (in-list (all-repr-names))])
    `(rule ((= e (If ifc ift iff)) (= tifc (typed-id ifc "bool"))
                                   (= tift (typed-id ift ,(symbol->string repr)))
                                   (= tiff (typed-id iff ,(symbol->string repr))))
           ((let t0 ,(symbol->string repr)
              )
            (let et0 (IfTy
                      tifc
                      tift
                      tiff)
              )
            (union (typed-id e t0) et0)))))

(define (impl-proj-rules pform)
  (for/list ([impl (in-list (platform-impls pform))])
    (define arity (length (impl-info impl 'itype)))
    `(rule ((= e (,(serialize-impl impl) ,@(impl-info impl 'vars)))
            ,@(for/list ([v (in-list (impl-info impl 'vars))]
                         [vt (in-list (impl-info impl 'itype))])
                `(= ,(string->symbol (string-append "t" (symbol->string v)))
                    (typed-id ,v ,(symbol->string (representation-name vt))))))
           ((let t0 ,(symbol->string (representation-name (impl-info impl 'otype)))
              )
            (let et0 (,(string->symbol (string-append (symbol->string (serialize-impl impl)) "Ty"))
                      ,@(for/list ([v (in-list (impl-info impl 'vars))])
                          (string->symbol (string-append "t" (symbol->string v)))))
              )
            (union (typed-id e t0) et0)))))

(define (serialize-op op)
  (if (hash-has-key? op-string-names op)
      (hash-ref op-string-names op)
      (string->symbol (string-titlecase (symbol->string op)))))

(define (serialize-impl impl)
  (define impl-split (string-split (symbol->string impl) "."))
  (define op (string->symbol (car impl-split)))
  (define type (if (= 2 (length impl-split)) (cadr impl-split) ""))
  (string->symbol (string-append (symbol->string (serialize-op op)) type)))
