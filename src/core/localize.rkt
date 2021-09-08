#lang racket

(require math/bigfloat)
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt" "../interface.rkt")

(provide localize-error get-locations)

(define *analyze-cache* (make-hash))
(define *analyze-context* (make-parameter #f))

(register-reset
 (λ ()
  (*analyze-context* (*pcontext*))
  (set! *analyze-cache* (make-hash))))

(define (repeat c)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    c))

(define (localize-on-expression expr vars cache repr)
  (hash-ref! cache expr
             (λ ()
                (match expr
                  [(? number?)
                   (cons (repeat (bf expr)) (repeat 1))]
                  [(? variable?)
                   (cons (map (curryr representation-repr->bf (dict-ref (*var-reprs*) expr))
                              (dict-ref vars expr))
                         (repeat 1))]
                  [`(if ,c ,ift ,iff)
                   (let ([exact-ift (car (localize-on-expression ift vars cache repr))]
                         [exact-iff (car (localize-on-expression iff vars cache repr))]
                         [exact-cond (for/list ([(p _) (in-pcontext (*pcontext*))])
				       (apply (eval-prog `(λ ,(map car vars) ,c) 'bf repr) p))])
                     (cons (for/list ([c exact-cond] [t exact-ift] [f exact-iff]) (if c t f))
                           (repeat 1)))]
                  [`(,f)
                   (define repr (get-representation (operator-info f 'otype)))
                   (define <-bf (representation-bf->repr repr))
                   (define exact ((operator-info f 'bf)))
                   (define approx ((operator-info f 'fl)))
                   (cons (repeat exact) (repeat (ulp-difference (<-bf exact) approx repr)))]
                  [`(,f ,args ...)
                   (define repr (get-representation (operator-info f 'otype)))
                   (define argreprs (map get-representation (operator-info f 'itype)))
                   (define <-bf (representation-bf->repr repr))
                   (define arg<-bfs (map representation-bf->repr argreprs))

                   (define argexacts
                     (flip-lists
                      (for/list ([arg args] [repr argreprs])
                        (car (localize-on-expression arg vars cache repr)))))
                   (define argapprox
                     (for/list ([pt argexacts])
                       (for/list ([val pt] [arg<-bf arg<-bfs])
                         (arg<-bf val))))

                   (define exact (map (curry apply (operator-info f 'bf)) argexacts))
                   (define approx (map (curry apply (operator-info f 'fl)) argapprox))
                   (cons exact (map (λ (ex ap) (ulp-difference (<-bf ex) ap repr))
                                    exact approx))]))))

(register-reset
 (λ ()
  (*analyze-context* (*pcontext*))
  (set! *analyze-cache* (make-hash))))

;; Returns the locations of `subexpr` within `expr`
(define (get-locations expr subexpr)
  (let loop ([expr expr] [loc '()])
    (match expr
      [(== subexpr)
       (list (reverse loc))]
      [(list op args ...)
       (apply
        append
        (for/list ([arg (in-list args)] [i (in-naturals 1)])
          (loop arg (cons i loc))))]
      [_
       (list)])))

;; Returns a list of locations and errors sorted
;; by error scores in descending order
(define (localize-error prog repr)
  (define varmap (map cons (program-variables prog)
		      (flip-lists (for/list ([(p e) (in-pcontext (*pcontext*))])
				    p))))
  (define cache
    (if (eq? (*analyze-context*) (*pcontext*))
        *analyze-cache*
        (make-hash)))

  (localize-on-expression (program-body prog) varmap cache repr)
  (sort
    (reap [sow]
      (for ([(expr ex&err) (in-hash cache)])
        (define err (cdr ex&err))
        (unless (andmap (curry = 1) err)
          (sow (cons err expr)))))
    > #:key (compose errors-score car)))
