#lang racket

(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")

(provide loop-aware-errors loop-aware-error-score)

(define (loop-aware-error-at-point prog pt)
  (let loop ([errs '()] [prog-fl prog] [prog-bf prog])
    (let ([prog-fl* (step-prog prog-fl pt mode:fl)]
          [prog-bf* (step-prog prog-bf pt mode:bf)])
      (if (not (and prog-fl* prog-bf*)) (reverse errs)
          (loop (cons (let ([approx (take-cur-val (program-body prog-fl*)
                                                  (program-variables prog-fl*)
                                                  pt mode:fl)]
                            [exact (->flonum (take-cur-val (program-body prog-bf*)
                                                           (program-variables prog-bf*)
                                                           pt mode:bf))])
                        (if (real? approx)
                            (add1 (abs (ulp-difference approx exact)))
                            (add1 (expt 2 (*bit-width*)))))
                      errs)
                prog-fl*
                prog-bf*)))))

(define (take-cur-val expr vars pt mode [bindings (hash)])
  (define (as-data val)
    (match val
      [`(list ,items ...) val]
      [`(,items ...) `(list . ,items)]
      [_ val]))
  (define (extend-bindings keys vals)
    (for/fold ([bindings* bindings])
        ([key keys] [val vals])
      (hash-set bindings key val)))
  (define ->precision (if (equal? mode mode:fl) ->flonum ->bf))
  (match expr
    [(? constant?) (->precision expr)]
    [(? variable?)
     (->precision (hash-ref bindings expr
                            (λ () (for/first ([var-name vars]
                                              [var-val pt]
                                              #:when (equal? var-name expr))
                                    var-val))))]
    [`(let ([,vars ,vals] ...) ,body)
     (take-cur-val body vars pt mode (extend-bindings vars vals))]
    [`(do ([,accs ,init-exprs ,_] ...)
          ,_
        ,ret-expr)
     (take-cur-val ret-expr vars pt mode
                    (extend-bindings accs (for/list ([expr init-exprs])
                                            (take-cur-val expr vars pt
                                                           mode bindings))))]
    [`(do-list ([,accs ,init-exprs ,_] ...)
               ([,items ,lsts] ...)
               ,ret-expr)
     (take-cur-val ret-expr vars pt mode
                    (extend-bindings accs (for/list ([expr init-exprs])
                                            (take-cur-val expr vars pt
                                                           mode bindings))))]
    [`(,fn ,args ...)
     (eval (cons (list-ref (hash-ref (*operations*) (car expr)) mode)
                 (for/list ([arg args])
                   (as-data (take-cur-val arg vars pt mode bindings))))
           common-eval-ns)]
    [_ (error "wat" expr)]))

(define (step-prog prog pt mode)
  ;; Returns the second list, except items that are false are replaced
  ;; with cooresponding items from the first list.
  (define (use-if-can-step vals vals*)
    (map (λ (a b) (or a b)) vals vals*))
  (define (inner-step expr [bindings (hash)])
    (define (inner-eval expr [bindings bindings])
      (when (not expr)
        (error "hey"))
      (take-cur-val expr (program-variables prog) pt mode bindings))
    (define (extend-bindings keys vals)
      (for/fold ([bindings* bindings])
          ([key keys] [val vals])
        (hash-set bindings key val)))
    (match expr
      [`(do ([,accs ,inits ,updates] ...)
            ,while-expr
          ,ret-expr)
       ;; First, if the inits can step, step as many of them can step
       ;; in parallel.
       (let ([inits* (for/list ([init inits]) (inner-step init bindings))])
         (if (ormap identity inits*)
             `(do ,(map list accs (use-if-can-step inits inits*) updates)
                  ,while-expr
                ,ret-expr)
             ;; If the inits are done, check if our condition is
             ;; false, and if so the loop is done, so try to step the
             ;; body, returning false if the body can't step, and
             ;; wrapping the stepped body in a let block that binds
             ;; the accs to their current inits if the body can step.
             (let ([condition-val (inner-eval while-expr (extend-bindings accs inits))])
               (if condition-val
                   (let ([inits** (for/list ([acc accs] [init inits] [update updates])
                                   (eval `(let ([,acc ,init])
                                            ,update)))])
                     `(do ,(map list accs inits** updates)
                          ,while-expr
                        ,ret-expr))
                   (let ([body* (inner-step ret-expr (extend-bindings accs inits))])
                     (and body* `(let ,(map list accs inits) body*)))))))]
      ;; See above for comments
      [`(do-list ([,accs ,inits ,updates] ...)
                 ([,items ,lsts] ...)
                 ,ret-expr)
       (let ([inits* (for/list ([init inits]) (inner-step init bindings))]
             [lsts* (for/list ([lst lsts]) (inner-step lst bindings))])
         (if (or (ormap identity inits*) (ormap identity lsts*))
             `(do-list ,(map list accs (use-if-can-step inits inits*) updates)
                       ,(map list items (use-if-can-step lsts lsts*))
                       ,ret-expr)
             (if (for/or ([lst lsts]) (null? (inner-eval lst)))
                 (let ([body* (inner-step ret-expr (extend-bindings accs inits))])
                   (and body* `(let ,(map list accs inits) body*)))
                 (let ([inits* (for/list ([acc accs] [init inits] [update updates])
                                 (inner-eval `(let ([,acc ,init])
                                                (let ,(for/list ([item items] [lst lsts])
                                                        (list item (inner-eval `(car ,lst))))
                                                  ,update))))]
                       [lsts* (for/list ([lst lsts])
                                `(cdr ,lst))])
                   `(do-list ,(map list accs inits* updates)
                             ,(map list items lsts*)
                             ,ret-expr)))))]
      [`(let ([,vars ,vals] ...) ,body)
       (let ([vals* (for/list ([val vals]) (inner-step val bindings))])
         (if (ormap identity vals*)
             `(let ,(map list vars (use-if-can-step vals vals*)) ,body)
             (let ([body* (inner-step body (extend-bindings vars vals))])
               (and body* `(let ,(map list vars vals) ,body*)))))]
      [`(,fn ,args ...)
       (let ([args* (for/list ([arg args]) (inner-step arg bindings))])
         (if (andmap not args*) #f
             (cons fn (use-if-can-step args args*))))]
      [_ #f]))
  (let ([body* (inner-step (program-body prog))])
    (and body* `(λ ,(program-variables prog) ,body*))))
             
                   
(define (loop-aware-errors prog)
  (for/list ([(p e) (in-pcontext (*pcontext*))])
    (loop-aware-error-at-point prog p)))

;; attempts to compensate for the random walk behavior of error
;; growth.
(define (loop-aware-error-score prog)
  (define (make-pt err-lst)
    (for/list ([(err i) (in-indexed err-lst)])
      (list i (sqr err))))
  (let ([err-lsts (loop-aware-errors prog)])
    (exact->inexact
     (* (apply max (map length err-lsts))
        (best-fit-slope
         (apply
          append
          (map make-pt err-lsts)))))))

(define-syntax-rule (for/avg ([items lsts]...)
                             body)
  (/ (for/sum ([items lsts] ...)
       body)
     (apply min (map length (list lsts ...)))))

;; got this from the internet
(define (best-fit-slope pts)
  (/ (- (for/avg ([pt pts])
                 (* (car pt) (cadr pt)))
        (* (for/avg ([pt pts])
                    (car pt))
           (for/avg ([pt pts])
                    (cadr pt))))
     (- (for/avg ([pt pts])
                 (sqr (car pt)))
        (sqr (for/avg ([pt pts])
                      (car pt))))))
