#lang racket

(require "common.rkt")
(require "points.rkt")
(require "programs.rkt")

(provide loop-aware-errors loop-aware-error-score
         loop-aware-error-score*)
(provide (all-defined-out))

(define (loop-aware-error-at-point prog pt)
  (define (take-error prog-approx prog-exact)
    (let ([approx (take-cur-val (program-body prog-approx)
				(program-variables prog-approx)
				pt mode:fl)]
	  [exact (->flonum (take-cur-val (program-body prog-exact)
					 (program-variables prog-approx)
					 pt mode:bf))])
      (if (real? approx)
	  (add1 (abs (ulp-difference approx exact)))
	  (add1 (expt 2 (*bit-width*))))))
  (let loop ([errs '()] [prog-fl prog] [prog-bf prog])
    (let ([prog-fl* (step-prog prog-fl pt mode:fl)]
          [prog-bf* (step-prog prog-bf pt mode:bf)])
      (if (not (and prog-fl* prog-bf*)) (reverse errs)
          (loop (cons (take-error prog-fl* prog-bf*) errs)
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
      (hash-set bindings* key val)))
  (define ->precision (if (equal? mode mode:fl) ->flonum ->bf))
  (match expr
    [(? constant?) (->precision expr)]
    [(? variable?)
     (->precision (hash-ref bindings expr
                            (λ () (for/first ([var-name vars]
                                              [var-val pt]
                                              #:when (equal? var-name expr))
                                    var-val))))]
    [`(let ([,names ,vals] ...) ,body)
     (take-cur-val body vars pt mode (extend-bindings names (for/list ([val vals])
							      (take-cur-val val vars pt
									    mode bindings))))]
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
     (with-handlers ([exn:fail? (λ _ (error "can't evaluate" expr "with bindings" bindings
					    "and variables" (map list vars pt)))])
       (eval (cons (list-ref (hash-ref (*operations*) (car expr)) mode)
		   (for/list ([arg args])
		     (as-data (take-cur-val arg vars pt mode bindings))))
           common-eval-ns))]
    [_ (error "wat" expr)]))

(define (step-prog prog pt mode)
  ;; Returns the second list, except items that are false are replaced
  ;; with cooresponding items from the first list.
  (define (use-if-can-step vals vals*)
    (map (λ (a b) (or a b)) vals vals*))
  (define (inner-step expr [bindings (hash)])
    (define (inner-eval expr [bindings bindings])
      (take-cur-val expr (program-variables prog) pt mode bindings))
    (define (extend-bindings keys vals)
      (for/fold ([bindings* bindings])
          ([key keys] [val vals])
        (hash-set bindings* key val)))
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
             (let* ([init-vals (for/list ([init inits])
				 (inner-eval init))]
		    [condition-val (inner-eval while-expr (extend-bindings accs init-vals))])
               (if condition-val
                   (let ([inits** (for/list ([update updates])
                                    (inner-eval update
                                                (extend-bindings accs init-vals)))])
                     `(do ,(map list accs inits** updates)
                          ,while-expr
                        ,ret-expr))
                   (let ([body* (inner-step ret-expr (extend-bindings accs init-vals))])
                     (and body* `(let ,(map list accs inits) ,body*)))))))]
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
	     (let ([init-vals (for/list ([init inits])
				(inner-eval init))])
	       (if (for/or ([lst lsts]) (null? (inner-eval lst)))
		   (let ([body* (inner-step ret-expr (extend-bindings accs init-vals))])
		     (and body* `(let ,(map list accs inits) ,body*)))
		   (let ([inits** (for/list ([update updates])
				    (inner-eval update (extend-bindings (append accs items)
									(append init-vals
										(for/list ([lst lsts])
										  (inner-eval `(car ,lst)))))))]
			 [lsts* (for/list ([lst lsts])
				  `(cdr ,lst))])
		     `(do-list ,(map list accs inits** updates)
			       ,(map list items lsts*)
			       ,ret-expr))))))]
      [`(let ([,vars ,vals] ...) ,body)
       (let ([vals* (for/list ([val vals]) (inner-step val bindings))])
         (if (ormap identity vals*)
             `(let ,(map list vars (use-if-can-step vals vals*)) ,body)
             (let ([body* (inner-step body (extend-bindings vars (for/list ([val vals])
								   (inner-eval val))))])
               (and body* `(let ,(map list vars vals) ,body*)))))]
      [`(,fn ,args ...)
       (let ([args* (for/list ([arg args]) (inner-step arg bindings))])
         (if (andmap not args*) #f
             (cons fn (use-if-can-step args args*))))]
      [_ #f]))
  (let ([body* (inner-step (program-body prog))])
    (and body* `(λ ,(program-variables prog) ,body*))))
             
                   
(define (loop-aware-errors prog context)
  (for/list ([(p e) (in-pcontext context)])
    (loop-aware-error-at-point prog p)))

;; attempts to compensate for the random walk behavior of error
;; growth.
(define (loop-aware-error-score errs)
  (define (make-pts err-lst)
    (for/list ([(err i) (in-indexed err-lst)])
      (list i (sqr err))))
  (exact->inexact
   (/ (for/sum ([err-lst errs])
	(best-fit-slope (make-pts err-lst)))
      (length errs))))

(define (loop-aware-error-score* errs)
  (define bad-ulps (expt 2 (/ (*bit-width*) 2)))
  (define (make-pts err-lst)
    (for/list ([(err i) (in-indexed err-lst)])
      (list i (sqr err))))
  (exact->inexact
   (/
    (for/sum ([err-lst errs])
      (let-values ([(decent-pts horrible-pts)
                    (partition (λ (pt)
                                 (< (cadr pt) (sqr bad-ulps)))
                               (make-pts err-lst))])
        (+ (* (if (null? err-lst) 0 (/ (length horrible-pts) (length err-lst))) (*bad-pts-cost*))
           (best-fit-slope decent-pts))))
    (length errs))))
