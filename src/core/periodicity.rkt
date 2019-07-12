#lang racket

;; Periodicity analysis
;
; Given a mathematical formula, determines which components are periodic,
; which variables they are periodic in, and the associated periods.
;
; The basic process is abstract interpretation, where each subexpression
; is classified, for every variable, as one of:
;  - Constant (and the value)
;  - Linear (and the coefficient)
;  - Periodic (and the period)
;  - Other
;
; Known periodic functions, like sin or tan, transform linear
; expressions into periodic expressions. Periods are then properly
; bubbled up the expression tree.

(require racket/match)
(require "../common.rkt")
(require "../programs.rkt")
(require "../alternative.rkt")
(require "../points.rkt")
(require "../syntax/rules.rkt")
(require "../float.rkt")
(require "matcher.rkt")

(struct annotation (expr loc type coeffs) #:transparent)
(struct lp (loc periods) #:prefab)

(provide optimize-periodicity (struct-out lp))

(define (constant-value? a) (eq? (annotation-type a) 'constant))
(define (linear? a)   (eq? (annotation-type a) 'linear))
(define (periodic? a) (or (eq? (annotation-type a) 'periodic) (interesting? a)))
(define (interesting? a) (eq? (annotation-type a) 'interesting))
(define (other? a) (eq? (annotation-type a) 'other))
(define coeffs annotation-coeffs)

(define (alist-merge merge . as)
  (define (merge-2 a b)
    (cond
     [(null? a) b]
     [(null? b) a]
     [(eq? (caar a) (caar b))
      (cons (cons (caar a) (merge (cdar a) (cdar b)))
            (merge-2 (cdr a) (cdr b)))]
     [(symbol<? (caar a) (caar b))
      (cons (car a) (merge-2 (cdr a) b))]
     [(symbol<? (caar b) (caar a))
      (cons (car b) (merge-2 a (cdr b)))]
     [else
      (error "Something horrible has happened" a b)]))
  (foldl merge-2 '() as))

(define (alist-map f al)
  (for/list ([rec al])
    (cons (car rec) (f (cdr rec)))))

(define (default-combine expr loc special)
  (cond
   [special special]
   [(andmap constant-value? (cdr expr))
    (annotation expr loc 'constant
                (eval-const-expr (cons (car expr) (map coeffs (cdr expr)))))]
   [(and (andmap periodic? (cdr expr)) (= 3 (length expr)))
    (annotation expr loc 'interesting
		(apply alist-merge lcm
		       (map coeffs (filter periodic? (cdr expr)))))]
   [(andmap (λ (x) (or (periodic? x) (constant-value? x))) (cdr expr))
    (annotation expr loc 'periodic
                (apply alist-merge lcm
                       (map coeffs (filter periodic? (cdr expr)))))]
   [else
    (annotation expr loc 'other #f)]))

(define (periodic-locs prog)
  (define (lp-loc-cons loc-el locp)
    (lp (cons loc-el (lp-loc locp)) (lp-periods locp)))
  (define (annot->plocs annot)
    (cond [(interesting? annot)
	   `(,(lp '() (coeffs annot)))]
	  [(other? annot)
	   (apply append
		  (let ([inner-annots (cdr (annotation-expr annot))])
		    (map (λ (lps base-loc)
			   (map (curry lp-loc-cons base-loc) lps))
			 (map annot->plocs inner-annots)
			 (map add1 (range (length inner-annots))))))]
	  [else '()]))
  (map (curry lp-loc-cons 2) (annot->plocs (program-body (periodicity prog)))))

(define (periodicity prog)
  (let loop ([prog prog] [loc '()])
    (match prog
      [(list (or 'lambda 'λ) (list vars ...) body)
       `(λ ,vars ,(loop body (cons 2 loc)))]
      [(? constant? c)
       (define repr (infer-representation c))
       ;; TODO : Do something more intelligent with 'PI
       (let ([val (if (rational? c) c (->flonum c repr))])
         (annotation val (reverse loc) 'constant val))]
      [(? variable? x)
       (annotation x (reverse loc) 'linear `((,x . 1)))]
      [(list 'if cond ift iff)
       (define ift* (loop ift (cons 2 loc)))
       (define iff* (loop iff (cons 3 loc)))
       (if (and (equal? (annotation-type ift*) (annotation-type iff*))
                (equal? (annotation-coeffs ift*) (annotation-coeffs iff*)))
           (annotation prog (reverse loc) (annotation-type ift*) (annotation-type ift*))
           (annotation prog (reverse loc) 'other #f))]
      [(list op args ...)
       (define expr (cons op (for/list ([idx (in-naturals 1)] [arg args]) (loop arg (cons idx loc)))))
       (define out (curry annotation expr (reverse loc)))

       ;; Default-combine handles function-generic things
       ;; The match below handles special cases for various functions
       (default-combine expr (reverse loc)
         (match expr
           [`(+ ,a ,b)
            (cond
             [(and (constant-value? a) (linear? b))
              (out 'linear (coeffs b))]
             [(and (linear? a) (constant-value? b))
              (out 'linear (coeffs a))]
             [(and (linear? a) (linear? b))
              (out 'linear (alist-merge + (coeffs a) (coeffs b)))]
             [else #f])]
           [`(- ,a)
            (cond
             [(linear? a)
              (out 'linear (alist-map - (coeffs a)))]
             [else #f])]
           [`(- ,a ,b)
            (cond
             [(and (constant-value? a) (linear? b))
              (out 'linear (coeffs b))]
             [(and (linear? a) (constant-value? b))
              (out 'linear (coeffs a))]
             [(and (linear? a) (linear? b))
              (out 'linear (alist-merge - (coeffs a) (coeffs b)))]
             [else #f])]

           [`(* ,a ,b)
            (cond
             [(and (linear? a) (constant-value? b))
              (out 'linear (alist-map (curry * (coeffs b)) (coeffs a)))]
             [(and (constant-value? a) (linear? b))
              (out 'linear (alist-map (curry * (coeffs a)) (coeffs b)))]
             [else #f])]
           [`(/ ,a ,b)
            (cond
             [(and (linear? a) (constant-value? b))
              (if (= 0 (coeffs b))
                  (out 'constant +nan.0)
                  (out 'linear (alist-map (curryr / (coeffs b)) (coeffs a))))]
             [else #f])]

           ;; Periodic functions record their period
           ;;         AS A MULTIPLE OF 2*PI
           ;; This prevents problems from round-off
           [`(sin ,a)
            (cond
             [(linear? a)
              (out 'periodic (alist-map / (coeffs a)))]
             [else #f])]
           [`(cos ,a)
            (cond
             [(linear? a)
              (out 'periodic (alist-map / (coeffs a)))]
             [else #f])]
           [`(tan ,a)
            (cond
             [(linear? a)
              (out 'periodic (alist-map / (coeffs a)))]
             [else #f])]
           [_ #f]))])))

(define (optimize-periodicity improve-func altn)
  (debug "Optimizing " altn " for periodicity..." #:from 'periodicity #:depth 2)
  (let* ([plocs (periodic-locs (alt-program altn))]
	 [oalts (map (λ (ploc)
		       (let* ([vars (map car (lp-periods ploc))]
			      [program `(λ ,vars ,(location-get (lp-loc ploc) (alt-program altn)))])
			 (debug "Looking at subexpression " program #:from 'periodicity #:depth 4)
			 (if (or (> (apply max (map cdr (lp-periods ploc))) *max-period-coeff*))
			     altn
			     (let ([context
				    (prepare-points
				     program
                                     `(and ,@(for/list ([(var period) (lp-periods ploc)])
                                               `(<= 0 ,var ,(* 2 pi var)))))])
			       (parameterize ([*pcontext* context])
				 (improve-func (make-alt program)))))))
		     plocs)]
	 ;; Substitute (mod x period) for x in any conditionals
	 [oexprs (map coerce-conditions
		      (map alt-program oalts)
		      (map lp-periods plocs))]
         [final-prog
          (for/fold ([prog (alt-program altn)]) ([oexpr oexprs] [ploc plocs])
            (location-do (lp-loc ploc) prog (const oexpr)))])
    (debug #:from 'periodicity "Periodicity result: " final-prog)
    (if (not (null? oalts))
        (alt final-prog 'periodicity (cons altn oalts))
        altn)))

(define (symbol-mod v periods)
  (if (dict-has-key? periods v)
      (let ([coeff (dict-ref periods v)])
        `(mod ,v ,(if (= 1/2 coeff) 'PI `(* ,(* 2 coeff) PI))))
      v))

(define (replace-vars vars expr periods)
  (for/fold ([expr expr]) ([var vars])
    (replace-expression expr var (symbol-mod var periods))))

(define (coerce-conditions prog periods)
  (let loop ([cur-body (program-body prog)])
    (match cur-body
      [`(if ,cond ,a ,b)
       `(if ,(replace-vars (program-variables prog) cond periods)
	    ,(loop a) ,(loop b))]
      [_ cur-body])))
