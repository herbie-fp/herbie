#lang racket

(require "points.rkt" "syntax/types.rkt" "core/localize.rkt" "timeline.rkt"
         "common.rkt")

(provide group-errors)

(define (group-errors expr pcontext)
  ;; (define points (for/list ([(pt _) (in-pcontext pcontext)]) pt))
  ;; (define local-errors (car (compute-local-errors (list expr) (*context*))))

  ;; (define max-per-point (make-hash))
  ;; (define errpts-per-subexpr (make-hash))

  ;; (for ([pt points])
  ;;   (hash-set! max-per-point pt (list 0 0)))

  ;; (for ([(sub-expr errors) local-errors])
  ;;   (hash-set! errpts-per-subexpr sub-expr null)
  ;;   (for ([pair (zip points errors)])
  ;;     (define pt (car pair))
  ;;     (define err (cadr pair))
  ;;     (define old-error (hash-ref max-per-point pt))
  ;;     (if (> err (cadr old-error))
  ;;       (hash-set! max-per-point pt (list sub-expr err))
  ;;       null)))
  
  ;; (hash-set! errpts-per-subexpr #f null)

  ;; (for ([(k v) max-per-point])
  ;;   (define sub-expr (car v))
  ;;   (define err (cadr v))
  ;;   (if (> err 2)
  ;;     (hash-update! errpts-per-subexpr sub-expr (curry cons k))
  ;;     (hash-update! errpts-per-subexpr #f (curry cons k))))
  
  ; (eprintf "[grouped errors]\n")
  ; (for ([(k v) errpts-per-subexpr])
  ;   (eprintf "~a, ~a \n" k v))
  
  ; (eprintf "\n[percentage-errored]\n")
  ;; (for ([(k v) errpts-per-subexpr])
  ;;   (define num-of-errors (length v))
  ;;   ; (eprintf "~a : ~a\n" k num-of-errors)

  ;;   (if (= 0 num-of-errors)
  ;;     #f
  ;;     (timeline-push! 'problems (~a k) num-of-errors)))

  
  (match-define (cons subexprs pt-errorss)
    (flip-lists
     (hash->list (car (compute-local-errors (list expr) (*context*))))))

  ;; (eprintf "~a\n" subexprs)
  ;; (eprintf "~a\n" pt-errors)

  (define pt-worst-subexprs
    (for/list ([pt-errors (in-list pt-errorss)])
      (match-define (cons worst-subexpr pt-max-error)
        (argmax cdr (map cons subexprs pt-errors)))
      (if (> pt-max-error 2) worst-subexpr #f)))

  (for/hash ([group (in-list (group-by identity pt-worst-subexprs))])
    (values (first group) (length group))))
