#lang racket
(require "../common.rkt" "enode.rkt" "../programs.rkt")
(provide mk-extractor extractor-cost extractor-iterate extractor-extract)

;; The work list maps enodes to a pair (cost . expr) of that node's
;; cheapest representation and its cost. If the cost is #f, the expr
;; is also #f, and in this case no expression is yet known for that
;; enode.

(define (mk-extractor . ens)
  (define work-list (make-hash))
  (for ([en ens])
    (hash-set! work-list (pack-leader en) (cons #f #f)))
  work-list)

(define (extractor-cost work-list . ens)
  (apply +
         (for/list ([en ens])
           (car (hash-ref work-list en '(#f . #f))))))

;; Extracting the smallest expression means iterating, until
;; fixedpoint, either discovering new relevant expressions or
;; cheaper expressions for some expression.
(define (extractor-iterate work-list)
  (let loop ([iter 0])
    (define changed? #f)
    (define-values (infs cost) (extractor-cost work-list))
    (debug #:from 'simplify #:depth 2 "Extracting #" iter ": cost " infs " inf + " cost)
    (for ([en (in-list (hash-keys work-list))]) ;; in-list to avoid mutating the iterator
      (define leader (pack-leader en))
      (when (not (eq? en leader))
        (hash-set! work-list leader (hash-ref work-list en))
        (hash-remove! work-list en))
      (define vars (enode-vars leader))
      (define vars*
        (filter identity
                (for/list ([var vars])
                  (match var
                    [(list op args ...)
                     (define args*
                       (for/list ([suben args])
                         (define subleader (pack-leader suben))
                         (match (hash-ref work-list subleader (cons #f #t))
                           [(cons (? number? cost) best-arg)
                            (cons cost best-arg)]
                           [(cons #f not-in-hash?)
                            (hash-set! work-list subleader (cons #f #f))
                            (set! changed? (or changed? not-in-hash?))
                            #f])))
                     (if (andmap identity args*)
                         (cons (apply + 1 (map car args*))
                               (cons op args*))
                         #f)]
                    [_
                     (cons 1 var)]))))
      (match vars*
        ['() #f]
        [_
         (define best-resolution (argmin car vars*))
         (define cost (car best-resolution))
         (define old-cost (car (hash-ref work-list leader)))
         (when (or (not old-cost) (< cost old-cost))
           (hash-set! work-list leader best-resolution)
           (set! changed? #t))]))
    (when changed?
      (loop (+ iter 1)))))

(define (extractor-extract work-list . ens)
  (for/list ([en ens])
    (hash-ref work-list (pack-leader en))))

