#lang racket
(require "common.rkt" "programs.rkt" "syntax/types.rkt" "core/simplify.rkt" "syntax/rules.rkt" "core/egg-herbie.rkt")

(provide connected-components)

(define (get-swaps vars expr)
  (define swapt
    (for/list ([swap (in-combinations vars 2)])
      (match-define (list a b) swap)
      (replace-vars (list (cons a b) (cons b a)) expr)))

  (define egg-query (make-egg-query (cons expr swapt) (*simplify-rules*)))
  (define out (map last (simplify-batch egg-query)))
  (match-define (cons orig swapt*) out)
  (for/list ([swap* swapt*] [swap (in-combinations vars 2)]
             #:when (equal? swap* orig))
    swap))

(define (connected-components expr ctx)
  (define vars (context-vars ctx))

  (define swaps (get-swaps vars expr))
  (define rules*
    (for/list ([swap swaps])
      (match-define (list a b) swap)
      (rule (string->symbol (format "swap-~a-~a" a b))
            (index-of vars a)
            (index-of vars b)
            '()
            'real)))

  (define egg-query (make-egg-query (range (length vars)) rules* #:const-folding? #f))
  (define groups (map last (simplify-batch egg-query)))
  (map (lambda (group) (map car group)) (group-by cdr (map cons vars groups))))

(define (get-abs var expr)
  ;; Make an egg query for (cons original-expression abs-expression)
  'todo)

