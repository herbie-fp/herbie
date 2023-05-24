#lang racket

(require "alternative.rkt" "points.rkt" "programs.rkt"
         "core/egg-herbie.rkt" "core/simplify.rkt" "syntax/types.rkt" 
         "core/matcher.rkt" "syntax/rules.rkt")

(provide add-soundiness)

(define (canonicalize-rewrite proof)
  (match proof
    [`(Rewrite=> ,rule ,something)
     (list 'Rewrite=> (get-canon-rule-name rule rule) something)]
    [`(Rewrite<= ,rule ,something)
     (list 'Rewrite<= (get-canon-rule-name rule rule) something)]
    [(list _ ...)
     (map canonicalize-rewrite proof)]
    [else proof]))

(define (get-proof-errors proof pcontext ctx)
  (define proof-exprs (map remove-rewrites proof))
  (define proof-errors (batch-errors proof-exprs pcontext ctx))
  (define proof-diffs
    (cons (list 0 0)
          (for/list ([prev proof-errors] [current (rest proof-errors)])
                    (define num-increase
                      (for/sum ([a prev] [b current])
                               (if (> b a)
                                   1
                                   0)))
                    (define num-decrease
                      (for/sum ([a prev] [b current])
                               (if (< b a)
                                   1
                                   0)))
                    (list num-increase
                          num-decrease (length prev)))))
  proof-diffs)

(define (generate-rewrite-once-proof rule prog prev)
  (list (alt-expr prev) ;; Start Expression
        (list 'Rewrite=> (rule-name rule) prog)))

(define (canonicalize-proof prog loc proof pcontext ctx)
  (cond 
   [proof
    (define proof*
      (for/list ([step (in-list proof)])
        (location-do loc prog (const (canonicalize-rewrite step)))))
    (define errors (get-proof-errors proof* pcontext ctx))
    (values proof* errors)]
   [else
    (values #f #f)]))

(define (add-soundiness-to pcontext ctx altn)
  (match (alt-event altn)
    [`(rr (2 ,@loc) ,(? egraph-query? input) #f #f)
     (match-define (list prev) (alt-prevs altn))
     (define p-input
       (cons (location-get loc (alt-expr prev))
             (location-get loc (alt-expr altn))))
     (match-define (cons variants proof) (run-egg input #t #t #:proof-input p-input))
     (define-values (proof* errors)
       (canonicalize-proof (alt-expr altn) loc proof pcontext ctx))
     (struct-copy alt altn [event `(rr (2 ,@loc) ,input ,proof* ,errors)])]

    [`(rr (2 ,@loc) ,(? rule? input) #f #f)
     (match-define (list prev) (alt-prevs altn))
     (define proof-ro
       (generate-rewrite-once-proof input (alt-expr altn) prev))
     (define errors-ro
       (get-proof-errors proof-ro pcontext ctx))
     (struct-copy alt altn [event `(rr (2 ,@loc) ,input ,proof-ro ,errors-ro)])]

    ;; This is alt coming from simplify
    [`(simplify (2 ,@loc) ,input, #f #f)
     (match-define (list prev) (alt-prevs altn))
     (define p-input
       (cons (location-get loc (alt-expr prev))
             (location-get loc (alt-expr altn))))
     (match-define (cons variants proof) (run-egg input #t #f #:proof-input p-input))
     (define-values (proof* errors)
       (canonicalize-proof (alt-expr altn) loc proof pcontext ctx))
     (struct-copy alt altn [event `(simplify (2 ,@loc) ,input ,proof* ,errors)])]

    [else altn]))


(define (add-soundiness alts pcontext ctx)
  (for/list ([altn alts])
    (alt-map (curry add-soundiness-to pcontext ctx) altn)))
