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

(define (get-proof-errors proof pcontext ctx program-vars)
  (define proof-programs
    (for/list ([step (in-list proof)])
      `(λ ,program-vars ,(remove-rewrites step))))
  (define proof-errors (batch-errors proof-programs pcontext ctx))
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

(define (add-soundiness-to pcontext ctx cache altn)
  (match altn
    ;; This is alt coming from rr
    [(alt prog `(rr, loc, input #f #f) `(,prev))
     (cond
       [(egraph-query? input) ;; Check if input is an egraph-query struct (B-E-R)
        (define e-input input)
        (define p-input (cons (location-get loc (alt-program prev)) (location-get loc prog)))
        (match-define (cons proof errs)
          (hash-ref! cache (cons p-input e-input)
                     (λ ()
                       (match-define (cons variants proof)
                         (run-egg e-input #t
                                  #:proof-input p-input
                                  #:proof-ignore-when-unsound? #t))
                       (cond
                         [proof
                          ;; Proofs are actually on subexpressions,
                          ;; we need to construct the proof for the full expression
                          (define proof*
                            (for/list ([step proof])
                              (let ([step* (canonicalize-rewrite step)])
                                (program-body (location-do loc prog (λ _ step*))))))
                          (define errors
                            (let ([vars (program-variables prog)])
                              (get-proof-errors proof* pcontext ctx vars)))
                          (cons proof* errors)]
                         [else
                          (cons #f #f)]))))
         (alt prog `(rr ,loc ,input ,proof ,errs) `(,prev))]
        [(rule? input) ;; (R-O) case
         (match-define (cons proof errs)
           (hash-ref! cache (cons input prog)
                      (λ ()
                        (define proof
                          (list (program-body (alt-program prev))
                                (list 'Rewrite=> (rule-name input) (program-body prog))))
                        (define errs
                          (let ([vars (program-variables prog)])
                            (get-proof-errors proof pcontext ctx vars)))
                        (cons proof errs))))
         (alt prog `(rr ,loc ,input ,proof ,errs) `(,prev))]
        [else
         (alt prog `(rr ,loc, input #f #f) `(,prev))])]
            
      ;; This is alt coming from simplify
    [(alt prog `(simplify ,loc ,input, #f #f) `(,prev))
     (define e-input input)
     (define p-input (cons (location-get loc (alt-program prev)) (location-get loc prog)))
     (match-define (cons proof errs)
       (hash-ref! cache (cons p-input e-input)
                  (λ ()
                    (match-define (cons variants proof)
                      (run-egg e-input #f
                               #:proof-input p-input
                               #:proof-ignore-when-unsound? #t))
                    (cond
                      [proof
                       ;; Proofs are actually on subexpressions,
                       ;; we need to construct the proof for the full expression
                       (define proof*
                         (for/list ([step proof])
                           (let ([step* (canonicalize-rewrite step)])
                             (program-body (location-do loc prog (λ _ step*))))))
                       (define errors
                         (let ([vars (program-variables prog)])
                           (get-proof-errors proof* pcontext ctx vars)))
                       (cons proof* errors)]
                      [else
                       (cons #f #f)]))))
     (alt prog `(simplify ,loc ,input ,proof ,errs) `(,prev))]
    [else
     altn]))

(define (add-soundiness alts pcontext ctx)
  (define cache (make-hash))
  (for/list ([altn alts])
    (alt-map (curry add-soundiness-to pcontext ctx cache) altn)))
