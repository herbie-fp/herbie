#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/types.rkt"
         "rival.rkt"
         "rules.rkt"
         "sampling.rkt")

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

(define *conditions*
  (list '[asinh-2 . (>= x 0)]
        '[pow-unpow . (>= a 0)]
        '[pow-pow . (>= a 0)]
        '[sqrt-pow1 . (>= x 0)]
        '[asin-sin-s . (<= (fabs x) (/ (PI) 2))]
        '[acos-cos-s . (and (<= 0 x) (<= x (PI)))]
        '[atan-tan-s . (<= (fabs x) (/ (PI) 2))]))

(define double-repr (get-representation 'binary64))
(define boolean-repr (get-representation 'bool))

(define (type->repr type)
  (match type
    ['real double-repr]
    ['bool boolean-repr]))

(define (env->ctx env out)
  (define vars (dict-keys env))
  (define itypes (map type->repr (dict-values env)))
  (context vars (type->repr out) itypes))

(define (extract-point v)
  (match v
    [#f #f]
    [(list v*) v*]
    [_ (error "Uknown Rival's result")]))

(define (eval-check-sound compiler1 compiler2 pt test-rule)
  (define cnt 0)
  (when (or (> (vector-count boolean? pt) 0)
            (zero? (vector-count nan? pt))) ; Do not check soundess for NaN points
    (set! cnt (add1 cnt))
    (define-values (status1 v1) (real-apply compiler1 pt))
    (define-values (status2 v2) (real-apply compiler2 pt))
    (set! v1 (extract-point v1))
    (set! v2 (extract-point v2))

    (with-check-info
     (['rule test-rule] ['pt pt] ['lhs v1] ['rhs v2] ['lhs-status status1] ['rhs-status status2])
     (when (or (and (set-member? '(exit valid) status1) ; Range shrinking!
                    (equal? 'invalid status2))
               (and (set-member? '(exit valid) status2) ; Range widening!
                    (equal? 'invalid status1))
               (and (equal? 'valid status1) ; Different results for valid points
                    (equal? 'valid status2)
                    (and (not (equal? v1 v2))
                         (and (not (equal? v1 0.0)) (not (equal? v2 -0.0)))
                         (and (not (equal? v1 -0.0)) (not (equal? v2 0.0))))))
       (fail "Rule is unsound"))))
  cnt)

(define (check-rule-sound test-rule)
  (define cnt 0)
  ; Rule's data
  (match-define (rule name p1 p2 env out tags) test-rule)
  (define ctx (env->ctx env out))

  ; Compilers + random sampler
  (define-values (sampler) (λ () (vector-map random-generate (list->vector (context-var-reprs ctx)))))
  (define compiler1 (make-real-compiler (list p1) (list ctx)))
  (define compiler2 (make-real-compiler (list p2) (list ctx)))

  ; Soundness for some common cases with simple integers
  ; Only for rules where every input var's repr is not 'bool
  (when (andmap (λ (x) (not (equal? 'bool (representation-name x)))) (context-var-reprs ctx))
    (define number-of-vars (length (context-vars ctx)))
    (define pt-combinations (combinations (range -10 10 1) number-of-vars))
    (for ([pt (in-list pt-combinations)])
      (set! cnt (+ cnt (eval-check-sound compiler1 compiler2 (list->vector pt) test-rule)))))

  ; Random soundess
  (for ([n (in-range (num-test-points))])
    (define pt (sampler))
    (set! cnt (+ cnt (eval-check-sound compiler1 compiler2 pt test-rule))))

  (printf "Rule ~a has been checked for soundess ~a times\n" test-rule cnt))

(define (check-rule-correct test-rule)
  (match-define (rule name p1 p2 env out tags) test-rule)
  (define ctx (env->ctx env out))

  (define pre (dict-ref *conditions* name '(TRUE)))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (sample-points pre (list p1 p2) (list ctx ctx))))

  (for ([pt (in-list pts)]
        [v1 (in-list exs1)]
        [v2 (in-list exs2)])
    (with-check-info* (map make-check-info (context-vars ctx) (vector->list pt))
                      (λ ()
                        (with-check-info (['lhs v1] ['rhs v2])
                                         (check-eq? (ulp-difference v1 v2 (context-repr ctx)) 1))))))

(define (check-rule rule)
  #;(check-rule-correct rule)
  (when (set-member? (rule-tags rule) 'sound)
    (check-rule-sound rule)))

(module+ main
  (num-test-points (* 100 (num-test-points)))
  (command-line #:args names
                (for ([name names])
                  (eprintf "Checking ~a...\n" name)
                  (define test-rule
                    (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
                  (check-rule test-rule))))

(module+ test
  (for* ([rule (in-list (*rules*))])
    (test-case (~a (rule-name rule))
      (check-rule rule))))
