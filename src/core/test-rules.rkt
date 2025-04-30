#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/types.rkt"
         "rival.rkt"
         "rules.rkt"
         "programs.rkt"
         "sampling.rkt")

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

(define *conditions*
  (list '[pow-unpow . (>= a 0)]
        '[pow-pow . (>= a 0)]
        '[sqrt-pow1 . (>= x 0)]
        '[asin-sin-s . (<= (fabs x) (/ (PI) 2))]
        '[acos-cos-s . (and (<= 0 x) (<= x (PI)))]
        '[atan-tan-s . (<= (fabs x) (/ (PI) 2))]))

(define double-repr (get-representation 'binary64))

(define (env->ctx p1 p2)
  (define vars (set-union (free-variables p1) (free-variables p2)))
  (context vars double-repr (map (const double-repr) vars)))

(define (check-rule test-rule)
  (match-define (rule name p1 p2 _ _ _) test-rule)
  (define ctx (env->ctx p1 p2))

  (define pre (dict-ref *conditions* name '(TRUE)))
  (unless (equal? pre '(TRUE))
    (check-false (set-member? (rule-tags test-rule) 'sound) "Sound rules cannot have conditions"))

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

(module+ main
  (num-test-points (* 100 (num-test-points)))
  (command-line #:args names
                (for* ([name (in-list names)]
                       [rule (in-list (*rules*))]
                       #:when (equal? (~a (rule-name rule)) name))
                  (eprintf "Checking ~a...\n" name)
                  (check-rule rule))))

(module+ test
  (for* ([rule (in-list (*rules*))])
    (test-case (~a (rule-name rule))
      (check-rule rule))))
