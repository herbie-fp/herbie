#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "rules.rkt"
         (submod "rules.rkt" internals)
         "../syntax/platform.rkt"
         "../syntax/load-plugin.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "compiler.rkt"
         "rival.rkt"
         "sampling.rkt")

(load-herbie-builtins)

(define num-test-points (make-parameter 100))

;; WARNING: These aren't treated as preconditions, they are only used for range inference
(define *conditions*
  (list '[asinh-2_binary64 . (>= x 0)]
        '[asinh-2_binary32 . (>= x 0)]
        '[pow-unpow_binary64 . (>= a 0)]
        '[pow-unpow_binary32 . (>= a 0)]
        '[pow-pow_binary64 . (>= a 0)]
        '[pow-pow_binary32 . (>= a 0)]
        '[sqrt-pow1_binary64 . (>= x 0)]
        '[sqrt-pow1_binary32 . (>= x 0)]
        ;; These next three approximate pi so that range analysis will work
        '[asin-sin-s_binary64 . (<= (fabs x) 1.5708)]
        '[asin-sin-s_binary32 . (<= (fabs x) 1.5708)]
        '[acos-cos-s_binary64 . (and (<= 0 x) (<= x 3.1416))]
        '[acos-cos-s_binary32 . (and (<= 0 x) (<= x 3.1416))]
        '[atan-tan-s_binary64 . (<= (fabs x) 1.5708)]
        '[atan-tan-s_binary32 . (<= (fabs x) 1.5708)]))

(define (rule->impl-rules rule)
  (platform-impl-rules (list rule)))

(define (check-rule-sound test-rule)
  (match-define (rule name p1 p2 env repr) test-rule)
  (define vars (map car env))
  (define itypes (map cdr env))
  (define ctx (context vars repr itypes))

  (define spec1 (prog->spec p1))
  (define spec2 (prog->spec p2))
  (match-define (list pts exs)
    (parameterize ([*num-points* (num-test-points)] [*max-find-range-depth* 0])
      (cdr (sample-points '(TRUE) (list spec1) (list ctx)))))

  (define compiler (make-real-compiler (list spec2) (list ctx)))
  (for ([pt (in-list pts)] [v1 (in-list exs)])
    (with-check-info* (map make-check-info vars pt)
                      (λ ()
                        (define-values (status v2) (real-apply compiler pt))
                        (with-check-info (['lhs v1] ['rhs v2] ['status status])
                                         (when (and (real? v2)
                                                    (nan? v2)
                                                    (not (set-member? '(exit unsamplable) status)))
                                           (fail "Right hand side returns NaN")))))))

(define (check-rule-correct test-rule)
  (match-define (rule name p1 p2 itypes repr) test-rule)
  (define fv (dict-keys itypes))
  (define ctx (context fv repr (map (curry dict-ref itypes) fv)))

  (define pre (dict-ref *conditions* name '(TRUE)))
  (define spec1 (prog->spec p1))
  (define spec2 (prog->spec p2))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)] [*max-find-range-depth* 0])
      (cdr (sample-points pre (list spec1 spec2) (list ctx ctx)))))

  (for ([pt (in-list pts)] [v1 (in-list exs1)] [v2 (in-list exs2)])
    (with-check-info*
     (map make-check-info fv pt)
     (λ () (with-check-info (['lhs v1] ['rhs v2]) (check-eq? (ulp-difference v1 v2 repr) 1))))))

(define (check-rule-fp-safe test-rule)
  (match-define (rule name p1 p2 itypes repr) test-rule)
  (define fv (dict-keys itypes))
  (define ctx (context fv repr (map (curry dict-ref itypes) fv)))
  (define (make-point _)
    (for/list ([v (in-list fv)])
      (random-generate (dict-ref itypes v))))
  (define points (build-list (num-test-points) make-point))
  (define prog (compile-progs (list p1 p2) ctx))
  (for ([pt points])
    (with-check-info (['point (map list fv pt)])
                     (match-define (vector v1 v2) (apply prog pt))
                     (check-equal? v1 v2))))

(module+ main
  (define _ (*simplify-rules*)) ; force an update
  (num-test-points (* 100 (num-test-points)))
  (command-line #:args names
                (for ([name names])
                  (eprintf "Checking ~a...\n" name)
                  (define rule (first (filter (λ (x) (equal? (~a (rule-name x)) name)) (*rules*))))
                  (for ([rule* (rule->impl-rules rule)])
                    (check-rule-correct rule*)
                    (unless (set-member? (*unsound-rules*) rule)
                      (check-rule-sound rule*))
                    (when (set-member? (*fp-safe-simplify-rules*) rule)
                      (check-rule-fp-safe rule*))))))

(module+ test
  (define _ (*simplify-rules*)) ; force an update

  (for* ([(_ test-ruleset) (in-dict (*rulesets*))]
         [test-rule (first test-ruleset)]
         [test-rule* (rule->impl-rules test-rule)])
    (test-case (~a (rule-name test-rule*))
      (check-rule-correct test-rule*)))

  (for* ([(_ test-ruleset) (in-dict (*rulesets*))]
         [test-rule (first test-ruleset)]
         [test-rule* (rule->impl-rules test-rule)]
         #:unless (set-member? (*unsound-rules*) test-rule))
    (test-case (~a (rule-name test-rule*))
      (check-rule-sound test-rule*)))

  (for* ([(_ test-ruleset) (in-dict (*rulesets*))]
         [test-rule (first test-ruleset)]
         [test-rule* (rule->impl-rules test-rule)]
         #:when (set-member? (*fp-safe-simplify-rules*) test-rule))
    (test-case (~a (rule-name test-rule*))
      (check-rule-fp-safe test-rule*))))
