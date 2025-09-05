#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/types.rkt"
         "rival.rkt"
         "rules.rkt"
         "programs.rkt"
         "../syntax/platform.rkt"
         "../syntax/load-platform.rkt"
         "sampling.rkt")

(activate-platform! (*platform-name*))

(define num-test-points (make-parameter 100))
(define double-repr (get-representation 'binary64))

(define (env->ctx p1 p2)
  (define vars (set-union (free-variables p1) (free-variables p2)))
  (context vars double-repr (map (const double-repr) vars)))

(define (drop-unsound expr)
  (match expr
    [(list op args ...)
     #:when (string-contains? (~a op) "unsound")
     (define op* (string->symbol (string-replace (symbol->string (car expr)) "unsound-" "")))
     (cons op* (map drop-unsound args))]
    [(list (? (λ (x) (string-prefix? (symbol->string x) "sound-")) op) args ...)
     (define op* (string->symbol (substring (symbol->string (car expr)) (string-length "sound-"))))
     (define args* (drop-right args 1))
     (cons op* (map drop-unsound args*))]
    [_ expr]))

(define (check-rule test-rule)
  (match-define (rule name p1 p2 _) test-rule)
  (define ctx (env->ctx p1 p2))

  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (sample-points '(TRUE) (list p1 (drop-unsound p2)) (list ctx ctx))))

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
