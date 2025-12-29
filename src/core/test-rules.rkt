#lang racket

(require rackunit)
(require "../utils/common.rkt"
         "../utils/float.rkt"
         "../syntax/types.rkt"
         "../syntax/batch.rkt"
         "rival.rkt"
         "rules.rkt"
         "programs.rkt"
         "../syntax/platform.rkt"
         "../syntax/load-platform.rkt"
         "sampling.rkt")

(activate-platform! (*platform-name*))

(define skip-rules '(log-pow))

(define num-test-points (make-parameter 100))
(define double-repr (get-representation 'binary64))

(define (env->ctx p1 p2)
  (define vars (set-union (free-variables p1) (free-variables p2)))
  (context vars double-repr (map (const double-repr) vars)))

(define (drop-sound expr)
  (match expr
    [(list op args ... extra)
     #:when (string-contains? (~a op) "sound")
     (define op* (string->symbol (substring (symbol->string (car expr)) (string-length "sound-"))))
     (cons op* (map drop-sound args))]
    [(list op args ...) (cons op (map drop-sound args))]
    [_ expr]))

(define (check-rule test-rule)
  (match-define (rule name p1 p2 _) test-rule)
  (define ctx (env->ctx p1 p2))

  (define-values (batch brfs) (progs->batch (list p1 (drop-sound p2))))
  (match-define (list pts exs1 exs2)
    (parameterize ([*num-points* (num-test-points)]
                   [*max-find-range-depth* 0])
      (sample-points '(TRUE) batch brfs (list ctx ctx))))

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
  (for* ([rule (in-list (*rules*))]
         #:unless (set-member? skip-rules (rule-name rule)))
    (test-case (~a (rule-name rule))
      (check-rule rule))))
