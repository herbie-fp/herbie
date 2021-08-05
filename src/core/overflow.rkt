#lang racket

(require math/bigfloat rival)
(require "../syntax/syntax.rkt" "../alternative.rkt" "../common.rkt"
         "../conversions.rkt" "../programs.rkt" "../sampling.rkt")

(provide setup-overflow-precondition! overflow-analysis-allowed? minimize-overflow)

(define *precondition* #f)

(register-reset
  (λ () (set! *precondition* #f)))

;; Precondition setup

(define (valid-precondition? intervals)
  (for/and ([interval (in-list intervals)])
    (= (length interval) 1)))

(define (setup-overflow-precondition! vars pre repr)
  (define intervals (precondition->intervals pre (map (λ (_) repr) vars) repr))
  (if (valid-precondition? intervals)
      (set! *precondition* (map (λ (x y) (cons x (first y))) vars intervals))
      (set! *precondition* #f)))

(define (overflow-analysis-allowed?)
  *precondition*)

;; Overflow analysis

(define (analyze expr cache)
  (let loop ([expr expr])
    (hash-ref! cache expr
               (λ ()
                (match expr
                 [(list 'if cond ift iff)
                  (ival-if (loop cond) (loop ift) (loop iff))]
                 [(list op args ...)
                  (apply (operator-info op 'ival) (map loop args))]
                 [(? variable?) (dict-ref *precondition* expr)]
                 [(? constant?) (constant-info expr 'ival)]
                 [(? number?) (mk-ival (bf expr))])))))

(define (minimize-overflow altn)
  (printf "minimize: ~a\n" altn)
  (define expr (program-body (alt-program altn)))
  (define cache (make-hash))
  (analyze expr cache)
  (define-values (expr* range)
    (let loop ([expr expr])
      (match expr
       [(list 'if cond ift iff)
        (define-values (ift* r1) (loop ift))
        (define-values (iff* r2) (loop iff))
        (values (list 'if cond ift* iff*)
                (hash-ref cache expr))]
       [(list op args ...)
        (define range (hash-ref cache expr))
        (define-values (args* ranges*)
          (for/lists (l1 l2) ([arg (in-list args)])
            (loop arg)))
        (define-values (lo hi)
          (for/fold ([lo (ival-lo range)] [hi (ival-hi range)])
                    ([range (in-list ranges*)])
            (values (bfmin lo (ival-lo range))
                    (bfmax hi (ival-hi range)))))
        (values (cons op args*) range)]
       [(? variable?) (values expr (hash-ref cache expr))]
       [(? constant?) (values expr (hash-ref cache expr))]
       [(? number?) (values expr (hash-ref cache expr))])))

  (void))
