#lang racket

(require "../common.rkt" "../programs.rkt" "../alternative.rkt"
         "../syntax/rules.rkt" "../interface.rkt")

(provide
 pattern-match pattern-substitute
 rewrite-expression-head rewrite-expression
 change-apply rule-rewrite)

;;; Our own pattern matcher.
;;
;; The racket (match) macro doesn't give us access to the bindings made
;; by the matcher, so we wrote our own.
;;
;; The syntax is simple:
;;   numbers are literals ; symbols are variables ; lists are expressions
;;
;; Bindings are stored as association lists

(define (merge-bindings binding1 binding2)
  (define (fail . irr) #f)
  (and binding1
       binding2
       (let/ec quit
         (for/fold ([binding binding1]) ([(k v) (in-dict binding2)])
           (dict-update binding k (λ (x) (if (equal? x v) v (quit #f))) v)))))

(define (pattern-match pattern expr)
  (define (fail . irr) #f)

  (match pattern
   [(? constant?)
    (and (equal? pattern expr) '())]
   [(? variable?)
    (list (cons pattern expr))]
   [(list phead _ ...)
    (and (list? expr)
         (equal? (car expr) phead)
         (= (length expr) (length pattern))
         (for/fold ([bindings '()])
             ([pat (cdr pattern)] [subterm (cdr expr)])
           (merge-bindings bindings (pattern-match pat subterm))))]))

(define (pattern-substitute pattern bindings)
  ; pattern binding -> expr
  (match pattern
   [(? constant?) pattern]
   [(? variable?)
    (dict-ref bindings pattern)]
   [(list phead pargs ...)
    (cons phead (map (curryr pattern-substitute bindings) pargs))]))

;; Random helper functions

(define (rule-apply rule expr)
  (let ([bindings (pattern-match (rule-input rule) expr)])
    (if bindings
        (cons (pattern-substitute (rule-output rule) bindings) bindings)
        #f)))

(define (rule-rewrite rule prog [loc '()])
  (let/ec return
    (location-do loc prog
                 (λ (x) (match (rule-apply rule x)
                          [(cons out bindings) out]
                          [#f (return #f)])))))

(define (change-apply cng prog)
  (match-define (change rule location bindings) cng)
  (location-do location prog (const (pattern-substitute (rule-output rule) bindings))))

;; The rewriter

(define (rewrite-expression expr repr #:rules rules #:root [root-loc '()] #:destruct [destruct? #f])
  (define type (representation-name (repr-of expr repr (*var-reprs*))))
  (reap [sow]
    (for ([rule rules] #:when (equal? type (rule-otype rule)))
      (let* ([result (rule-apply rule expr)])
        (when result
            (sow (list (change rule root-loc (cdr result)))))))))

(define (rewrite-expression-head expr repr #:rules rules #:root [root-loc '()] #:depth [depth 1])
  (define type (representation-name (repr-of expr repr (*var-reprs*))))
  (define (rewriter sow expr ghead glen loc cdepth)
    ; expr _ _ _ _ -> (list (list change))
    (for ([rule rules] #:when (equal? type (rule-otype rule)))
      (when (or
             (not ghead) ; Any results work for me
             (and
              (list? (rule-output rule))
              (= (length (rule-output rule)) glen)
              (eq? (car (rule-output rule)) ghead)))
        (for ([option (matcher* expr (rule-input rule) loc (- cdepth 1))])
          ;; Each option is a list of change lists
          (sow (cons (change rule (reverse loc) (cdr option)) (car option)))))))

  (define (reduce-children sow options)
    ; (list (list ((list change) * bindings)))
    ; -> (list ((list change) * bindings))
    (for ([children options])
      (let ([bindings* (foldl merge-bindings '() (map cdr children))])
        (when bindings*
          (sow (cons (apply append (map car children)) bindings*))))))

  (define (fix-up-variables sow pattern cngs)
    ; pattern (list change) -> (list change) * bindings
    (match-define (change rule loc bindings) (car cngs))
    (define result (pattern-substitute (rule-output rule) bindings))
    (define bindings* (pattern-match pattern result))
    (when bindings* (sow (cons cngs bindings*))))

  (define cache (make-hash))
  (define (matcher* expr pattern loc cdepth)
    (hash-ref! cache (list loc pattern cdepth) (λ () (matcher expr pattern loc cdepth))))

  (define (matcher expr pattern loc cdepth)
    ; expr pattern _ -> (list ((list change) * bindings))
    (reap [sow]
      (match pattern
        [(? variable?)
         (sow (cons '() (list (cons pattern expr))))]
        [(? constant?)
         (when (equal? expr pattern)
           (sow (cons '() '())))]
        [(list phead _ ...)
         (when (and (list? expr) (equal? phead (car expr))
                    (= (length pattern) (length expr)))
           (let/ec k ;; We have an option to early exit if a child pattern cannot be matched
             (define child-options ; (list (list ((list cng) * bnd)))
               (for/list ([i (in-naturals)] [sube expr] [subp pattern] #:when (> i 0))
                 ;; Note: fuel is "depth" not "cdepth", because we're recursing to a child
                 (define options (matcher* sube subp (cons i loc) depth))
                 (when (null? options) (k)) ;; Early exit 
                 options))
             (reduce-children sow (apply cartesian-product child-options))))

         (when (and (> cdepth 0)
                    (or (flag-set? 'generate 'better-rr)
                        (not (and (list? expr) (equal? phead (car expr)) (= (length pattern) (length expr))))))
           ;; Sort of a brute force approach to getting the bindings
           (rewriter (curry fix-up-variables sow pattern)
                     expr (car pattern) (length pattern) loc (- cdepth 1)))])))

  ;; The "#f #f" means that any output result works. It's a bit of a hack
  (reap [sow] (rewriter (compose sow reverse) expr #f #f (reverse root-loc) depth)))
