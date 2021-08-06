#lang racket

(require math/bigfloat rival)
(require "../syntax/syntax.rkt" "../alternative.rkt" "../common.rkt" "../conversions.rkt"
         "../interface.rkt" "../programs.rkt" "../sampling.rkt")

(provide setup-overflow-precondition!
         overflow-analysis-allowed?
         minimize-overflow)

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

(define (replace lst k v)
  (cond
   [(list? lst) (map (curryr replace k v) lst)]
   [(equal? lst k) v]
   [else lst]))

(define (get-repr-param repr)
  (define name (representation-name repr))
  (define search (*overflow-search-reprs*))
  (let loop ([name name] [search search] [bound #f])
    (cond
     [(or (null? name) (null? search)) bound]
     [(equal? (car name) (car search)) (loop (cdr name) (cdr search) bound)]
     [(and (number? (car name)) (symbol? (car search)))
      (cons (car search) (car name))])))

(define (search lo hi repr)
  (define param (get-repr-param repr))
  (define name (*overflow-search-reprs*))
  (define range (caar (precondition->intervals `(λ (x) TRUE) (list repr) repr)))
  (define incr-ideal
    (let loop ([curr-lo (ival-lo range)] [curr-hi (ival-hi range)] [param param])   ; increase parameter
      (define param* (cons (car param) (+ (cdr param) 1)))
      (define name* (replace name (car param*) (cdr param*)))
      (define repr* (get-representation name*))
      (define range (caar (precondition->intervals `(λ (x) TRUE) (list repr*) repr*)))
      (cond
       [(or (bf< (ival-lo range) curr-lo) (bf> (ival-lo range) curr-hi))      ; increasing range
        (if (and (bf< curr-lo lo) (bf> curr-hi hi))   ; curr is already big
            (replace name (car param) (cdr param))
            (loop (ival-lo range) (ival-hi range) param*))]
       [(and (bf> (ival-lo range) curr-lo) (bf< (ival-lo range) curr-hi))     ; decreasing range
        (cond
         [(and (bf> curr-lo lo) (bf< curr-hi hi))   ; curr is already big
          #f]
         [(or (bf> (ival-lo range) lo) (bf< (ival-hi range) hi))
          (replace name (car param) (cdr param))]
         [else
          (loop (ival-lo range) (ival-hi range) param*)])])))

  (define decr-param (if incr-ideal (get-repr-param (get-representation incr-ideal)) param))
  (let loop ([curr-lo (ival-lo range)] [curr-hi (ival-hi range)] [param decr-param])   ; decrease parameter
    (define param* (cons (car param) (- (cdr param) 1)))
    (define name* (replace name (car param*) (cdr param*)))
    (define repr* (get-representation name*))
    (define range (caar (precondition->intervals `(λ (x) TRUE) (list repr*) repr*)))
    (cond
      [(or (bf< (ival-lo range) curr-lo) (bf> (ival-lo range) curr-hi))     ; increasing range
      (if (and (bf< curr-lo lo) (bf> curr-hi hi))   ; curr is already big
          (replace name (car param) (cdr param))
          (loop (ival-lo range) (ival-hi range) param*))]
      [(and (bf> (ival-lo range) curr-lo) (bf< (ival-lo range) curr-hi))     ; decreasing range
      (cond
        [(and (bf> curr-lo lo) (bf< curr-hi hi))   ; curr is already big
        #f]
        [(or (bf> (ival-lo range) lo) (bf< (ival-hi range) hi))
        (replace name (car param) (cdr param))]
        [else
        (loop (ival-lo range) (ival-hi range) param*)])])))


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
       [(list (? repr-conv? conv) body)
        (loop body)]
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
        (cond
         [(bf= lo hi)   ; exact?
          (values (cons op args*) range)]
         [else
            (define repr (get-representation (operator-info op 'otype)))
            (define ideal (search lo hi repr))
            (printf "~a: ~a\n" expr ideal)
            (values (cons op args*) range)])]
       [(? variable?) (values expr (hash-ref cache expr))]
       [(? constant?) (values expr (hash-ref cache expr))]
       [(? number?) (values expr (hash-ref cache expr))])))

  (void))
