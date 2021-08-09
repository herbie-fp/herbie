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
  (and *precondition* (not (null? (*overflow-search-reprs*)))))

;; Overflow analysis

(define range-cache (make-hash))

(register-reset
  (λ () (set! range-cache (make-hash))))

(define (get-range repr)
  (hash-ref! range-cache repr
             (λ () (caar (precondition->intervals `(λ (x) TRUE) (list repr) repr)))))

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
  (define range (get-range repr))
  (define incr-ideal
    (let loop ([curr-lo (ival-lo range)] [curr-hi (ival-hi range)] [param param])   ; increase parameter
      (define param* (cons (car param) (+ (cdr param) 1)))
      (define name* (replace name (car param*) (cdr param*)))
      (define repr* (get-representation name*))
      (define range (get-range repr*))
      (cond
       [(and (bf> (ival-lo range) curr-lo) (bf< (ival-lo range) curr-hi))     ; decreasing range
        (cond
         [(and (bf> curr-lo lo) (bf< curr-hi hi))   ; curr is already big
          #f]
         [(or (bf> (ival-lo range) lo) (bf< (ival-hi range) hi))
          (replace name (car param) (cdr param))]
         [else
          (loop (ival-lo range) (ival-hi range) param*)])]
       [else                                                                  ; increasing range
        (if (and (bf< curr-lo lo) (bf> curr-hi hi))   ; curr is already big
            (replace name (car param) (cdr param))
            (loop (ival-lo range) (ival-hi range) param*))])))

  (define decr-param (if incr-ideal (get-repr-param (get-representation incr-ideal)) param))
  (let loop ([curr-lo (ival-lo range)] [curr-hi (ival-hi range)] [param decr-param])   ; decrease parameter
    (define param* (cons (car param) (- (cdr param) 1)))
    (define name* (replace name (car param*) (cdr param*)))
    (define repr* (get-representation name*))
    (define range (get-range repr*))
    (cond
     [(and (bf> (ival-lo range) curr-lo) (bf< (ival-lo range) curr-hi))     ; decreasing range
      (cond
        [(and (bf> curr-lo lo) (bf< curr-hi hi))   ; curr is already big
        #f]
        [(or (bf> (ival-lo range) lo) (bf< (ival-hi range) hi))
        (replace name (car param) (cdr param))]
        [else
        (loop (ival-lo range) (ival-hi range) param*)])]
     [else                                                                  ; increasing range
      (if (and (bf< curr-lo lo) (bf> curr-hi hi))   ; curr is already big
          (replace name (car param) (cdr param))
          (loop (ival-lo range) (ival-hi range) param*))])))


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
  (define prog (alt-program altn))
  (define expr (program-body prog))
  (define cache (make-hash))
  (analyze expr cache)
  (define-values (expr* range repr)
    (let loop ([expr expr])
      (match expr
       [(list 'if cond ift iff)
        (define-values (ift* rng1 ift-repr) (loop ift))
        (define-values (iff* rng2 iff-repr) (loop iff))
        (values (list 'if cond ift* iff*)
                (hash-ref cache expr)
                ift-repr)]
       [(list (? repr-conv? conv) body)
        (define oprec (operator-info conv 'otype))
        (define-values (body* range _) (loop body))
        (values body* range (get-representation oprec))]
       [(list op args ...)
        (define range (hash-ref cache expr))
        (define-values (args* ranges* reprs*)
          (for/lists (l1 l2 l3) ([arg (in-list args)])
            (loop arg)))
        (define-values (lo hi)
          (for/fold ([lo (ival-lo range)] [hi (ival-hi range)])
                    ([range (in-list ranges*)])
            (values (bfmin lo (ival-lo range))
                    (bfmax hi (ival-hi range)))))
        (cond
         [(bf= lo hi)   ; exact?
          (values (cons op args*) range (first reprs*))]
         [(or (bfinfinite? lo) (bfinfinite? hi))
          (values (cons op args*) range (first reprs*))]
         [else
          (define repr (get-representation (operator-info op 'otype)))
          (define ideal (search lo hi repr))
          (cond
           [ideal
            (define args**
              (for/list ([arg (in-list args*)] [repr (in-list reprs*)])
                (let ([prec (representation-name repr)])
                  (if (equal? ideal prec)
                      arg
                      (begin
                        (generate-conversions `((,prec ,ideal)))
                        (let ([conv (get-repr-conv prec ideal)])
                          (list conv arg)))))))
            (values (cons op args**) range (get-representation ideal))]
           [else    ; something failed
            (values (cons op args*) range (first reprs*))])])]
       [(? variable?) (values expr (hash-ref cache expr) (*output-repr*))]
       [(? constant?) (values expr (hash-ref cache expr) (*output-repr*))]
       [(? number?) (values expr (hash-ref cache expr) (*output-repr*))])))

  `(λ ,(program-variables prog) ,expr*))
