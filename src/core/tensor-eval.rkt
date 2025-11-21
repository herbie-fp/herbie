#lang racket

(require math/bigfloat)
(require "../syntax/types.rkt"
         "../syntax/syntax.rkt"
         "../syntax/platform.rkt"
         "../utils/float.rkt"
         "../utils/errors.rkt")

(provide tensor-context?
         tensor-sample
         tensor-eval
         dim->size
         tensor-error)

(define default-dim-max 4)

(define (tensor-context? ctx)
  (or (tensor-type? (context-repr ctx)) (ormap tensor-type? (context-var-reprs ctx))))

(define (dim->size dim env)
  (match dim
    [(? integer?) dim]
    [(? number?) (inexact->exact dim)]
    [(? symbol?) (hash-ref env dim (lambda () (raise-herbie-error "Unknown dimension ~a" dim)))]
    [_ (raise-herbie-error "Invalid dimension ~a" dim)]))

(define (make-dim-env ctx)
  (define dim-names
    (remove-duplicates (apply append
                              (for/list ([ty (in-list (context-var-reprs ctx))]
                                         #:when (tensor-type? ty))
                                (filter symbol? (tensor-type-dims ty))))))
  (for/hash ([d (in-list dim-names)])
    (values d (add1 (random default-dim-max)))))

(define (random-tensor dims repr env)
  (define sizes (map (curryr dim->size env) dims))
  (let loop ([ds sizes])
    (match ds
      ['() (random-generate repr)]
      [(list d)
       (for/vector ([i (in-range d)])
         (random-generate repr))]
      [(list d rest ...)
       (for/vector ([i (in-range d)])
         (loop rest))])))

(define (build-env ctx dims-env)
  (define vars (context-vars ctx))
  (define types (context-var-reprs ctx))
  (define repr (context-repr ctx))
  (make-immutable-hash (append (for/list ([var (in-list vars)]
                                          [ty (in-list types)])
                                 (cond
                                   [(hash-has-key? dims-env var) (cons var (hash-ref dims-env var))]
                                   [(representation? ty) (cons var (random-generate (or ty repr)))]
                                   [(tensor-type? ty)
                                    (define elem (tensor-type-elem ty))
                                    (define dims (tensor-type-dims ty))
                                    (cons var (random-tensor dims elem dims-env))]
                                   [else (cons var 0)]))
                               (for/list ([(k v) (in-hash dims-env)]
                                          #:unless (memq k vars))
                                 (cons k v)))))

(define (repr-op op repr)
  (match op
    ['+.f64 +]
    ['-.f64 -]
    ['*.f64 *]
    ['/.f64 /]
    ['+.f32 +]
    ['-.f32 -]
    ['*.f32 *]
    ['/.f32 /]
    [else
     (define impl (and (impl-exists? op) (impl-info op 'fl)))
     (or impl (raise-herbie-error "Unsupported operator ~a in tensor eval" op))]))

(define (scalar-cast v repr)
  (real->repr v repr))

(define (exact-op op)
  (match op
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['sqrt sqrt]
    [else #f]))

(define (tensor-ref arr idxs)
  (foldl (lambda (i a) (vector-ref a i)) arr idxs))

(define (tensor-eval expr env repr #:exact? [exact? #f])
  (define (recur e)
    (tensor-eval e env repr #:exact? exact?))
  (match expr
    [(? literal? lit) (literal-value lit)]
    [`(let ([,vars ,vals] ...) ,body)
     (define env+
       (for/fold ([e env])
                 ([v vars]
                  [val vals])
         (hash-set e v (tensor-eval val env repr #:exact? exact?))))
     (tensor-eval body env+ repr #:exact? exact?)]
    [`(let* ([,vars ,vals] ...) ,body)
     (define env+
       (for/fold ([e env])
                 ([v vars]
                  [val vals])
         (hash-set e v (tensor-eval val e repr #:exact? exact?))))
     (tensor-eval body env+ repr #:exact? exact?)]
    [(? number?) expr]
    [(? symbol?) (hash-ref env expr)]
    [`(array ,elems ...) (list->vector (map recur elems))]
    [`(tensor ([,idxs ,sizes] ...) ,body)
     (define sizes* (map recur sizes))
     (define idxlist idxs)
     (define (build ds depth env*)
       (match ds
         ['() (tensor-eval body env* repr #:exact? exact?)]
         [(list d rest ...)
          (for/vector ([i (in-range d)])
            (build rest (add1 depth) (hash-set env* (list-ref idxlist depth) i)))]))
     (build sizes* 0 env)]
    [`(tensor* ([,idxs ,sizes] ...) ([,vars ,inits ,updates] ...) ,body)
     (define sizes* (map recur sizes))
     (define idxlist idxs)
     (define vars* vars)
     (define inits* (map recur inits))
     (define updates* updates)
     (define (loop idxs-left env*)
       (match idxs-left
         ['()
          (define env-with-acc
            (for/fold ([acc-env env*])
                      ([v vars*]
                       [i inits*])
              (hash-set acc-env v i)))
          (define new-acc
            (for/list ([var vars*]
                       [update updates*])
              (recur update)))
          (define env-updated
            (for/fold ([acc env-with-acc]) ([pair (in-list (map cons vars* new-acc))])
              (hash-set acc (car pair) (cdr pair))))
          (for/list ([var vars*])
            (hash-ref env-updated var))]
         [(cons size rest)
          (for/vector ([i (in-range size)])
            (loop rest (hash-set env* (first idxlist) i)))]))
     (list->vector (loop sizes* env))]
    [`(for ([,idxs ,sizes] ...)
        ([,vars ,inits ,updates] ...)
        ,body)
     (define idxs* idxs)
     (define sizes* (map recur sizes))
     (define vars* vars)
     (define init-vals (map recur inits))
     (define (loop-level k env* accs)
       (if (= k (length idxs*))
           accs
           (for/fold ([accs accs]) ([i (in-range (list-ref sizes* k))])
             (define env+ (hash-set env* (list-ref idxs* k) i))
             (define accs-
               (for/list ([var vars*]
                          [update updates])
                 (tensor-eval update
                              (for/fold ([e env+])
                                        ([v vars*]
                                         [a accs])
                                (hash-set e v a))
                              repr
                              #:exact? exact?)))
             accs-)))
     (define result (loop-level 0 env init-vals))
     (if (and (pair? result) (null? (cdr result)))
         (car result)
         result)]
    [`(ref ,arr ,idxs ...) (tensor-ref (recur arr) (map recur idxs))]
    [`(dim ,arr) (vector-length (recur arr))]
    [`(size ,arr ,dims ...)
     (define target (tensor-ref (recur arr) (map recur dims)))
     (if (vector? target)
         (vector-length target)
         0)]
    [`(cast ,arg) (scalar-cast (recur arg) repr)]
    [`(,op ,args ...)
     (define args* (map recur args))
     (cond
       [exact?
        (define op*
          (or (exact-op op) (exact-op (string->symbol (regexp-replace #rx"\\.f[0-9]+" (~a op) "")))))
        (if op*
            (apply op* args*)
            (apply (repr-op op repr) args*))]
       [else (apply (repr-op op repr) args*)])]
    [_ (raise-herbie-error "Unsupported tensor expression ~a" expr)]))

(define (flatten-values v)
  (cond
    [(vector? v) (append-map flatten-values (vector->list v))]
    [else (list v)]))

(define (tensor-error approx exact repr)
  (define flats (flatten-values approx))
  (define flats* (flatten-values exact))
  (unless (= (length flats) (length flats*))
    (raise-herbie-error "Tensor shape mismatch when computing error"))
  (define errs
    (for/list ([a (in-list flats)]
               [e (in-list flats*)])
      (ulp-difference a e repr)))
  (/ (apply + errs) (length errs)))

(define (tensor-sample ctx expr)
  (define dim-env (make-dim-env ctx))
  (define repr (context-repr ctx))
  (define vars (context-vars ctx))
  (define env (build-env ctx dim-env))
  (define point (list->vector (map (curry hash-ref env) vars)))
  (define exact (tensor-eval expr env repr #:exact? #t))
  (list point exact))
