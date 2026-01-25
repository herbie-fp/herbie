#lang racket

(require racket/set
         (only-in ffi/vector u32vector? u32vector->list)
         "../src/config.rkt"
         "../src/core/egg-herbie.rkt"
         "../src/core/rules.rkt"
         "../src/syntax/batch.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/syntax/matcher.rkt"
         "../src/syntax/platform.rkt"
         "../src/syntax/syntax.rkt"
         "../src/syntax/types.rkt")

(define (read-sexprs path)
  (call-with-input-file path (λ (in) (sequence->list (in-port read in)))))

(define (rule-table)
  (define rules (append (*rules*) (*sound-removal-rules*)))
  (for/hash ([ru (in-list rules)])
    (values (rule-name ru) ru)))

(define (rewrite-once expr input output)
  (define (apply-at expr)
    (define results '())
    (define m (pattern-match input expr))
    (when m
      (set! results (cons (pattern-substitute output m) results)))
    (match expr
      [(approx spec impl)
       (define spec* (apply-at spec))
       (define impl* (apply-at impl))
       (append results (map (λ (s) (approx s impl)) spec*) (map (λ (i) (approx spec i)) impl*))]
      [(hole precision spec)
       (define spec* (apply-at spec))
       (append results (map (λ (s) (hole precision s)) spec*))]
      [(list op args ...)
       (define args-list args)
       (define n (length args-list))
       (append
        results
        (apply append
               (for/list ([i (in-range n)])
                 (define arg (list-ref args-list i))
                 (define rewrites (apply-at arg))
                 (for/list ([arg* (in-list rewrites)])
                   (append (list op) (take args-list i) (list arg*) (drop args-list (add1 i)))))))]
      [_ results]))
  (remove-duplicates (apply-at expr)))

(define (strip-annotations expr)
  (let loop ([expr expr])
    (match expr
      [(list (or 'Rewrite=> 'Rewrite<=) _ inner) (loop inner)]
      [(approx spec impl) (approx (loop spec) (loop impl))]
      [(hole precision spec) (hole precision (loop spec))]
      [(list op args ...) (cons op (map loop args))]
      [_ expr])))

(define (find-annotation expr)
  (let loop ([expr expr])
    (match expr
      [(list (or 'Rewrite=> 'Rewrite<=) rule inner)
       (list (if (eq? (car expr) 'Rewrite=>) '=> '<=) rule inner)]
      [(approx spec impl) (or (loop spec) (loop impl))]
      [(hole _ spec) (loop spec)]
      [(list _ args ...)
       (for/or ([arg (in-list args)])
         (loop arg))]
      [_ #f])))

(define (vars-in-batch batch roots)
  (define free-vars (batch-free-vars batch))
  (define all-vars
    (for/fold ([vars (set)]) ([root (in-list roots)])
      (set-union vars (free-vars root))))
  (sort (set->list all-vars) symbol<?))

(define (build-egg-graph batch roots ctx)
  (define reprs (make-list (length roots) (get-representation (*default-precision*))))
  (define runner (make-egraph batch roots reprs '(rewrite) ctx))
  (egg-runner-egg-graph runner))

(define (enode->op+args enode)
  (match enode
    [(list op args ...) (values op args)]
    [(cons op args)
     (cond
       [(u32vector? args) (values op (u32vector->list args))]
       [else (values op (list args))])]
    [_ (values #f #f)]))

(define (index-egraph egg-graph)
  (define atom->ids (make-hash))
  (define op->entries (make-hash))

  (for ([eid (in-list (egraph-eclass-ids egg-graph))])
    (define enodes (egraph-eclass-nodes egg-graph eid))
    (for ([enode (in-vector enodes)])
      (match enode
        [(? number?) (hash-update! atom->ids enode (λ (s) (set-add s eid)) (set))]
        [(? symbol?) (hash-update! atom->ids enode (λ (s) (set-add s eid)) (set))]
        [_
         (define-values (op args) (enode->op+args enode))
         (when op
           (define key (cons op (length args)))
           (hash-update! op->entries key (λ (entries) (cons (cons eid args) entries)) '()))])))
  (values atom->ids op->entries))

(define (egraph-contains-egg-expr? egg-graph egg-expr)
  (define-values (atom->ids op->entries) (index-egraph egg-graph))
  (define memo (make-hash))

  (define (ids-for expr)
    (hash-ref! memo
               expr
               (λ ()
                 (match expr
                   [(? number?) (hash-ref atom->ids expr (set))]
                   [(? symbol?) (hash-ref atom->ids expr (set))]
                   [(list op args ...)
                    (define key (cons op (length args)))
                    (define entries (hash-ref op->entries key '()))
                    (define arg-ids (map ids-for args))
                    (for/fold ([acc (set)]) ([entry (in-list entries)])
                      (match-define (cons eid child-ids) entry)
                      (if (and (= (length child-ids) (length arg-ids))
                               (for/and ([cid (in-list child-ids)]
                                         [ids (in-list arg-ids)])
                                 (set-member? ids cid)))
                          (set-add acc eid)
                          acc))]
                   [_ (set)]))))

  (not (set-empty? (ids-for egg-expr))))

(define (run-rr-check proof-path #:node-limit [node-limit #f])
  (activate-platform! "c")
  (define exprs (read-sexprs proof-path))
  (define start (car exprs))
  (define rules (rule-table))

  (define-values (batch roots)
    (let-values ([(b brfs) (progs->batch (list (strip-annotations start)))])
      (values b brfs)))

  (define repr (get-representation (*default-precision*)))
  (define vars (vars-in-batch batch roots))
  (define ctx (context vars repr (make-list (length vars) repr)))

  (parameterize ([*context* ctx]
                 [*node-limit* (if node-limit
                                   node-limit
                                   (*node-limit*))])
    (define egg-graph (build-egg-graph batch roots ctx))
    (define proof-exprs (map strip-annotations exprs))

    (define (present? expr)
      (define egg-expr (expr->egg-expr (strip-annotations expr) ctx))
      (egraph-contains-egg-expr? egg-graph egg-expr))

    (printf "proof steps: ~a\n" (sub1 (length exprs)))
    (define ok? #t)
    (define present-ok? #t)
    (printf "step 0: present? ~a\n" (if (present? start) "yes" "no"))
    (for ([prev (in-list proof-exprs)]
          [next (in-list (cdr proof-exprs))]
          [idx (in-naturals 1)]
          [prev-raw (in-list exprs)]
          [next-raw (in-list (cdr exprs))])
      (define rewrite (or (find-annotation next-raw) (find-annotation prev-raw)))
      (define dir (and rewrite (first rewrite)))
      (define rule-name (and rewrite (second rewrite)))
      (define ru (and rule-name (hash-ref rules rule-name #f)))
      (define rule-ok?
        (and rewrite
             ru
             (let-values ([(input output) (if (eq? dir '=>)
                                              (values (rule-input ru) (rule-output ru))
                                              (values (rule-output ru) (rule-input ru)))])
               (member next (rewrite-once prev input output)))))
      (when (not rule-ok?)
        (set! ok? #f))
      (define next-present? (present? next))
      (printf "step ~a: rule ~a ~a, present? ~a\n"
              idx
              (or rule-name 'none)
              (if rule-ok? "ok" "invalid")
              (if next-present? "yes" "no"))
      (when (not next-present?)
        (set! present-ok? #f)))
    (unless (and ok? present-ok?)
      (exit 1))))

(module+ main
  (define node-limit #f)
  (command-line
   #:program "rr-check"
   #:once-each
   [("--node-limit") num "Override e-graph node limit." (set! node-limit (string->number num))]
   #:args (proof)
   (run-rr-check proof #:node-limit node-limit)))
