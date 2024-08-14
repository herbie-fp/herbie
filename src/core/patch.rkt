#lang racket

(require "../syntax/platform.rkt"
         "../syntax/syntax.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../utils/alternative.rkt"
         "../utils/common.rkt"
         "../utils/timeline.rkt"
         "egg-herbie.rkt"
         "programs.rkt"
         "rules.rkt"
         "simplify.rkt"
         "taylor.rkt")

(provide generate-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Simplify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lower-approximations approxs approx->prev)
  (timeline-event! 'simplify)

  (define reprs
    (for/list ([approx (in-list approxs)])
      (define prev (hash-ref approx->prev approx))
      (repr-of (alt-expr prev) (*context*))))

  ; generate real rules
  (define rules (real-rules (*simplify-rules*)))
  (define lowering-rules (platform-lowering-rules))

  ; egg runner
  (define schedule
    (if (flag-set? 'generate 'simplify)
        ; if simplify enabled, 2-phases for real rewrites and implementation selection
        `((,rules . ((node . ,(*node-limit*))))
          (,lowering-rules . ((iteration . 1) (scheduler . simple))))
        ; if disabled, only implementation selection
        `((,lowering-rules . ((iteration . 1) (scheduler . simple))))))

  ; run egg
  (define runner (make-egg-runner (map alt-expr approxs) reprs schedule))
  (define simplification-options
    (simplify-batch runner
                    (typed-egg-extractor
                     (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc))))

  ; convert to altns
  (define simplified
    (reap [sow]
          (for ([altn (in-list approxs)]
                [outputs (in-list simplification-options)])
            (match-define (cons _ simplified) outputs)
            (define prev (hash-ref approx->prev altn))
            (for ([expr (in-list simplified)])
              (define spec (prog->spec (alt-expr prev)))
              (sow (alt (approx spec expr) `(simplify ,runner #f #f) (list altn) '()))))))

  (timeline-push! 'count (length approxs) (length simplified))
  simplified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Taylor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define transforms-to-try
  (let ([invert-x (λ (x) `(/ 1 ,x))]
        [exp-x (λ (x) `(exp ,x))]
        [log-x (λ (x) `(log ,x))]
        [ninvert-x (λ (x) `(/ 1 (neg ,x)))])
    `((0 ,identity ,identity) (inf ,invert-x ,invert-x)
                              (-inf ,ninvert-x ,ninvert-x)
                              #;(exp ,exp-x ,log-x)
                              #;(log ,log-x ,exp-x))))

(define (taylor-alts altns)
  (define exprs
    (for/list ([altn (in-list altns)])
      (prog->spec (alt-expr altn))))
  (define free-vars (map free-variables exprs))
  (define vars (list->set (append* free-vars)))

  (reap [sow]
        (for* ([var (in-set vars)]
               [transform-type transforms-to-try])
          (match-define (list name f finv) transform-type)
          (define timeline-stop! (timeline-start! 'series (~a exprs) (~a var) (~a name)))
          (define genexprs (approximate-new exprs var #:transform (cons f finv)))
          (for ([genexpr (in-list genexprs)]
                [altn (in-list altns)]
                [fv (in-list free-vars)]
                #:when (member var fv))
            (for ([n (in-range (*taylor-order-limit*))])
              (define gex (genexpr))
              #;(printf "~a) altn=~a, var=~a, tf=~a, genexpr=~a\n" n altn var transform-type gex)
              (sow (alt gex `(taylor ,name ,var) (list altn) '()))))
          (timeline-stop!))))

(define (taylor-alt altn)
  (define expr (prog->spec (alt-expr altn)))
  (reap [sow]
        (for* ([var (free-variables expr)]
               [transform-type transforms-to-try])
          (match-define (list name f finv) transform-type)
          (define timeline-stop! (timeline-start! 'series (~a expr) (~a var) (~a name)))
          (define genexpr (approximate expr var #:transform (cons f finv)))
          (for ([n (in-range (*taylor-order-limit*))])
            (define gex (genexpr))
            #;(printf "OLD: ~a) altn=~a, var=~a, tf=~a, genexpr=~a\n" n altn var transform-type gex)
            (sow (alt gex `(taylor ,name ,var) (list altn) '())))
          (timeline-stop!))))

(define (spec-has-nan? expr)
  (expr-contains? expr (lambda (term) (eq? term 'NAN))))

(define (run-taylor altns)
  (timeline-event! 'series)
  (timeline-push! 'inputs (map ~a altns))

  (define approx->prev (make-hasheq))
  #;(define approx->prev* (make-hasheq))

  (define approxs
    (reap [sow]
          (for ([approximation (taylor-alts altns)])
            (unless (spec-has-nan? (alt-expr approximation))
              ; here (car (alt-prevs approximation)) is simply an original altn
              (hash-set! approx->prev approximation (car (alt-prevs approximation)))
              (sow approximation)))))

  #;(define approxs
      (reap [sow]
            (for ([altn (in-list altns)])
              (for ([approximation (taylor-alt altn)])
                (unless (spec-has-nan? (alt-expr approximation))
                  (hash-set! approx->prev approximation altn)
                  (sow approximation))))))

  ; Debugging
  #;(for ([(key* value*) (in-hash approx->prev*)])
      (define flag #t)
      (for ([(key value) (in-hash approx->prev)])
        (when (and (equal? (alt-event key) (alt-event key*))
                   (equal? (alt-expr key) (alt-expr key*))
                   (equal? (alt-expr value) (alt-expr value*))
                   (equal? (alt-prevs key) (alt-prevs key*))
                   (equal? (alt-prevs value) (alt-prevs value*)))
          (set! flag #f)))
      (when flag
        (println "mismatch")
        (printf "key=~a, value=~a\n" key* value*)
        (println approx->prev)
        (sleep 10)))
  #;(for ([(key value) (in-hash approx->prev)])
      (define flag #t)
      (for ([(key* value*) (in-hash approx->prev*)])
        (when (and (equal? (alt-event key) (alt-event key*))
                   (equal? (alt-expr key) (alt-expr key*))
                   (equal? (alt-expr value) (alt-expr value*))
                   (equal? (alt-prevs key) (alt-prevs key*))
                   (equal? (alt-prevs value) (alt-prevs value*)))
          (set! flag #f)))
      (when flag
        (println "mismatch2")
        (printf "key=~a, value=~a\n" key value)
        (println approx->prev*)
        (sleep 10)))

  (timeline-push! 'outputs (map ~a approxs))
  (timeline-push! 'count (length altns) (length approxs))
  (lower-approximations approxs approx->prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Recursive Rewrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-rr altns)
  (timeline-event! 'rewrite)

  ; generate required rules
  (define rules (real-rules (*rules*)))
  (define lifting-rules (platform-lifting-rules))
  (define lowering-rules (platform-lowering-rules))

  (define extractor
    (typed-egg-extractor (if (*egraph-platform-cost*) platform-egg-cost-proc default-egg-cost-proc)))

  ; egg schedule (3-phases for mathematical rewrites and implementation selection)
  (define schedule
    `((,lifting-rules . ((iteration . 1) (scheduler . simple)))
      (,rules . ((node . ,(*node-limit*))))
      (,lowering-rules . ((iteration . 1) (scheduler . simple)))))

  ; run egg
  (define exprs (map alt-expr altns))
  (define reprs (map (curryr repr-of (*context*)) exprs))
  (timeline-push! 'inputs (map ~a exprs))
  (define runner (make-egg-runner exprs reprs schedule #:context (*context*)))
  (define variantss (run-egg runner `(multi . ,extractor)))

  ; apply changelists
  (define rewritten
    (reap [sow]
          (for ([variants (in-list variantss)]
                [altn (in-list altns)])
            (for ([variant (in-list (remove-duplicates variants))])
              (sow (alt variant (list 'rr runner #f #f) (list altn) '()))))))
  (timeline-push! 'outputs (map (compose ~a alt-expr) rewritten))
  (timeline-push! 'count (length altns) (length rewritten))
  rewritten)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-candidates exprs)
  ; Starting alternatives
  (define start-altns
    (for/list ([expr (in-list exprs)])
      (define repr (repr-of expr (*context*)))
      (alt expr (list 'patch expr repr) '() '())))
  ; Series expand
  (define approximations (if (flag-set? 'generate 'taylor) (run-taylor start-altns) '()))
  ; Recursive rewrite
  (define rewritten (if (flag-set? 'generate 'rr) (run-rr start-altns) '()))

  (append approximations rewritten))
