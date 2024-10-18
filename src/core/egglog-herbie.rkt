#lang racket

(require racket/file)

(provide run-egglog
         (struct-out egglog-program))

;; Track the entire Egglog program in one go by "converting" into racket based code
;; TODO : prelude, rules, expressions, extractions
(struct egglog-program (program) #:prefab)

(define program-to-egglog "program-to-egglog.egg")

; Types handled
; - rationals
; - string
(define (write-program-to-egglog program)
  (with-output-to-file program-to-egglog #:exists 'replace (lambda () (for-each writeln program))))

(define (process-egglog egglog-filename)
  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define curr-path (build-path (current-directory) egglog-filename))

  (define-values (sp out in err) (subprocess #f #f #f egglog-path curr-path))

  (subprocess-wait sp)

  (define stdout-content (port->string out))
  (define stderr-content (port->string err))

  (close-input-port out)
  (close-output-port in)
  (close-input-port err)

  (cons stdout-content stderr-content))

;; High-level function that writes the program to a file, runs it then returns output
;;; TODO : Faster way to read/write from/to files
(define (run-egglog program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))

  (process-egglog program-to-egglog))

;; TODO:
;; 1. Add make-egglog-runner
;; Constructs an egg runner.
;;
;; The schedule is a list of pairs specifying
;;  - a list of rules
;;  - scheduling parameters:
;;     - node limit: `(node . <number>)`
;;     - iteration limit: `(iteration . <number>)`
;;     - constant fold: `(const-fold? . <boolean>)` [default: #t]
;;     - scheduler: `(scheduler . <name>)` [default: backoff]
;;        - `simple`: run all rules without banning
;;        - `backoff`: ban rules if the fire too much
(define (make-egg-runner batch roots reprs schedule #:context [ctx (*context*)])
  (define (oops! fmt . args)
    (apply error 'verify-schedule! fmt args))
  ; verify the schedule
  (for ([instr (in-list schedule)])
    (match instr
      [(cons rules params)
       ;; `run` instruction
       (unless (and (list? rules) (andmap rule? rules))
         (oops! "expected list of rules: `~a`" rules))
       (for ([param (in-list params)])
         (match param
           [(cons 'node (? nonnegative-integer?)) (void)]
           [(cons 'iteration (? nonnegative-integer?)) (void)]
           [(cons 'const-fold? (? boolean?)) (void)]
           [(cons 'scheduler mode)
            (unless (set-member? '(simple backoff) mode)
              (oops! "in instruction `~a`, unknown scheduler `~a`" instr mode))]
           [_ (oops! "in instruction `~a`, unknown parameter `~a`" instr param)]))]
      [_ (oops! "expected `(<rules> . <params>)`, got `~a`" instr)]))
  ; make the runner
  (egg-runner batch roots reprs schedule ctx))

;; 2. Add run-egglog
;; Runs egg using an egg runner.
;;
;; Argument `cmd` specifies what to get from the e-graph:
;;  - single extraction: `(single . <extractor>)`
;;  - multi extraction: `(multi . <extractor>)`
;;  - proofs: `(proofs . ((<start> . <end>) ...))`
(define (run-egg runner cmd)
  ;; Run egg using runner
  (define ctx (egg-runner-ctx runner))
  (define-values (root-ids egg-graph)
    (egraph-run-schedule (egg-runner-batch runner)
                         (egg-runner-roots runner)
                         (egg-runner-schedule runner)
                         ctx))
  ; Perform extraction
  (match cmd
    [`(single . ,extractor) ; single expression extraction
     (define regraph (make-regraph egg-graph))
     (define reprs (egg-runner-reprs runner))
     (when (flag-set? 'dump 'egg)
       (regraph-dump regraph root-ids reprs))
     (define extract-id (extractor regraph))
     (define finalize-batch (last extract-id))
     ; (Listof (Listof batchref))
     (define out
       (for/list ([id (in-list root-ids)]
                  [repr (in-list reprs)])
         (regraph-extract-best regraph extract-id id repr)))
     ; commit changes to the batch
     (finalize-batch)
     out]
    [`(multi . ,extractor) ; multi expression extraction
     (define regraph (make-regraph egg-graph))
     (define reprs (egg-runner-reprs runner))
     (when (flag-set? 'dump 'egg)
       (regraph-dump regraph root-ids reprs))
     (define extract-id (extractor regraph))
     (define finalize-batch (last extract-id))
     ; (Listof (Listof batchref))
     (define out
       (for/list ([id (in-list root-ids)]
                  [repr (in-list reprs)])
         (regraph-extract-variants regraph extract-id id repr)))
     ; commit changes to the batch
     (finalize-batch)
     out]
    [`(proofs . ((,start-exprs . ,end-exprs) ...)) ; proof extraction
     (for/list ([start (in-list start-exprs)]
                [end (in-list end-exprs)])
       (unless (egraph-expr-equal? egg-graph start end ctx)
         (error 'run-egg
                "cannot find proof; start and end are not equal.\n start: ~a \n end: ~a"
                start
                end))
       (define proof (egraph-get-proof egg-graph start end ctx))
       (when (null? proof)
         (error 'run-egg "proof extraction failed between`~a` and `~a`" start end))
       proof)]
    [`(equal? . ((,start-exprs . ,end-exprs) ...)) ; term equality?
     (for/list ([start (in-list start-exprs)]
                [end (in-list end-exprs)])
       (egraph-expr-equal? egg-graph start end ctx))]
    [_ (error 'run-egg "unknown command `~a`\n" cmd)]))
