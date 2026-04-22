#lang racket

(require racket/runtime-path
         "../config.rkt"
         "../utils/timeline.rkt")

(define-runtime-path egglog-timed-binary
  (build-path 'up 'up "egglog-timed" "target" "release" "egglog-timed"))

(provide (struct-out egglog-subprocess)
         create-new-egglog-subprocess
         egglog-send
         egglog-extract
         egglog-multi-extract
         egglog-subprocess-close
         with-egglog-phase
         time-egglog-surround)

;; Struct to hold egglog subprocess handles
(struct egglog-subprocess (process output input error dump-file) #:transparent)

;; Wraps an egglog phase, emitting one egglog-phase row with the phase's
;; wall duration. Per-send engine time lands on egglog-send rows; the
;; report derives per-phase engine/process totals by grouping sends on
;; the (stage, phase) keys. Must be called inside a running timeline
;; event (e.g. after timeline-event!).
(define-syntax-rule (with-egglog-phase phase-name body ...)
  (let ([wall-start (current-inexact-milliseconds)])
    (begin0 (parameterize ([*egglog-phase* phase-name])
              (let ()
                body ...))
      (timeline-push! 'egglog-phase
                      (- (current-inexact-milliseconds) wall-start)
                      (*timing-stage*)
                      phase-name))))

;; Outer wrapper used from patch.rkt to label the entire Herbie-side egglog
;; call (runner construction + run-egglog dispatch + post-processing).
(define-syntax-rule (time-egglog-surround body ...)
  (with-egglog-phase "surround" body ...))

;; Match `(done <nanos>)` or `(error <nanos>)`, capturing the nanosecond
;; engine-time measurement reported by the egglog-timed wrapper. `<nanos>`
;; is the wall time `parse_and_run_program` spent inside the subprocess;
;; missing digits (`(done)`) match zero.
(define done-marker-px #px"^\\((done|error)(?:\\s+(\\d+))?\\)$")

;; Close all ports and wait for/kill the subprocess
(define (egglog-subprocess-close subproc)
  (close-output-port (egglog-subprocess-input subproc))
  (close-input-port (egglog-subprocess-output subproc))
  (subprocess-wait (egglog-subprocess-process subproc))
  (unless (eq? (subprocess-status (egglog-subprocess-process subproc)) 'done)
    (subprocess-kill (egglog-subprocess-process subproc) #f)))

;; High-level function that writes the program to a file, runs it then returns output
;;
;; If the flag is set to dump the egglog file, creates a new dump file in dump-egglog/ directory
(define (create-new-egglog-subprocess [label #f])
  (unless (file-exists? egglog-timed-binary)
    (error 'create-new-egglog-subprocess
           "egglog-timed not built; run `make egglog-timed` (expected at ~a)"
           egglog-timed-binary))

  (define-values (egglog-process egglog-output egglog-in err)
    (subprocess #f #f (current-error-port) egglog-timed-binary))

  ;; Create dump file if flag is set
  (define dump-file
    (cond
      [(flag-set? 'dump 'egglog)
       (define dump-dir "dump-egglog")
       (unless (directory-exists? dump-dir)
         (make-directory dump-dir))
       (define name
         (for/first ([i (in-naturals)]
                     #:unless
                     (file-exists? (build-path dump-dir (format "~a~a.egg" (if label label "") i))))
           (build-path dump-dir (format "~a~a.egg" (if label label "") i))))
       (open-output-file name #:exists 'replace)]
      [else #f]))

  (egglog-subprocess egglog-process egglog-output egglog-in err dump-file))

(define (egglog-send subproc . commands)
  (match-define (egglog-subprocess egglog-process egglog-output egglog-in err dump-file) subproc)

  (when dump-file
    (for ([expr commands])
      (pretty-print expr dump-file 1))
    (flush-output dump-file))

  (for/list ([command (in-list commands)])
    (define cmd-tag
      (match command
        [(cons head _) (~a head)]
        [_ (~a command)]))

    (define command-str
      (let ([p (open-output-string)])
        (writeln command p)
        (get-output-string p)))

    (define wall-start (current-inexact-milliseconds))

    (display command-str egglog-in)
    (flush-output egglog-in)

    ;; Read response lines until we hit the egglog-timed wrapper's
    ;; `(done <nanos>)` / `(error <nanos>)` terminator. Engine time
    ;; is the nanos field (wall time the Rust subprocess spent inside
    ;; parse_and_run_program), measured in-process with Instant::now().
    (define-values (result engine-nanos)
      (let loop ([out '()])
        (define next (read-line egglog-output))
        (when (eof-object? next)
          (error 'egglog-send "egglog-timed closed stdout unexpectedly (cmd=~a)" cmd-tag))
        (cond
          [(regexp-match done-marker-px next)
           =>
           (lambda (m)
             (define ns (string->number (or (third m) "0")))
             (values (reverse out) (or ns 0)))]
          [else (loop (cons next out))])))

    (define wall (- (current-inexact-milliseconds) wall-start))
    (define engine (/ engine-nanos 1000000.0))

    (timeline-push! 'egglog-send
                    1
                    wall
                    engine
                    (*timing-stage*)
                    (*egglog-phase*)
                    cmd-tag)

    result))

;; Send extract commands and read results
(define (egglog-extract subproc extract-command)
  (match-define (list "(" results ... ")") (first (egglog-send subproc extract-command)))
  (for/list ([result (in-list results)])
    (read (open-input-string result))))

(define (egglog-multi-extract subproc extract-command)
  (define raw-lines (first (egglog-send subproc extract-command)))
  (define combined (string-join raw-lines " "))
  (define parsed (read (open-input-string combined)))
  (for/list ([result-list (in-list parsed)])
    (for/list ([result (in-list result-list)])
      result)))
