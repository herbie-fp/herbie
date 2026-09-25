#lang racket

(require "../config.rkt")

(provide (struct-out egglog-subprocess)
         create-new-egglog-subprocess
         call-with-egglog-subprocess
         egglog-send
         egglog-send/read
         egglog-extract
         egglog-subprocess-close)

;; Struct to hold egglog subprocess handles
(struct egglog-subprocess (process output input error dump-file) #:transparent)

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
  (define egglog-path
    (or (find-executable-path "egglog-experimental")
        (find-executable-path "egglog")
        (error "egglog-experimental executable not found in PATH")))

  ;; The current custodian owns the process, so shutting the custodian down
  ;; (e.g. a test finishing or timing out, see sandbox.rkt) kills egglog even
  ;; if it is spinning inside a schedule.
  (define-values (egglog-process egglog-output egglog-in err)
    (parameterize ([current-subprocess-custodian-mode 'kill])
      (subprocess #f #f (current-error-port) egglog-path "--mode=interactive")))

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

;; One cached subprocess with `static-commands` already loaded, reused across
;; calls and isolated per call with push/pop. It is owned by the custodian of
;; the call that spawned it: sandbox.rkt runs each test under its own
;; custodian and shuts it down on completion or timeout, which kills the
;; subprocess, so reuse never crosses a test. A stale custodian or different
;; commands (platform or rules changed) respawns. With dump:egglog the dump
;; file therefore records the whole real session: prelude, rules, then each
;; call between push and pop.
(define cached-subprocess #f)
(define cached-key #f)

(define (call-with-egglog-subprocess static-commands label proc)
  (define key (cons (current-custodian) static-commands))
  (unless (equal? key cached-key)
    (when cached-subprocess
      (egglog-subprocess-close cached-subprocess))
    (set! cached-subprocess (create-new-egglog-subprocess label))
    (set! cached-key key)
    (apply egglog-send cached-subprocess static-commands))
  (define subproc cached-subprocess)
  ;; A failure mid-call leaves the subprocess in an unknown protocol state:
  ;; discard it so the next call respawns.
  (with-handlers ([exn:fail? (lambda (e)
                               (set! cached-subprocess #f)
                               (set! cached-key #f)
                               (egglog-subprocess-close subproc)
                               (raise e))])
    (egglog-send subproc '(push))
    (begin0 (proc subproc)
      (egglog-send subproc '(pop)))))

(define (egglog-send subproc . commands)
  (match-define (egglog-subprocess egglog-process egglog-output egglog-in err dump-file) subproc)

  (when dump-file
    (for ([expr commands])
      (pretty-print expr dump-file 1))
    (flush-output dump-file))

  (for/list ([command (in-list commands)])
    (writeln command egglog-in)
    (flush-output egglog-in)

    (let loop ([out '()])
      (define next (read-line egglog-output 'any))
      (if (equal? next "(done)")
          (reverse out)
          (loop (cons next out))))))

;; Send a command whose response is a single s-expression (possibly printed
;; across several lines) and parse it. The response is read directly from the
;; subprocess port: extraction responses can be many megabytes, and collecting
;; them as line strings and joining them before parsing costs 2-3x as much as
;; parsing the port itself.
(define (egglog-send/read subproc command)
  (match-define (egglog-subprocess egglog-process egglog-output egglog-in err dump-file) subproc)

  (when dump-file
    (pretty-print command dump-file 1)
    (flush-output dump-file))

  (writeln command egglog-in)
  (flush-output egglog-in)

  (define result (read egglog-output))
  (when (eof-object? result)
    (error 'egglog-send/read "egglog subprocess closed its output"))
  ;; Drain the rest of the response up to the (done) marker.
  (let loop ()
    (define line (read-line egglog-output 'any))
    (unless (or (eof-object? line) (equal? line "(done)"))
      (loop)))
  result)

;; Send extract commands and read results
(define (egglog-extract subproc extract-command)
  (egglog-send/read subproc extract-command))
