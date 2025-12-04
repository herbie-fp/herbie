#lang racket

(require "../config.rkt")

(provide (struct-out egglog-subprocess)
         create-new-egglog-subprocess
         egglog-send
         egglog-extract
         egglog-send-unsound-detection
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
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define-values (egglog-process egglog-output egglog-in err)
    (subprocess #f #f (current-error-port) egglog-path "--mode=interactive"))

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
      (pretty-print expr dump-file 1)))

  (for/list ([command (in-list commands)])
    (writeln command egglog-in)
    (flush-output egglog-in)

    (let loop ([out '()])
      (define next (read-line egglog-output))
      (if (equal? next "(done)")
          (reverse out)
          (loop (cons next out))))))

;; Send extract commands and read results
(define (egglog-extract subproc extract-command)
  (match-define (list "(" results ... ")") (first (egglog-send subproc extract-command)))
  (for/list ([result (in-list results)])
    (read (open-input-string result))))

(define (egglog-send-unsound-detection subproc commands)
  (match commands
    [`((push) (run-schedule (repeat 1 ,(or 'rewrite 'const-fold)))
              (print-size)
              (run bad-merge-rule 1)
              (extract (bad-merge?)))
     (void)])

  (match-define (list (list) (list) (list lines ...) (list) (list unsound?))
    (apply egglog-send subproc commands))
  (values lines (match unsound? ["false" #f] ["true" #t])))
