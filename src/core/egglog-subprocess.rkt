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
  (close-input-port (egglog-subprocess-error subproc))
  (subprocess-wait (egglog-subprocess-process subproc))
  (unless (eq? (subprocess-status (egglog-subprocess-process subproc)) 'done)
    (subprocess-kill (egglog-subprocess-process subproc) #f)))

;; High-level function that writes the program to a file, runs it then returns output
;;
;; If the flag is set to dump the egglog file, creates a new dump file in dump-egglog/ directory
(define (create-new-egglog-subprocess [label #f])
  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  (define-values (egglog-process egglog-output egglog-in err) (subprocess #f #f #f egglog-path))

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

  (define egglog-program (apply ~s #:separator "\n" commands))

  (when dump-file
    (for ([expr commands])
      (pretty-print expr dump-file 1)))

  (with-handlers ([exn:fail? (lambda (exn)
                               (printf "Egglog command failed with exception:\n~a\n"
                                       (exn-message exn))

                               (define stdout-output (port->string egglog-output))
                               (define stderr-output (port->string err))

                               (printf "Stdout:\n~a\n" stdout-output)
                               (printf "Stderr:\n~a\n" stderr-output)

                               (unless (subprocess-status egglog-process)
                                 (subprocess-kill egglog-process #t))

                               ; Reraise the exception
                               (raise exn))])

    (displayln egglog-program egglog-in)
    (flush-output egglog-in)))

;; Send extract commands and read results
(define (egglog-extract subproc extract-commands)
  (apply egglog-send subproc extract-commands)
  (match-define (egglog-subprocess egglog-process egglog-output egglog-in err dump-file) subproc)
  (for/list ([i (in-range (length extract-commands))])
    (read egglog-output)))

(define (egglog-send-unsound-detection subproc commands)
  (match-define (egglog-subprocess egglog-process egglog-output egglog-in err dump-file) subproc)

  (define egglog-program (apply ~s #:separator "\n" commands))

  (when dump-file
    (for ([expr commands])
      (pretty-print expr dump-file 1)))

  (with-handlers ([exn:fail? (lambda (exn)
                               (printf "Egglog command failed with exception:\n~a\n"
                                       (exn-message exn))

                               (define stdout-output (port->string egglog-output))
                               (define stderr-output (port->string err))

                               (printf "Stdout:\n~a\n" stdout-output)
                               (printf "Stderr:\n~a\n" stderr-output)

                               (unless (subprocess-status egglog-process)
                                 (subprocess-kill egglog-process #t))

                               ; Reraise the exception
                               (raise exn))])

    (displayln egglog-program egglog-in)
    (flush-output egglog-in)

    (define lines '())
    (define unsound? #t)

    (let loop ()
      (define line (read-line egglog-output 'any))
      (cond
        [(or (equal? line "true") (equal? line "false")) (set! unsound? (equal? line "true"))]
        [else
         (set! lines (cons line lines))

         (loop)]))

    (values lines unsound?)))
