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

  ; TODO : "RUST_BACKTRACE=1"
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

(module+ test
  (require rackunit)
  (when (find-executable-path "egglog")
    (define subproc (create-new-egglog-subprocess #f))

    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_) (void))])
                (for ([line (in-lines (egglog-subprocess-error subproc))])
                  (void)))))

    (define first-commands
      (list '(datatype Expr (Var String :cost 150) (Add Expr Expr :cost 200))
            '(constructor const1 () Expr :unextractable)
            '(constructor const2 () Expr :unextractable)
            '(constructor const3 () Expr :unextractable)
            '(function unsound () bool :merge (or old new))
            '(ruleset unsound-rule)
            '(set (unsound) false)
            '(rule ((= (Num c1) (Num c2)) (!= c1 c2)) ((set (unsound) true)) :ruleset unsound-rule)
            '(ruleset init)
            '(rule ()
                   ((let a1 (Var
                             "x")
                      )
                    (set (const1) a1)
                    (let a2 (Var
                             "y")
                      )
                    (set (const2) a2)
                    (let b1 (Add
                             a1
                             a2)
                      )
                    (set (const3) b1))
                   :ruleset
                   init)
            '(run init 1)))

    ; Nothing to output
    (apply egglog-send subproc first-commands)

    ; Has extract 1 thing
    (define lines1 (egglog-extract subproc (list '(extract (const1)))))
    (check-equal? lines1 '((Var "x")))

    ;; Print size

    (define print-size-commands (list '(print-size) '(run unsound-rule 1) '(extract (unsound))))

    (define-values (node-values unsound?) (egglog-send-unsound-detection subproc print-size-commands))
    (check-equal? node-values '("unsound: 1" "const3: 1" "const2: 1" "const1: 1" "Var: 2" "Add: 1" ""))
    (check-false unsound?)

    ;; last two
    (define lines2 (egglog-extract subproc (list '(extract (const2)) '(extract (const3)))))
    (check-equal? lines2 '((Var "y") (Add (Var "x") (Var "y"))))

    (egglog-subprocess-close subproc)))

(define (calculate-nodes lines)
  ;; Don't start from last index, but previous to last index - as last has current unsoundness result
  (define process-lines
    (reverse (if (empty? lines)
                 lines ;; Has no nodes or first iteration
                 (take lines (- (length lines) 1)))))

  ;; Break when we reach the previous unsoundness result -> NOTE: "true" should technically never be reached
  (for/fold ([total_nodes 0]) ([line (in-list process-lines)])
    #:break (or (equal? line "true") (equal? line "false"))

    ;; We need to add the total number of nodes for this one of the format
    ;; "node_name : num_nodes"
    ;; break up into (list node_name num_nodes) with spaces
    (define parts (string-split line ":"))

    ;; Get num_nodes in number
    (define num_nodes
      (if (> (length parts) 0)
          (string->number (string-trim (cadr parts)))
          0))

    (values (+ total_nodes num_nodes))))
