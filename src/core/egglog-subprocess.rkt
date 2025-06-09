#lang racket

(require "../config.rkt")

(provide create-new-egglog-subprocess
         send-to-egglog
         send-to-egglog-unsound-detection)

;; High-level function that writes the program to a file, runs it then returns output
;;
;; If the flag is set to dump the egglog file, since a new subprocess is starting, we can
;;  create a new file to dump the egglog program is and set it
(define (create-new-egglog-subprocess)
  (define egglog-path
    (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

  ; TODO : "RUST_BACKTRACE=1"
  (define-values (egglog-process egglog-output egglog-in err) (subprocess #f #f #f egglog-path))

  (values egglog-process egglog-output egglog-in err))

(define (send-to-egglog commands
                        egglog-process
                        egglog-output
                        egglog-in
                        err
                        dump-file
                        #:num-extracts [num-extracts 0])

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

    ;; Return each S-expr based on the numer of extractions
    (for/list ([i (in-range num-extracts)])
      (read egglog-output))))

(define (send-to-egglog-unsound-detection commands
                                          egglog-process
                                          egglog-output
                                          egglog-in
                                          err
                                          dump-file)
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
  (when (find-executable-path "egglog")
    (define-values (egglog-process egglog-output egglog-in err) (create-new-egglog-subprocess))

    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_) (void))])
                (for ([line (in-lines err)])
                  (printf "[egglog-log] ~a\n" line)))))

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
    (send-to-egglog first-commands egglog-process egglog-output egglog-in err #f)

    ; Has extract 1 thing
    (define second-commands (list '(extract (const1))))

    (define lines1
      (send-to-egglog second-commands egglog-process egglog-output egglog-in err #f #:num-extracts 1))
    (printf "\noutput-vals1 : ~a\n\n" lines1)

    ;; Print size

    (define print-size-commands (list '(print-size) '(run unsound-rule 1) '(extract (unsound))))

    (define-values (node-values unsound?)
      (send-to-egglog-unsound-detection print-size-commands
                                        egglog-process
                                        egglog-output
                                        egglog-in
                                        err
                                        #f))

    (for ([line node-values]
          #:when (> (string-length line) 0))
      (printf "Line : ~a\n" line)
      (printf "string? : ~a\n\n" (string? line)))
    (printf "\nUnsound : ~a\n\n" unsound?)

    (printf "num-nodes : ~a\n" (calculate-nodes node-values))

    ;; last two
    (define third-commands (list '(extract (const2)) '(extract (const3))))

    (define lines2
      (send-to-egglog third-commands egglog-process egglog-output egglog-in err #f #:num-extracts 2))
    (printf "\noutput-vals2 : ~a\n\n" lines2)

    (close-output-port egglog-in)
    (close-input-port egglog-output)
    (close-input-port err)

    (subprocess-wait egglog-process)

    (unless (eq? (subprocess-status egglog-process) 'done)
      (subprocess-kill egglog-process #f))))

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
