#lang racket
(require racket/string)

(define egglog-path
  (or (find-executable-path "egglog") (error "egglog executable not found in PATH")))

(define (create-new-egglog-subprocess)
  (define-values (egglog-process egglog-output egglog-in err) (subprocess #f #f #f egglog-path))

  (values egglog-process egglog-output egglog-in err))

(define (send-to-egglog commands egglog-process egglog-output egglog-in err)
  (define egglog-program (apply ~s #:separator "\n" commands))

  (with-handlers ([exn:fail? (lambda (exn)
                               ; Handle the exception - print diagnostic info and re-raise
                               (printf "Egglog command failed with exception:\n~a\n"
                                       (exn-message exn))

                               (define stdout-output (read-available-output egglog-output))
                               (define stderr-output (read-available-output err))

                               (printf "Stdout:\n~a\n" stdout-output)
                               (printf "Stderr:\n~a\n" stderr-output)

                               ; Kill process if still running
                               (when (not (subprocess-status egglog-process))
                                 (subprocess-kill egglog-process #t))

                               ; Re-raise the exception
                               (raise exn))])

    (displayln egglog-program egglog-in)
    (flush-output egglog-in)
    (sleep 0.1)

    (define partial-output (read-available-output egglog-output))
    (define output-lines (string-split partial-output "\n"))

    (for ([line (in-list output-lines)])
      (printf "Line: ~a\n" line))

    ; (printf "Egglog error:\n~a" (read-available-output err))

    output-lines))

(define (close-subprocess egglog-process egglog-output egglog-in err)
  (close-output-port egglog-in)
  (close-input-port egglog-output)
  (close-input-port err)

  (subprocess-wait egglog-process)

  (unless (eq? (subprocess-status egglog-process) 'done)
    (subprocess-kill egglog-process #f)))

(define (read-available-output port)
  (define out '())
  (let loop ()
    (if (char-ready? port)
        (begin
          (set! out (cons (read-char port) out))
          (loop))
        (list->string (reverse out)))))

(module+ test
  (require rackunit)

  (define first-commands
    (list '(datatype Expr (Var String :cost 150) (Add Expr Expr :cost 200))
          '(constructor const1 () Expr :unextractable)
          '(constructor const2 () Expr :unextractable)
          '(constructor const3 () Expr :unextractable)
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
          '(run init 1)
          '(extract (const1))))

  (define second-commands (list '(extract (const2)) '(extract (const3))))

  (define-values (egglog-process egglog-output egglog-in err) (create-new-egglog-subprocess))

  (define lines1 (send-to-egglog first-commands egglog-process egglog-output egglog-in err))
  (printf "\noutput-vals1 : ~a\n\n" lines1)

  (define lines2 (send-to-egglog second-commands egglog-process egglog-output egglog-in err))
  (printf "\noutput-vals2 : ~a\n\n" lines2)

  (close-output-port egglog-in)
  (close-input-port egglog-output)

  (define err-results (read-string 1000 err))
  (close-input-port err)

  (printf "Egglog logs:\n~a" err-results)

  (subprocess-wait egglog-process)

  (unless (eq? (subprocess-status egglog-process) 'done)
    (subprocess-kill egglog-process #f))

  ;   (close-subprocess egglog-process egglog-output egglog-in err)
  )
