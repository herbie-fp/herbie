#lang racket

(provide (struct-out egglog-program))

;; Track the entire Egglog program in one go by "converting" into racket based code
;; TODO : prelude, rules, expressions, extractions
(struct egglog-program (program) #:prefab)

(define program-to-egglog "program-to-egglog.egg")

(define (write-program-to-egglog program)
  (with-output-to-file program-to-egglog
    #:exists 'replace
    (lambda ()
      (for-each writeln program))))


(define (write-output-to-file output filename)
  (with-output-to-file filename
    #:exists 'replace
    (lambda ()
      (display output))))


(define (process-egglog egglog-filename)
  (define egglog-path (or (find-executable-path "egglog")
                          (error "egglog executable not found in PATH")))
                          
  (define curr-path (build-path (current-directory) egglog-filename))

  (define-values (sp out in err)
    (subprocess #f #f #f egglog-path curr-path))

  (subprocess-wait sp)

  (define stdout-content (port->string out)) 
  (define stderr-content (port->string err))

  ; Write content to file
  (write-output-to-file stdout-content "stdout.txt")
  (write-output-to-file stderr-content "stderr.txt")

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))


;; High-level function that writes the program to a file and then runs it
(define (run-egglog program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))
  (process-egglog program-to-egglog)
  
  ; TODO : read stdout and stderr
  )

;; TODO:
;; 1. Make egglog program a struct -> list of expressions
;;  Embed arbitrary data -> make type egglog-prog


; 2. Types I need
; - rationals
; - string
