#lang racket

(require racket/file)

(provide run-egglog
         (struct-out egglog-program))

;; Track the entire Egglog program in one go by "converting" into racket based code
;; TODO : prelude, rules, expressions, extractions
(struct egglog-program (program) #:prefab)

(define program-to-egglog "program-to-egglog.egg")

(define std-out-file "stdout.txt")
(define std-err-file "stdout.txt")

; Types handled
; - rationals
; - string
(define (write-program-to-egglog program)
  (with-output-to-file program-to-egglog
    #:exists 'replace
    (lambda ()
      (for-each writeln program))))


(define (write-output-to-file output filename)
  (with-output-to-file filename
    #:exists 'replace
    (lambda ()
      (displayln output))))

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
  (write-output-to-file stdout-content std-out-file)
  (write-output-to-file stderr-content std-err-file)

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))


;; High-level function that writes the program to a file and then runs it
;;; TODO : Faster way to read/write from/to files
(define (run-egglog program-struct)
  (write-program-to-egglog (egglog-program-program program-struct))

  (process-egglog program-to-egglog)
  
  (cons (file->string std-out-file) (file->string std-out-file)))
