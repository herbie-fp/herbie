#lang racket

(define program-to-egglog "program-to-egglog.egg")

(define egglog-output "egglog-output")

; prelude, rules, expressions, extractions

;; Track the entire Egglog program in one go by "converting" into racket based code
(define egglog-program 
  '((datatype M
     (Num Rational))
   (datatype MTy
     (Numf64 Rational))
    (function typed-id (M String) MTy)
    (rule ((= e (Num n)))
          ((let ty "binary64")
            (let ety (Numf64 n))
            (union (typed-id e ty) ety)))
    (let e (Num (rational 1 1)))
    (run 3)
    (extract e)))

(define (is-simple-expression? expr)
  (or (symbol? expr) (number? expr) (string? expr) (boolean? expr)))

(define (write-egglog-expression expr out)
  (cond
    [(string? expr) (fprintf out "\"~s\"" expr)]  ; Write strings with quotes
    [(symbol? expr) (fprintf out "~s" expr)]      ; Write symbols as is
    [(list? expr)    ; If it's a list, recursively write each part
     (fprintf out "(")
     (for-each (lambda (sub-expr)
                 (write-egglog-expression sub-expr out)
                 (fprintf out " "))  ; Add a space between list elements
               expr)
     (fprintf out ")")]
    [else (fprintf out "~s" expr)]))  ; Fallback for other types

; ;; Function to write the program to a filename
; (define (write-program-to-egglog program)
;   (with-output-to-file program-to-egglog
;     #:exists 'replace
;     (lambda ()
;       (for-each (lambda (expr)
;                   (write-egglog-expression expr (current-output-port))  ; Write each expression
;                   (newline))  ; Add a newline after each expression for readability
;                 program))))

    
;       (for-each write program))))

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


(define (run-egglog egglog-filename)
  (define egglog-path (or (find-executable-path "egglog")
                          (error "egglog executable not found in PATH")))
                          
  (define curr-path (build-path (current-directory) egglog-filename))

  (define-values (sp out in err)
    (subprocess #f #f #f egglog-path curr-path))

  (subprocess-wait sp)

  (displayln (port-closed? out))
  (displayln (port-closed? err))

  ; (sleep 1)
  ;; does subprocess-wait close ports

  (define stdout-content (port->string out)) 
  (define stderr-content (port->string err))

  ; Write content to file
  (write-output-to-file stdout-content "stdout.txt")
  (write-output-to-file stderr-content "stderr.txt")

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))



;; High-level function that writes the program to a file and then runs it
(define (run-egglog-program program)
  (write-program-to-egglog program)
  (run-egglog program-to-egglog)
  ; read stdout and stderr
  
  )

(run-egglog-program egglog-program)

;; Can you capture stdout from program
;;      We have data and we done

;; string ports : open-output-string : use parametrieto override ports

;; Make egglog program a struct -> list of expressions
;;  Embed arbitrary data -> make type egglog-prog

;; Function to run the Egglog file using the system's Egglog engine
; (define (run-egglog filename)
;   (with-output-to-string 
;     (lambda () 
;         (system (string-append "egglog " filename)))))


; boolean
; f64
; fn
; i64
; macros
; map
; set
; unit -> '()
; vector


; rationals
; string