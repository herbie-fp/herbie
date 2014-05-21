#lang racket

(provide write-file write-string copy-file-overwriting)

(define-syntax (write-file stx)
  (syntax-case stx ()
    [(_ filename . rest)
     #'(with-output-to-file filename (lambda () . rest) #:exists 'replace)]))

(define-syntax (write-string stx)
  (syntax-case stx ()
    [(_ . rest)
     #'(with-output-to-string (lambda () . rest))]))

(define (copy-file-overwriting src dest)
  "Copies a file, replacing the file at destination if it exists."
  (when (file-exists? dest) (delete-file dest))
  (copy-file src dest))
