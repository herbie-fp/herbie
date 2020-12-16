#lang racket

(define (read-lines port)
  (define line (read port))
  (if (equal? line eof)
      empty
      (cons line (read-lines port))))


(define (fpcore-less-than fpcore fpcore2)
  (string>? (~a (rest (rest fpcore))) (~a (rest (rest fpcore2)))))

(define (sort-fpcores fpcores)
  (sort fpcores fpcore-less-than))

(define (no-casts expr)
  (cond
    [(list? expr)
     (andmap no-casts expr)]
    [else (not (equal? expr '!))]))

(module+ main
  (command-line 
   #:program "sort"
   #:args (json-file output-file)
   (define filtered (filter no-casts (sort-fpcores (read-lines (open-input-file json-file)))))
   (unless (empty? filtered)
   (define output (open-output-file output-file #:exists 'replace))
   (for ([line filtered])
     (write line output)))))
