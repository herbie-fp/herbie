#lang racket

(require casio/common)

(provide write-datafile read-datafile)

(define (write-datafile file results)
  (write-file file
    (for ([entry results])
      (write entry)
      (newline))))

(define (read-datafile file)
  (reap [sow]
        (let ([fd (open-input-file file)])
          (let loop ([line (read fd)])
            (when (not (eof-object? line))
              (sow line)
              (loop (read fd)))))))
