#lang racket

(require "../common.rkt" "../errors.rkt" "../points.rkt" "../alternative.rkt"
         "../formats/test.rkt" "../sandbox.rkt")
(provide run-herbie)

(define (read-fpcore name port)
  (with-handlers
      ([(or/c exn:fail:user? exn:fail:read?)
        (λ (e)
          ((error-display-handler) (exn-message e) e)
          (read-fpcore name port))])
    (define input (read-syntax name port))
    (if (eof-object? input) eof (parse-test input))))

(define (herbie-input? fname)
  (or (not fname) ; Command line
      (and
       (not (file-name-from-path fname))
       (directory-exists? fname)) ; Directory of files
      (and
       (file-name-from-path fname)
       (regexp-match? #rx"\\.fpcore" (file-name-from-path fname))
       (file-exists? fname)))) ; Herbie input format 1 or 2

(define (in-herbie-files files)
  (if (null? files)
      (in-port (curry read-fpcore "stdin") (current-input-port))
      (all-herbie-tests files)))


(define (all-herbie-tests files)
  (apply append
   (for/list ([file files])
     (if (directory-exists? file)
         (all-herbie-tests (filter herbie-input? (directory-list file #:build? #t)))
         (call-with-input-file file
           (λ (port)
             (define file* (if (string? file) (string->path file) file))
             (port-count-lines! port)
             (sequence->list (in-port (curry read-fpcore file*) port))))))))


(define (in-herbie-output files #:seed seed)
  (eprintf "Seed: ~a\n" seed)
  (sequence-map
   (λ (test) (get-test-result test #:seed seed))
   (in-herbie-files files)))


(define (run-herbie files)
  (define seed (get-seed))
  (with-handlers ([exn:break? (λ (e) (exit 0))])
    (for ([output (in-herbie-output files #:seed seed)] [idx (in-naturals)]
          #:when output)
      (match output
        [(test-result test time bits start-alt end-alt points exacts
                      start-est-error end-est-error newpoint newexacts
                      start-error end-error target-error timeline)
         (eprintf "[ ~ams]\t~a\t(~a→~a)\n"
                  (~a time #:width 8)
                  (test-name test)
                  (~r (errors-score start-error) #:min-width 2 #:precision 1)
                  (~r (errors-score end-error) #:min-width 2 #:precision 1))
         (printf "~a\n" (unparse-test (alt-program end-alt)))]
        [(test-failure test bits exn time timeline)
         (eprintf "[   CRASH   ]\t~a\n" (test-name test))
         (printf ";; Crash in ~a\n" (test-name test))
         ((error-display-handler) (exn-message exn) exn)]
        [(test-timeout test bits time timeline)
         (eprintf "[  timeout  ]\t~a\n" (test-name test))
         (printf ";; ~as timeout in ~a\n;; use --timeout to change timeout\n" (/ time 1000) (test-name test))]))))
