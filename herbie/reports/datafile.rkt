#lang racket

(require racket/date)
(require json)
(require "../common.rkt")

(provide
 git-command
 (struct-out table-row) (struct-out report-info)
 make-report-info read-datafile write-datafile)

(define (git-command #:default [default ""] gitcmd . args)
  (if (directory-exists? ".git")
      (let ([cmd (format "git ~a ~a" gitcmd (string-join args " "))])
        (string-trim (write-string (system cmd))))
      default))

(struct table-row
  (name status start result target inf- inf+ result-est vars input output time bits link) #:prefab)

(struct report-info
  (date commit branch seed flags points iterations note tests) #:prefab)

(define (make-report-info tests #:note [note ""])
  (report-info (current-date)
               (git-command "rev-parse" "HEAD")
               (git-command "rev-parse" "--abbrev-ref" "HEAD")
               (pseudo-random-generator->vector (current-pseudo-random-generator))
               (*flags*)
               (*num-points*)
               (*num-iterations*)
               note
               tests))

(define (write-datafile file info)
  (define (simplify-test test)
    (match test
      [(table-row name status start-bits end-bits target-bits
                  inf- inf+ end-est vars input output time bits link)
       (make-hash
        `((name . ,name)
          (status . ,status)
          (start . ,start-bits)
          (end . ,end-bits)
          (target . ,target-bits)
          (ninf . ,inf-)
          (pinf . ,inf+)
          (end-est . ,end-est)
          (vars . ,(if vars (map symbol->string vars) #f))
          (input . ,(~a input))
          (output . ,(~a output))
          (time . ,time)
          (bits . ,bits)
          (link . ,(~a link))))]))
  
  (define data
    (match info
      [(report-info date commit branch seed flags points iterations note tests)
       (make-hash
        `((date . ,(date->seconds date))
          (commit . ,commit)
          (branch . ,branch)
          (seed . ,(~a seed))
          (flags .
                 ,(for*/list ([rec (hash->list flags)] [fl (cdr rec)])
                    (format "~a:~a" (car rec) fl)))
          (points . ,points)
          (iterations . ,iterations)
          (note . ,note)
          (tests . ,(map simplify-test tests))))]))

  (call-with-output-file file (curry write-json data) #:exists 'replace))

(define (read-datafile file)
  (define (parse-string s)
    (if s
        (call-with-input-string s read)
        #f))
  
  (let* ([json (call-with-input-file file read-json)]
         [get (λ (field) (hash-ref json field))])
    (report-info (seconds->date (get 'date)) (get 'commit) (get 'branch) (get 'seed)
                 (get 'flags) (get 'points) (get 'iterations) (hash-ref json 'note #f)
                 (for/list ([test (get 'tests)])
                   (let ([get (λ (field) (hash-ref test field))])
                                        ; TODO: ignoring the result-est
                     (table-row (get 'name) (get 'status) (get 'start) (get 'end) (get 'target)
                                (get 'ninf) (get 'pinf) (hash-ref json 'end-est 0)
                                (get 'vars) (parse-string (get 'input)) (parse-string (get 'output))
                                (get 'time) (get 'bits) (get 'link)))))))
