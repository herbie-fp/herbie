#lang racket

(require racket/date)
(require json)
(require "../common.rkt")
(require "../float.rkt")

(provide
 (struct-out table-row) (struct-out report-info)
 make-report-info read-datafile write-datafile)

(define (git-command #:default [default ""] gitcmd . args)
  (if (directory-exists? ".git")
      (let ([cmd (format "git ~a ~a" gitcmd (string-join args " "))])
        (or (string-trim (with-output-to-string (λ () (system cmd)))) default))
      default))

(struct table-row
  (name status start result target inf- inf+ result-est vars samplers input output time bits link) #:prefab)

(struct report-info
  (date commit branch seed flags points iterations bit-width note tests) #:prefab #:mutable)

(define (make-report-info tests #:note [note ""] #:seed [seed #f])
  (report-info (current-date)
               (git-command "rev-parse" "HEAD" #:default *herbie-version*)
               (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release")
               (or seed (get-seed))
               (*flags*)
               (*num-points*)
               (*num-iterations*)
               (*bit-width*)
               note
               tests))

(define (write-datafile file info)
  (define (simplify-test test)
    (match test
      [(table-row name status start-bits end-bits target-bits
                  inf- inf+ end-est vars samplers input output time bits link)
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
          (samplers . ,(if samplers (map ~a samplers) #f))
          (input . ,(~a input))
          (output . ,(~a output))
          (time . ,time)
          (bits . ,bits)
          (link . ,(~a link))))]))
  
  (define data
    (match info
      [(report-info date commit branch seed flags points iterations bit-width note tests)
       (make-hash
        `((date . ,(date->seconds date))
          (commit . ,commit)
          (branch . ,branch)
          (seed . ,(~a seed))
          (flags . ,(flags->list flags))
          (points . ,points)
          (iterations . ,iterations)
          (bit_width . ,bit-width)
          (note . ,note)
          (tests . ,(map simplify-test tests))))]))

  (call-with-output-file file (curry write-json data) #:exists 'replace))

(define (flags->list flags)
  (for*/list ([rec (hash->list flags)] [fl (cdr rec)])
    (format "~a:~a" (car rec) fl)))

(define (list->flags list)
  (make-hash
   (for/list ([part (group-by car (map (compose (curry map string->symbol) (curryr string-split ":")) list))])
     (cons (car (first part)) (map cadr part)))))

(define (read-datafile file)
  (define (parse-string s)
    (if s
        (call-with-input-string s read)
        #f))
  
  (let* ([json (call-with-input-file file read-json)]
         [get (λ (field) (hash-ref json field))])
    (report-info (seconds->date (get 'date)) (get 'commit) (get 'branch) (parse-string (get 'seed))
                 (list->flags (get 'flags)) (get 'points) (get 'iterations) (hash-ref json 'bit_width 64)
                 (hash-ref json 'note #f)
                 (for/list ([test (get 'tests)] #:when (hash-has-key? test 'vars))
                   (let ([get (λ (field) (hash-ref test field))])
                     ;; TODO: ignoring the result-est
                     (define vars
                       (match (hash-ref test 'vars)
                         [(list names ...) (map string->symbol names)]
                         [string-lst (parse-string string-lst)]))
                     (define samplers
                       (match (hash-ref test 'samplers #f)
                         ['#f (if vars (map (const 'default) vars) #f)]
                         [(list sampler ...) (map parse-string sampler)]))
                     (table-row (get 'name) (get 'status) (get 'start) (get 'end) (get 'target)
                                (get 'ninf) (get 'pinf) (hash-ref test 'end-est 0)
                                vars samplers
                                (parse-string (get 'input)) (parse-string (get 'output))
                                (get 'time) (get 'bits) (get 'link)))))))
