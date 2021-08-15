#lang racket

(require racket/date json)
(require "common.rkt" "interface.rkt" "preprocess.rkt")

(provide
 (struct-out table-row) (struct-out report-info)
 make-report-info read-datafile write-datafile
 merge-datafiles diff-datafiles)


(struct table-row
  (name identifier status pre preprocess precision conversions vars
        input output spec target-prog start result target
        start-est result-est time bits link cost-accuracy) #:prefab)

(struct report-info
  (date commit branch hostname seed flags points iterations note tests) #:prefab #:mutable)

(define (make-report-info tests #:note [note ""] #:seed [seed #f])
  (report-info (current-date)
               *herbie-commit*
               *herbie-branch*
               *hostname*
               (or seed (get-seed))
               (*flags*)
               (*num-points*)
               (*num-iterations*)
               note
               tests))

(define (write-datafile file info)
  (define (simplify-test test)
    (match test
      [(table-row name identifier status pre preprocess prec conversions vars
                  input output spec target-prog
                  start-bits end-bits target-bits start-est end-est
                  time bits link cost-accuracy)
       (define cost-accuracy*
         (if (null? cost-accuracy)
             '()
             (list (first cost-accuracy)
                   (second cost-accuracy)
                   (for/list ([entry (third cost-accuracy)])
                    (list (first entry) (second entry) (~s (third entry)))))))
       (make-hash
        `((name . ,name)
          (identifier . ,(~s identifier))
          (pre . ,(~s pre))
          (preprocess . ,(~s (map preprocess->sexp preprocess)))
          (prec . ,(~s prec))
          (conversions . ,(~a conversions))
          (status . ,status)
          (start . ,start-bits)
          (end . ,end-bits)
          (target . ,target-bits)
          (start-est . ,start-est)
          (end-est . ,end-est)
          (vars . ,(if vars (map symbol->string vars) #f))
          (input . ,(~s input))
          (output . ,(~s output))
          (spec . ,(~s spec))
          (target-prog . ,(~s target-prog))
          (time . ,time)
          (bits . ,bits)
          (link . ,(~a link))
          (cost-accuracy . ,(~a cost-accuracy*))))]))
  
  (define data
    (match info
      [(report-info date commit branch hostname seed flags points iterations note tests)
       (make-hash
        `((date . ,(date->seconds date))
          (commit . ,commit)
          (branch . ,branch)
          (hostname . ,hostname)
          (seed . ,(~a seed))
          (flags . ,(flags->list flags))
          (points . ,points)
          (iterations . ,iterations)
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
    (report-info (seconds->date (get 'date)) (get 'commit) (get 'branch) (hash-ref json 'hostname "")
                 (parse-string (get 'seed))
                 (list->flags (get 'flags)) (get 'points)
                 (get 'iterations)
                 (hash-ref json 'note #f)
                 (for/list ([test (get 'tests)] #:when (hash-has-key? test 'vars))
                   (let ([get (λ (field) (hash-ref test field))])
                     (define vars
                       (match (hash-ref test 'vars)
                         [(list names ...) (map string->symbol names)]
                         [string-lst (parse-string string-lst)]))
                     (table-row (get 'name)
                                (parse-string (hash-ref test 'identifier "#f"))
                                (get 'status)
                                (parse-string (hash-ref test 'pre "TRUE"))
                                (map sexp->preprocess (parse-string (hash-ref test 'herbie-preprocess "()")))
                                (string->symbol (hash-ref test 'prec "binary64"))
                                (parse-string (hash-ref test 'conversions "()"))
                                vars (parse-string (get 'input)) (parse-string (get 'output))
                                (parse-string (hash-ref test 'spec "#f"))
                                (parse-string (hash-ref test 'target-prog "#f"))
                                (get 'start) (get 'end) (get 'target)
                                (hash-ref test 'start-est 0) (hash-ref test 'end-est 0)
                                (get 'time) (get 'bits) (get 'link)
                                (parse-string (hash-ref test 'cost-accuracy "()"))))))))

(define (unique? a)
  (or (null? a) (andmap (curry equal? (car a)) (cdr a))))

(define (merge-datafiles . dfs)
  (when (null? dfs)
    (error' merge-datafiles "Cannot merge no datafiles"))
  (for ([f (in-list (list report-info-commit report-info-hostname report-info-seed
                          report-info-flags report-info-points report-info-iterations))])
    (unless (unique? (map f dfs))
      (error 'merge-datafiles "Cannot merge datafiles at different ~a" f)))

  (report-info
   (last (sort (map report-info-date dfs) < #:key date->seconds))
   (report-info-commit (first dfs))
   (first (filter values (map report-info-branch dfs)))
   (report-info-hostname (first dfs))
   (report-info-seed (first dfs))
   (report-info-flags (first dfs))
   (report-info-points (first dfs))
   (report-info-iterations (first dfs))
   (~a (cons 'merged (map report-info-note dfs)))
   (apply append (map report-info-tests dfs))))

(define (diff-datafiles old new)
  (define old-tests
    (for/hash ([ot (in-list (report-info-tests old))])
      (values (table-row-name ot) ot)))
  (define tests*
    (for/list ([nt (in-list (report-info-tests new))])
      (if (hash-has-key? old-tests (table-row-name nt))
          (let ([ot (hash-ref old-tests (table-row-name nt))])
            (define end-score (table-row-result nt))
            (define target-score (table-row-result ot))
            (define start-score (table-row-start nt))

            (struct-copy table-row nt
                         [status
                          (if (and end-score target-score start-score)
                              (cond
                               [(< end-score (- target-score 1)) "gt-target"]
                               [(< end-score (+ target-score 1)) "eq-target"]
                               [(> end-score (+ start-score 1)) "lt-start"]
                               [(> end-score (- start-score 1)) "eq-start"]
                               [(> end-score (+ target-score 1)) "lt-target"])
                              (table-row-status nt))]
                         [target-prog (table-row-output ot)]
                         [target (table-row-result ot)]))
          nt)))
  (struct-copy report-info new [tests tests*]))
