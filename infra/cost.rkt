#lang racket

(require herbie/load-plugin
         herbie/points
         herbie/sandbox
         herbie/syntax/read)

(load-herbie-builtins)

(module+ main
  (define seed 1)
  (command-line
    #:program "cost"
    #:once-each
    [("--seed") _seed "Seed to use within Herbie"
     (set! seed (string->number _seed))]
    #:args (subcommand . args)
    (match subcommand
      ["sample"
       (match-define (list n e) args)
       (match-define (list #\" cs ... #\") (string->list (string-replace e "\\\"" "\"")))
       (define stx (read-syntax #f (open-input-string (apply string cs))))
       (define test (parse-test stx))
       
       (*reeval-pts* (string->number n))
       (define result (run-herbie 'sample test #:seed seed))
       (define pctx (job-result-backend result))
       (for ([(pt _) (in-pcontext pctx)])
         (printf "~a\n" (string-join (map ~s pt) " ")))])))
