#lang racket

(require herbie/load-plugin
         herbie/points
         herbie/sandbox
         herbie/syntax/read
         (only-in fpbench core->c))

(load-herbie-builtins)

(module+ main
  (define seed 1)
  (*timeout* (* 1000 60 5))
  (command-line
    #:program "cost"
    #:once-each
    [("--seed") _seed "Seed to use within Herbie"
     (set! seed (string->number _seed))]
    #:args (subcommand . args)
    (match subcommand
      ["compile"
       (match-define (list arg) args)
       (match-define (list #\" cs ... #\") (string->list (string-replace arg "\\\"" "\"")))
       (define core (read (open-input-string (apply string cs))))
       (printf "~a" (core->c core "foo"))]
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
