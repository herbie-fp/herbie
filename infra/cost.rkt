#lang racket

(require (only-in fpbench core->c)
         herbie/load-plugin
         herbie/points
         herbie/sandbox
         herbie/syntax/read)

(load-herbie-builtins)

;; An insane contraption.
;; Reads commands from stdin and writes results to stdout.
;; All output must be on a single line and terminated by a newline.
(define (run-server seed)
  (let loop ()
    (define cmd (read))
    (case cmd
      [(compile)
       ; compile <lang:symbol> <core:expr>
       (define lang (read))
       (define core (read))
       (define output
         (case lang
           [(c) (core->c core "foo")]
           [else (error 'run-server "compile: unsupported language ~a" lang)]))
       (printf "~a\n" (string-replace output "\n" "\\n"))
       (loop)]
      [(sample)
       ; sample <num_points:int> <core:expr>
       (define n (read))
       (define test (parse-test (read-syntax)))
       (*reeval-pts* (string->number n))
       (define result (run-herbie 'sample test #:seed seed))
       (define pctx (job-result-backend result))
       (printf "~a\n"
               (string-join
                  (for ([(pt _) (in-pcontext pctx)])
                    (string-join (map ~s pt) ","))
                  "|"))
       (loop)]
      [(exit)
       (void)]
      [else
       (error 'run-server "unknown command ~a" cmd)])))

(module+ main
  (define seed 1)
  (command-line
    #:program "cost"
    #:once-each
    [("--seed") _seed "Seed to use within Herbie"
     (set! seed (string->number _seed))]
    #:args ()
    (run-server seed)))

; (module+ main
;   (define seed 1)
;   (*timeout* (* 1000 60 5))
;   (command-line
;     #:program "cost"
;     #:once-each
;     [("--seed") _seed "Seed to use within Herbie"
;      (set! seed (string->number _seed))]
;     #:args (subcommand . args)
;     (match subcommand
;       ["compile"
;        (match-define (list lang arg) args)
;        (match-define (list #\" cs ... #\") (string->list (string-replace arg "\\\"" "\"")))
;        (define core (read (open-input-string (apply string cs))))
;        (case lang
;          [("c") (printf "~a" (core->c core "foo"))]
;          [("mkl") (printf "~a" (core->mkl core "foo"))])]
;       ["sample"
;        (match-define (list n e) args)
;        (match-define (list #\" cs ... #\") (string->list (string-replace e "\\\"" "\"")))
;        (define stx (read-syntax #f (open-input-string (apply string cs))))
;        (define test (parse-test stx))
       
;        (*reeval-pts* (string->number n))
;        (define result (run-herbie 'sample test #:seed seed))
;        (define pctx (job-result-backend result))
;        (for ([(pt _) (in-pcontext pctx)])
;          (printf "~a\n" (string-join (map ~s pt) " ")))])))
