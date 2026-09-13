#lang racket

(require "../src/config.rkt"
         "../src/syntax/read.rkt"
         "../src/syntax/load-platform.rkt"
         "../src/api/sandbox.rkt")

(module+ main
  (define seed #f)
  (command-line
   #:program "rival-dump"
   #:once-each
   [("--seed") n "Random seed" (set! seed (string->number n))]
   [("--timeout") s "Timeout per test, in seconds" (*timeout* (* 1000 (string->number s)))]
   #:args bench-dirs
   (activate-platform! (*platform-name*))
   (enable-flag! 'dump 'rival)
   (define tests (append-map load-tests bench-dirs))
   (for ([test (in-list tests)]
         [i (in-naturals 1)])
     (define status
       (with-handlers ([exn:fail? exn-message])
         (run-herbie 'sample test #:seed seed)
         'ok))
     (printf "~a/~a\t~s\t~a\n" i (length tests) status (test-name test)))))
