#lang racket

(require json)
(require "../reports/common.rkt"
         "../reports/pages.rkt"
         "../reports/timeline.rkt"
         "../syntax/read.rkt"
         "../syntax/sugar.rkt"
         "../syntax/types.rkt"
         "../utils/common.rkt"
         "../utils/profile.rkt"
         "../utils/timeline.rkt"
         "../core/points.rkt"
         "datafile.rkt"
         "sandbox.rkt"
         "server.rkt")

(provide make-report
         rerun-report)

(define (extract-test row)
  (define vars (table-row-vars row))
  (define repr (get-representation (table-row-precision row)))
  (define var-reprs (map (curryr cons repr) vars))
  (define ctx (context vars repr (map (const repr) vars)))
  (test (table-row-name row)
        (table-row-identifier row)
        (table-row-vars row)
        (fpcore->prog (table-row-input row) ctx)
        (fpcore->prog (table-row-output row) ctx)
        (table-row-target-prog row)
        (fpcore->prog (table-row-spec row) ctx)
        (fpcore->prog (table-row-pre row) ctx)
        (representation-name repr)
        (for/list ([(k v) (in-dict var-reprs)])
          (cons k (representation-name v)))
        (table-row-conversions row)))

(define (make-report bench-dirs #:dir dir #:threads threads)
  (define tests (reverse (sort (append-map load-tests bench-dirs) test<?)))
  (run-tests tests #:dir dir #:threads threads))

(define (rerun-report json-file #:dir dir #:threads threads)
  (define data (call-with-input-file json-file read-datafile))
  (define tests (map extract-test (report-info-tests data)))
  (*flags* (report-info-flags data))
  (set-seed! (report-info-seed data))
  (*num-points* (report-info-points data))
  (*num-iterations* (report-info-iterations data))
  (run-tests tests #:dir dir #:threads threads))

(define (read-json-files info dir name)
  (filter identity
          (for/list ([res (report-info-tests info)])
            (define out
              (with-handlers ([exn? (const #f)])
                (call-with-input-file (build-path dir (table-row-link res) name) read-json)))
            (and out (not (eof-object? out)) out))))

(define (merge-profile-jsons ps)
  (profile->json (apply profile-merge (map json->profile ps))))

(define (generate-bench-report result bench-name test-number dir total-tests)
  (define report-path (bench-folder-path bench-name test-number))
  (define report-directory (build-path dir report-path))
  (unless (directory-exists? report-directory)
    (make-directory report-directory))

  (for ([page (all-pages result)])
    (call-with-output-file (build-path report-directory page)
                           #:exists 'replace
                           (λ (out)
                             (with-handlers ([exn:fail? (page-error-handler result page out)])
                               (make-page-timeout page out result #t #f #:timeout 10000)))))

  (get-table-data-from-hash result report-path))

(define (run-tests tests #:dir dir #:threads threads)
  (define seed (get-seed))
  (unless (directory-exists? dir)
    (make-directory dir))

  (server-start threads)
  (define job-ids
    (for/list ([test (in-list tests)])
      (job-start 'improve test #:seed seed #:pcontext #f #:profile? #t #:timeline? #t)))

  (define total-tests (length tests))
  (define results
    (for/list ([job-id (in-list job-ids)]
               [test (in-list tests)]
               [test-number (in-naturals)])
      (define result (job-wait job-id))
      (print-test-result (+ test-number 1) total-tests test result)
      (generate-bench-report result (test-name test) test-number dir total-tests)))

  (define info (make-report-info results #:seed seed))
  (write-datafile (build-path dir "results.json") info)
  (copy-file (web-resource "report-page.js") (build-path dir "report-page.js") #t)
  (copy-file (web-resource "report.js") (build-path dir "report.js") #t)
  (copy-file (web-resource "report.css") (build-path dir "report.css") #t)
  (copy-file (web-resource "logo-car.png") (build-path dir "logo-car.png") #t)
  (copy-file (web-resource "report.html") (build-path dir "index.html") #t)
  (define timeline (apply timeline-merge (read-json-files info dir "timeline.json")))
  (call-with-output-file (build-path dir "timeline.json")
                         (curry write-json timeline)
                         #:exists 'replace)
  (define profile (merge-profile-jsons (read-json-files info dir "profile.json")))
  (call-with-output-file (build-path dir "profile.json") (curry write-json profile) #:exists 'replace)

  (call-with-output-file
   (build-path dir "timeline.html")
   #:exists 'replace
   (λ (out) (write-html (make-timeline "Herbie run" timeline #:info info #:path ".") out)))

  ; Delete old files
  (define expected-dirs
    (map string->path (filter identity (map table-row-link (report-info-tests info)))))
  (define actual-dirs
    (filter (λ (name) (directory-exists? (build-path dir name))) (directory-list dir)))
  (define extra-dirs (filter (λ (name) (not (member name expected-dirs))) actual-dirs))
  (for ([subdir extra-dirs])
    (with-handlers ([exn:fail:filesystem? (const true)])
      (delete-directory/files (build-path dir subdir)))))

(define (test<? t1 t2)
  (cond
    [(and (test-output t1) (test-output t2)) (string<? (test-name t1) (test-name t2))]
    [(and (not (test-output t1)) (not (test-output t2))) (string<? (test-name t1) (test-name t2))]
    ; Put things with an output first
    [else (test-output t1)]))

;; Generate a path for a given benchmark name
(define (bench-folder-path bench-name index)
  (define replaced (string-replace bench-name #px"\\W+" ""))
  (format "~a-~a" index (substring replaced 0 (min (string-length replaced) 50))))

(define (hash-ref-path hash . path)
  (match path
    ['() hash]
    [(cons (? symbol? key) rest) (apply hash-ref-path (hash-ref hash key) rest)]
    [(cons (? integer? key) rest) (apply hash-ref-path (list-ref hash key) rest)]))

(define (print-test-result i n test result-hash)
  (eprintf "~a/~a\t" (~a i #:width 3 #:align 'right) n)
  (define name (test-name test))
  (match (hash-ref-path result-hash 'status)
    ["failure"
     (match-define (list 'exn type msg url locs tb) (hash-ref-path result-hash 'backend))
     (if type
         (eprintf "[ ERROR ]\t\t~a\n" name)
         (eprintf "[ CRASH ]\t\t~a\n" name))]
    ["timeout" (eprintf "[TIMEOUT]\t\t~a\n" name)]
    [_
     (define bits (representation-total-bits (test-output-repr test)))
     (define time (hash-ref-path result-hash 'time))
     (define start-score (errors-score (hash-ref-path result-hash 'backend 'start 'errors)))
     (define end-score (errors-score (hash-ref-path result-hash 'backend 'end 0 'errors)))
     (eprintf "[~as]  ~a% → ~a%\t~a\n"
              (~r (/ time 1000) #:min-width 6 #:precision '(= 1))
              (~r (* 100 (- 1 (/ start-score bits))) #:min-width 3 #:precision 0)
              (~r (* 100 (- 1 (/ end-score bits))) #:min-width 3 #:precision 0)
              name)]))
