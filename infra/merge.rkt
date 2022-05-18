#lang racket
(require json)
(require "../src/common.rkt" "../src/timeline.rkt" "../src/profile.rkt"
         "../src/datafile.rkt" "../src/web/timeline.rkt" "../src/web/make-report.rkt"
         "../src/load-plugin.rkt")

(define (merge-timelines outdir . dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (define tl (call-with-input-file (build-path outdir dir "timeline.json") read-json))
         (timeline-relink dir tl)))))
  (define info (read-datafile (build-path outdir (first dirs) "results.json")))
  (define joint-tl (apply timeline-merge tls))
  (call-with-output-file (build-path outdir "timeline.json")
    #:exists 'replace
    (curry write-json joint-tl))
  (call-with-output-file (build-path outdir "timeline.html")
    #:exists 'replace
    (λ (out) (make-timeline "Herbie run" joint-tl out #:info info))))

(define (merge-profiles outdir . dirs)
  (define pfs
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (call-with-input-file (build-path outdir dir "profile.json") read-json)))))
  (define joint-pf (apply profile-merge (map json->profile pfs)))
  (call-with-output-file (build-path outdir "profile.json")
    #:exists 'replace
    (curry write-json (profile->json joint-pf))))

(define (merge-reports outdir . dirs)
  (load-herbie-builtins)
  (define rss
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (read-datafile (build-path outdir dir "results.json"))))))
  (define joint-rs (merge-datafiles rss #:dirs dirs))
  (write-datafile (build-path outdir "results.json") joint-rs)
  (call-with-output-file (build-path outdir "results.html")
    #:exists 'replace
    (curryr make-report-page joint-rs #f)))

(module+ main
  (command-line
   #:args (outdir . dirs)
   (apply merge-reports outdir dirs)
   (apply merge-timelines outdir dirs)
   (apply merge-profiles outdir dirs)
   (copy-file (web-resource "arrow-chart.js") (build-path outdir "arrow-chart.js") #t)
   (copy-file (web-resource "report.js") (build-path outdir "report.js") #t)
   (copy-file (web-resource "report.css") (build-path outdir "report.css") #t)))
