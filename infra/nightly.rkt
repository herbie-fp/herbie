#lang racket
(require json)
(require "../src/common.rkt" "../src/timeline.rkt" "../src/profile.rkt"
         "../src/datafile.rkt" "../src/web/timeline.rkt" "../src/web/make-report.rkt")

(define (merge-timelines outdir . dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (define tl (call-with-input-file (build-path dir "timeline.json") read-json))
         (define link
           (find-relative-path (simple-form-path outdir) (simple-form-path dir)))
         (timeline-relink link tl)))))
  (define info (read-datafile (build-path (first dirs) "results.json")))
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
         (call-with-input-file (build-path dir "profile.json") read-json)))))
  (define joint-pf (apply profile-merge (map json->profile pfs)))
  (call-with-output-file (build-path outdir "profile.json")
    #:exists 'replace
    (curry write-json (profile->json joint-pf))))

(define (merge-reports outdir . dirs)
  (define rss
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (call-with-input-file (build-path dir "results.json") read-datafile)))))
  (define joint-rs (apply merge-datafiles rss))
  (write-datafile (build-path outdir "results.json") joint-rs)
  (call-with-output-file (build-path outdir "results.html")
    #:exists 'replace
    (curryr make-report joint-rs #f)))

(module+ main
  (command-line
   #:args (outdir . dirs)
   (apply merge-reports outdir dirs)
   (apply merge-timelines outdir dirs)
   (apply merge-profiles outdir dirs)
   (copy-file (web-resource "report.js") (build-path outdir "report.js") #t)
   (copy-file (web-resource "report.css") (build-path outdir "report.css") #t)))
