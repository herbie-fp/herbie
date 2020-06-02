#lang racket
(require json)
(require "../src/common.rkt" "../src/timeline.rkt" "../src/profile.rkt"
         "../src/datafile.rkt" "../src/web/timeline.rkt")

(define (merge-timelines outdir . dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (define tl (call-with-input-file (build-path dir "timeline.json") read-json))
         (timeline-relink dir tl)))))
  (define info (read-datafile (build-path (first dirs) "results.json")))
  (define joint-tl (apply timeline-merge tls))
  (call-with-output-file (build-path outdir "timeline.json")
    #:exists 'replace
    (curry write-json joint-tl))
  (call-with-output-file (build-path outdir "timeline.html")
    #:exists 'replace
    (curryr make-summary-html info joint-tl))
  (copy-file (web-resource "report.js") (build-path outdir "report.js") #t)
  (copy-file (web-resource "report.css") (build-path outdir "report.css") #t))

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

(module+ main
  (command-line
   #:args (outdir . dirs)
   (apply merge-timelines outdir dirs)
   (apply merge-profiles outdir dirs)))
