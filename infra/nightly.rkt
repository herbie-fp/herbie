#lang racket
(require json)
(require "../src/timeline.rkt" "../src/profile.rkt")

(define (merge-timelines . dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (define tl (call-with-input-file (build-path dir "timeline.json") read-json))
         (timeline-relink dir tl)))))
  (define info (read-datafile (build-path (first dir) "results.json")))
  (define joint-tl (apply timeline-merge tls))
  (call-with-output-file "timeline.json" #:exists 'replace (curry write-json joint-tl))
  (call-with-output-file "timeline.html" #:exists 'replace (curryr make-summary-html info joint-tl)))

(define (merge-profiles dirs)
  (define pfs
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (call-with-input-file (build-path dir "profile.json") read-json)))))
  (define joint-pf (apply profile-merge (map json->profile pfs)))
  (call-with-output-file "profiles.json" #:exists 'replace (curry write-json (profile->json joint-pf))))

(module+ main
  (command-line
   #:args dirs
   (apply merge-timelines dirs)
   (apply merge-profiles dirs)))
