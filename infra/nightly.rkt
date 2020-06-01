#lang racket
(require json)
(require "../src/timeline.rkt" "../src/profile.rkt")

(define (merge-timelines out dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (define tl (call-with-input-file (build-path dir "timeline.json") read-json))
         (timeline-relink dir tl)))))
  (write-json out (apply timeline-merge tls)))

(define (merge-profiles out dirs)
  (define tls
    (filter
     (conjoin (negate eof-object?) identity)
     (for/list ([dir (in-list dirs)])
       (with-handlers ([exn? (const #f)])
         (call-with-input-file (build-path dir "profile.json") read-json)))))
  (write-json out (profile->json (apply profile-merge (map json->profile tls)))))

(module+ main
  (command-line
   #:args dirs
   (call-with-output-file "timeline.json" #:exists 'replace (curryr merge-timelines dirs))
   (call-with-output-file "profiles.json" #:exists 'replace (curryr merge-profiles dirs))
   ))
