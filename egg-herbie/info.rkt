#lang info

(define collection "egg-herbie")
(define version "2.0")

(define pkg-desc "Racket bindings for simplifying math expressions using egg")
(define pkg-authors
  `("Oliver Flatt"))

(define build-deps
  '("rackunit-lib"))

(define deps '(("base" #:version "8.0")))

(define license 'MIT)
