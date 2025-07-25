#lang racket

(require "syntax/platforms-language.rkt")
(provide (all-from-out "syntax/platforms-language.rkt"))
(module reader syntax/module-reader
  herbie/syntax/platforms-language)
