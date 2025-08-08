#lang racket

(require "syntax/platform-language.rkt")
(provide (all-from-out "syntax/platform-language.rkt"))
(module reader syntax/module-reader
  herbie/syntax/platform-language)
