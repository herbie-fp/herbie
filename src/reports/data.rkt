#lang racket

(provide (struct-out table-row)
         (struct-out report-info))

(struct table-row
        (name identifier
              status
              pre
              precision
              conversions
              vars
              warnings
              input
              output
              spec
              target-prog
              start
              result
              target
              time
              link
              cost-accuracy)
  #:prefab)

(struct report-info (date commit branch seed flags points iterations tests merged-cost-accuracy)
  #:prefab)
