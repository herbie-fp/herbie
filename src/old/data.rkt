#lang racket

(require "../points.rkt")
(require "../alternative.rkt")
(require "../formats/test.rkt")
(require "../formats/datafile.rkt")
(require "../config.rkt")
(require "../plot.rkt")
(require "../common.rkt")
(require "../mainloop.rkt")
(require json)
(require math/flonum)


(define (get-report-errs start end target samplers file)
  (define newcontext
    (parameterize ([*num-points* 8000])
      (prepare-points (alt-program start) samplers)))
  (write-errors file newcontext start end target))

(define (run-test-write-errors tst file)
  (define start (make-alt (test-program tst)))
  (define target (make-alt (test-target tst)))
  (define end (run-improve (test-program tst) (*num-iterations*) #:samplers (test-samplers tst)))
  (get-report-errs start end target (test-samplers tst) file))

(define (write-errors file pcontext start end target)
  (parameterize ([*pcontext* pcontext])
    (define (get-errors altn)
      (for/list ([(p _) (in-pcontext pcontext)]
                 [err (alt-errors altn)])
        (list p err)))
    (let ([data
           (make-hash
            `((points . ,(for/list ([(p _) (in-pcontext pcontext)]) p))
              (startErrors . ,(alt-errors start))
              (endErrors . ,(alt-errors end))
              (targetErrors . ,(alt-errors target))))])
      (call-with-output-file file (curry write-json data) #:exists 'replace))))

(define (json->graph axis-index json-file graph-file)
  (let* ([data (call-with-input-file json-file read-json)]
         [pnts (hash-ref data 'points)])
    (define (sow-data sow data-points theme)
      (when (not (= (length data-points) (length pnts)))
        (error "lists don't match"))
      (sow (error-points data-points pnts #:axis axis-index #:color theme))
      (sow (error-avg data-points pnts #:axis axis-index #:color theme)))
    (call-with-output-file graph-file #:exists 'replace
      (λ (port)
        (herbie-plot #:port port #:kind 'png
                     (reap [sow]
                           (sow-data sow (hash-ref data 'startErrors) *red-theme*)
                           (sow-data sow (hash-ref data 'endErrors) *blue-theme*)
                           (sow-data sow (hash-ref data 'targetErrors) *green-theme*)))))))

(define (json->ordinal-json in-file out-file)
  (let* ([data (call-with-input-file in-file read-json)]
         [float-points (hash-ref data 'points)]
         [ordinal-points (map (curry map flonum->ordinal) float-points)]
         [out-data (hash-set data 'points ordinal-points)])
    (call-with-output-file out-file (curry write-json out-data) #:exists 'replace)))

