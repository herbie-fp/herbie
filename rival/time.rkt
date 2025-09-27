#lang racket

(require racket/math
         math/base
         math/flonum
         math/bigfloat
         racket/random
         profile)
(require json)

(require "main.rkt"
         "test.rkt"
         "profile.rkt"
         "eval/machine.rkt" ; for accessing iteration number of machine
         "infra/run-sollya.rkt")

(define sample-vals (make-parameter 5000))
(define *sampling-timeout* (make-parameter 20.0)) ; this parameter is used for plots generation

; These parameters are used for latex data
(define *num-tuned-benchmarks* (make-parameter 0))
(define *rival-timeout* (make-parameter 0))
(define *baseline-timeout* (make-parameter 0))
(define *sollya-timeout* (make-parameter 0))

(define (time-operation ival-fn bf-fn itypes otype)
  (define n
    (if (set-member? slow-tests ival-fn)
        (/ (sample-vals) 100) ; Gamma functions are super duper slow
        (sample-vals)))
  (define ivtime 0.0)
  (define bftime 0.0)
  (for ([i (in-range n)])
    (define args
      (for/list ([itype (in-list itypes)])
        (sample-interval itype)))
    (define start (current-inexact-milliseconds))
    (apply ival-fn args)
    (define dt1 (- (current-inexact-milliseconds) start))
    (set! ivtime (+ ivtime dt1))
    (define start2 (current-inexact-milliseconds))
    (apply bf-fn (map ival-lo args))
    (apply bf-fn (map ival-hi args))
    (define dt2 (- (current-inexact-milliseconds) start2))
    (set! bftime (+ bftime dt2)))
  (values (* 1000 (/ ivtime n)) (* 1000 (/ bftime n))))

(define (read-from-string s)
  (read (open-input-string s)))

(define (time-expr rec timeline sollya-reeval)
  (define exprs (map read-from-string (hash-ref rec 'exprs)))
  (define vars (map read-from-string (hash-ref rec 'vars)))
  (unless (andmap symbol? vars)
    (raise 'time "Invalid variable list ~a" vars))
  (match-define `(bool flonum ...) (map read-from-string (hash-ref rec 'discs)))
  (define discs (cons boolean-discretization (map (const flonum-discretization) (cdr exprs))))

  ; Rival machine
  (define start-compile (current-inexact-milliseconds))
  (define rival-machine (rival-compile exprs vars discs))
  (define compile-time (- (current-inexact-milliseconds) start-compile))

  ; Baseline and Sollya machines
  (define baseline-machine (baseline-compile exprs vars discs))

  (define sollya-machine
    (match (or (equal? (cdr exprs) `((* (fmod (exp x) (sqrt (cos x))) (exp (neg x))))) ; id 65
               (equal? (cdr exprs) `((* (exp (neg w)) (pow l (exp w)))))) ; id 68
      [#t
       (printf "Sollya didn't compile due to the bugs in evaluation of:\n\t~a\n" exprs)
       #f]
      [#f
       (with-handlers ([exn:fail? (λ (e)
                                    (printf "Sollya didn't compile")
                                    (printf "~a\n" e)
                                    #f)])
         (sollya-compile exprs vars 53))])) ; prec=53 is an imitation of flonum

  (define tuned-bench #f)
  (define times
    (for/list ([pt* (in-list (hash-ref rec 'points))])
      (match-define (list pt sollya-exs sollya-status sollya-apply-time) pt*)
      ; --------------------------- Baseline execution ----------------------------------------------
      (define baseline-start-apply (current-inexact-milliseconds))
      (match-define (list baseline-status baseline-exs)
        (parameterize ([*rival-max-precision* 32256])
          (with-handlers ([exn:rival:invalid? (λ (e) (list 'invalid #f))]
                          [exn:rival:unsamplable? (λ (e) (list 'unsamplable #f))])
            (define exs (vector-ref (baseline-apply baseline-machine (list->vector (map bf pt))) 1))
            (list 'valid exs))))
      (define baseline-apply-time (- (current-inexact-milliseconds) baseline-start-apply))
      (define baseline-precision (baseline-profile baseline-machine 'precision))
      (define baseline-executions (baseline-profile baseline-machine 'executions))
      (define baseline-iteration (baseline-profile baseline-machine 'iteration))

      ; Store histograms data
      (when (> baseline-iteration 0)
        (for ([execution (in-vector baseline-executions)])
          (define name (symbol->string (execution-name execution)))
          (define precision (execution-precision execution))
          (when (equal? baseline-status 'valid)
            (timeline-push! timeline
                            'mixsample-baseline-valid
                            (list (execution-time execution) name precision)))
          (timeline-push! timeline
                          'mixsample-baseline-all
                          (list (execution-time execution) name precision))))

      ; Record the percentage of instructions has been executed
      (when (equal? baseline-status 'valid)
        (for ([execution (in-vector baseline-executions)])
          (timeline-push! timeline
                          'instr-executed-cnt
                          (list 'baseline (execution-iteration execution) 1)))
        (define ivec-len (vector-length (baseline-machine-instructions baseline-machine)))
        (for ([n (in-range (add1 baseline-iteration))])
          (timeline-push! timeline 'instr-executed-cnt (list 'baseline-no-repeats n ivec-len))))

      ; --------------------------- Rival execution -------------------------------------------------
      (define rival-start-apply (current-inexact-milliseconds))
      (match-define (list rival-status rival-exs)
        (parameterize ([*rival-max-precision* 32256])
          (with-handlers ([exn:rival:invalid? (λ (e) (list 'invalid #f))]
                          [exn:rival:unsamplable? (λ (e) (list 'unsamplable #f))])
            (define exs (vector-ref (rival-apply rival-machine (list->vector (map bf pt))) 1))
            (list 'valid exs))))
      (define rival-apply-time (- (current-inexact-milliseconds) rival-start-apply))
      (define rival-iter (rival-machine-iteration rival-machine))
      (define rival-executions (rival-profile rival-machine 'executions))

      (when (and (> baseline-iteration 0) (not tuned-bench))
        (set! tuned-bench #t)
        (*num-tuned-benchmarks* (add1 (*num-tuned-benchmarks*))))

      ; Store histograms data
      (when (> baseline-iteration 0)
        (for ([execution (in-vector rival-executions)])
          (define name (symbol->string (execution-name execution)))
          (define precision (execution-precision execution))
          (when (equal? rival-status 'valid)
            (timeline-push! timeline
                            'mixsample-rival-valid
                            (list (execution-time execution) name precision)))
          (timeline-push! timeline
                          'mixsample-rival-all
                          (list (execution-time execution) name precision))))

      ; Store density plot data
      (when (and (equal? rival-status 'valid) (> baseline-iteration 0))
        (define h (make-hash))
        (define max-prec 0)
        (for ([exec (in-vector rival-executions)])
          (match-define (execution name number precision time _ _) exec)
          (unless (equal? name 'adjust)
            (define precision* (hash-ref h (list name number) (λ () 0)))
            (hash-set! h (list name number) (max precision precision*))
            (set! max-prec (max precision precision* max-prec))))
        (for ([(_ precision) (in-hash h)])
          (timeline-push! timeline 'density (~a (exact->inexact (/ precision max-prec)) #:width 5))))

      ; Record the percentage of instructions has been executed
      (when (equal? rival-status 'valid)
        (for ([execution (in-vector rival-executions)])
          (timeline-push! timeline
                          'instr-executed-cnt
                          (list 'rival (execution-iteration execution) 1)))
        (define ivec-len (vector-length (rival-machine-instructions rival-machine)))
        (for ([n (in-range (add1 rival-iter))])
          (timeline-push! timeline 'instr-executed-cnt (list 'rival-no-repeats n ivec-len))))

      ; --------------------------- Sollya execution ------------------------------------------------
      ; Points for expressions where Sollya has not compiled do not go to the plot/speed graphs!
      ; Also, if Rival's status is invalid - these points do not go to the graphs!
      ; We treat Rival's results as the right ones since for some benchs Sollya has produced wrong results!
      (when (and (and rival-machine baseline-machine sollya-machine)
                 (or (equal? rival-status 'valid) (equal? rival-status 'unsamplable)))
        (match sollya-reeval
          [#t
           (set! sollya-apply-time 0.0)
           (match sollya-machine
             [#f (list #f #f)] ; if sollya machine is not working for this benchmark
             [else
              (with-handlers ([exn:fail? (λ (e)
                                           (printf "Sollya failed")
                                           (printf "~a\n" e)
                                           (sollya-kill sollya-machine)
                                           (set! sollya-machine #f)
                                           (list #f #f))])
                (match-define (list internal-time external-time exs status)
                  (sollya-apply sollya-machine pt #:timeout (*sampling-timeout*)))
                (set! sollya-apply-time external-time)
                (set! sollya-status status)
                (set! sollya-exs exs))])]
          [#f
           (set! sollya-exs
                 (match sollya-exs
                   ["#f" #f]
                   [#f #f]
                   [_ (fl (string->number sollya-exs))]))

           (set! sollya-status
                 (match sollya-status
                   ["#f" 'invalid]
                   [#f 'invalid]
                   [_ (string->symbol sollya-status)]))

           (set! sollya-apply-time
                 (match sollya-apply-time
                   ["#f" 0.0]
                   [#f 0.0]
                   [_ sollya-apply-time]))])

        ; -------------------------------- Combining results ----------------------------------------
        ; When all the machines have compiled and produced results - write the results to outcomes
        (when (> (*sampling-timeout*) sollya-apply-time)
          (point-bucketing timeline
                           rival-status
                           rival-apply-time
                           rival-exs
                           baseline-status
                           baseline-apply-time
                           baseline-exs
                           sollya-status
                           sollya-apply-time
                           sollya-exs
                           baseline-precision
                           rival-iter))

        (when (<= (*sampling-timeout*) sollya-apply-time)
          (*sollya-timeout* (add1 (*sollya-timeout*))))
        (when (<= (*sampling-timeout*) rival-apply-time)
          (*rival-timeout* (add1 (*rival-timeout*))))
        (when (<= (*sampling-timeout*) baseline-apply-time)
          (*baseline-timeout* (add1 (*baseline-timeout*)))))

      ; Count differences where baseline is better than rival
      (define rival-baseline-difference
        (if (and (or (equal? rival-status 'unsamplable) (equal? rival-status 'invalid))
                 (equal? baseline-status 'valid))
            1
            0))
      (cons rival-status (cons rival-apply-time rival-baseline-difference))))

  ; Zombie process
  (when sollya-machine
    (sollya-kill sollya-machine))

  (cons (cons 'compile compile-time) times))

(define (time-exprs data)
  (define times
    (for/hash ([group (in-list (group-by car data))])
      (values (caar group) (map cdr group))))

  (list (/ (car (hash-ref times 'compile)) 1000)
        (length (hash-ref times 'valid '()))
        (/ (apply + (map car (hash-ref times 'valid '()))) 1000)
        (length (hash-ref times 'invalid '()))
        (/ (apply + (map car (hash-ref times 'invalid '()))) 1000)
        (length (hash-ref times 'unsamplable '()))
        (/ (apply + (map car (hash-ref times 'unsamplable '()))) 1000)
        (apply + (map cdr (hash-ref times 'unsamplable '())))))

(define (make-operation-table test-id)
  (for/list ([fn (in-list function-table)]
             #:when (or (not test-id) (equal? test-id (~a (object-name (first fn))))))
    (match-define (list ival-fn bf-fn itypes otype) fn)
    (define-values (iv256 bf256) (time-operation ival-fn bf-fn itypes otype))
    (define-values (iv4k bf4k)
      (if (set-member? slow-tests ival-fn)
          (values 1000000 1)
          (parameterize ([bf-precision 4096])
            (time-operation ival-fn bf-fn itypes otype))))
    (printf "~a ~aµs (~a×)\t~aµs (~a×)\n"
            (~a (object-name ival-fn) #:align 'left #:min-width 20)
            (~r iv256 #:precision '(= 3) #:min-width 8)
            (~r (/ iv256 bf256) #:precision '(= 2) #:min-width 4)
            (~r iv4k #:precision '(= 3) #:min-width 8)
            (~r (/ iv4k bf4k) #:precision '(= 2) #:min-width 4))

    (list (object-name ival-fn) iv256 (/ iv256 bf256) iv4k (/ iv4k bf4k))))

(define (timeline-push! timeline key args*)
  (match key
    ['outcomes
     (match-define (list status iter precision time*) args*)
     (define outcomes-hash (hash-ref timeline key))
     (match-define (list time num-points)
       (hash-ref outcomes-hash (list status iter precision) (λ () (list 0 0))))
     (hash-set! outcomes-hash (list status iter precision) (list (+ time time*) (+ num-points 1)))]
    [(or 'mixsample-rival-valid
         'mixsample-rival-all
         'mixsample-baseline-valid
         'mixsample-baseline-all)
     (define mixsample-hash (hash-ref timeline key))
     (match-define (list time* name precision) args*)
     (define time (hash-ref mixsample-hash (list name precision) (λ () 0)))
     (hash-set! mixsample-hash (list name precision) (+ time time*))]
    ['instr-executed-cnt
     (define instr-cnt-hash (hash-ref timeline key))
     (match-define (list tool iter cnt) args*)
     (define cnt* (hash-ref instr-cnt-hash (list tool iter) (λ () 0)))
     (hash-set! instr-cnt-hash (list tool iter) (+ cnt cnt*))]
    ['density
     (define density-hash (hash-ref timeline key))
     (define precision args*)
     (define cnt (hash-ref density-hash precision (λ () 0)))
     (hash-set! density-hash precision (+ cnt 1))]
    [else (error "Unknown key for timeline!")]))

(define (timeline->jsexpr timeline)
  (hash 'outcomes
        (for/list ([(key value) (in-hash (hash-ref timeline 'outcomes))])
          (list (first value) (second key) (third key) (first key) (second value)))
        'mixsample-rival-valid
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-rival-valid))])
          (list value (car key) (second key)))
        'mixsample-rival-all
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-rival-all))])
          (list value (car key) (second key)))
        'mixsample-baseline-valid
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-baseline-valid))])
          (list value (car key) (second key)))
        'mixsample-baseline-all
        (for/list ([(key value) (in-hash (hash-ref timeline 'mixsample-baseline-all))])
          (list value (car key) (second key)))
        'instr-executed-cnt
        (for/list ([(key value) (in-hash (hash-ref timeline 'instr-executed-cnt))])
          (list (~a (car key)) (second key) value))
        'density
        (for/list ([(key value) (in-hash (hash-ref timeline 'density))])
          (list key value))))

(define (make-expression-table points test-id timeline-port sollya-reeval)
  (newline)
  (define total-c 0.0)
  (define total-v 0.0)
  (define count-v 0.0)
  (define total-i 0.0)
  (define count-i 0.0)
  (define total-u 0.0)
  (define count-u 0.0)
  (define total-mem-bytes 0)

  (define timeline
    (make-hash ; this hash is to be used for the plots
     (list (cons 'outcomes (make-hash))
           (cons 'mixsample-rival-valid (make-hash))
           (cons 'mixsample-baseline-valid (make-hash))
           (cons 'mixsample-rival-all (make-hash))
           (cons 'mixsample-baseline-all (make-hash))
           (cons 'instr-executed-cnt (make-hash))
           (cons 'density (make-hash)))))

  (define table
    (for/list ([rec (in-port read-json points)]
               [i (in-naturals)]
               #:break (and test-id (> i (string->number test-id)))
               #:unless (and test-id (not (equal? (~a i) test-id))))
      (when test-id
        (pretty-print (map read-from-string (hash-ref rec 'exprs))))

      (define mem-before (current-memory-use 'cumulative))
      (match-define (list c-time v-num v-time i-num i-time u-num u-time rival-baseline-diff)
        (time-exprs (time-expr rec timeline sollya-reeval)))
      (define mem-after (current-memory-use 'cumulative))
      (define mem-delta (- mem-after mem-before))
      (define mem-mib (/ (exact->inexact mem-delta) (* 1024 1024)))
      (set! total-c (+ total-c c-time))
      (set! total-v (+ total-v v-time))
      (set! count-v (+ count-v v-num))
      (set! total-i (+ total-i i-time))
      (set! count-i (+ count-i i-num))
      (set! total-u (+ total-u u-time))
      (set! count-u (+ count-u u-num))
      (set! total-mem-bytes (+ total-mem-bytes mem-delta))
      (define t-time (+ c-time v-time i-time u-time))
      (printf "~a: ~as ~as ~as ~as ~as MiB\n"
              (~a i #:align 'left #:min-width 3)
              (~r t-time #:precision '(= 3) #:min-width 8)
              (~r v-time #:precision '(= 3) #:min-width 8)
              (~r i-time #:precision '(= 3) #:min-width 8)
              (~r u-time #:precision '(= 3) #:min-width 8)
              (~r mem-mib #:precision '(= 3) #:min-width 8))
      (list i t-time c-time v-num v-time i-num i-time u-num u-time mem-mib rival-baseline-diff)))
  (printf "\nDATA:\n")
  (printf "\tNUMBER OF TUNED BENCHMARKS = ~a\n" (*num-tuned-benchmarks*))
  (printf "\tRIVAL TIMEOUTS = ~a\n" (*rival-timeout*))
  (printf "\tBASELINE TIMEOUTS = ~a\n" (*baseline-timeout*))
  (printf "\tSOLLYA TIMEOUTS = ~a\n" (*sollya-timeout*))

  (when timeline-port
    (write-json (timeline->jsexpr timeline) timeline-port)
    (close-output-port timeline-port))

  (define total-t (+ total-c total-v total-i total-u))
  (define total-mem (/ (exact->inexact total-mem-bytes) (* 1024 1024)))
  (printf "\nTotal Time: ~as\n" (~r total-t #:precision '(= 3)))
  (printf "Total Memory: ~a MiB\n" (~r total-mem #:precision '(= 3)))
  (define footer
    (list "Total" total-t total-c count-v total-v count-i total-i count-u total-u total-mem))
  (values table footer))

(define (html-write port)
  (define sortable-css "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.css")
  (define sortable-js "https://cdn.jsdelivr.net/gh/tofsjonas/sortable@latest/sortable.min.js")
  (when port
    (fprintf port "<!doctype html><meta charset=utf-8 />")
    (fprintf port "<link href='~a' rel='stylesheet' />" sortable-css)
    (fprintf port "<script src='profile.js' defer></script>")
    (fprintf port "<script src='~a' async defer></script>" sortable-js)
    (fprintf
     port
     "<style>body { max-width: 100ex; margin: 3em auto; } td:nth-child(1n+2) { text-align: right; }</style>")))

(define current-heading #f)

(define (html-write-table port name cols)
  (set! current-heading cols)
  (when port
    (fprintf port "<h1>~a</h1>" name)
    (fprintf port "<table class=sortable>")
    (fprintf port "<thead><tr>")
    (for ([col (in-list cols)])
      (define name
        (match col
          [(list name _) name]
          [name name]))
      (fprintf port "<th>~a</th>" name))
    (fprintf port "</tr></thead><tbody>")))

(define (html-write-row port row)
  (when port
    (fprintf port "<tr>")
    (for ([cell (in-list row)]
          [heading (in-list current-heading)])
      (define unit
        (match heading
          [(list _ s) s]
          [_ ""]))
      (cond
        [(and (number? cell) (zero? cell)) (fprintf port "<td></td>")]
        [(integer? cell) (fprintf port "<td>~a~a</td>" (~r cell #:group-sep " ") unit)]
        [(real? cell)
         (fprintf port "<td data-sort=~a>~a~a</td>" cell (~r cell #:precision '(= 2)) unit)]
        [else (fprintf port "<td><code>~a</code></td>" cell)]))
    (fprintf port "</tr>")))

(define (html-end-table port)
  (when port
    (fprintf port "</table>")))

(define (html-write-footer port row)
  (when port
    (fprintf port "<tfoot>")
    (html-write-row port row)))

(define (html-write-profile port)
  (when port
    (fprintf port "<section id='profile'><h1>Profiling</h1>")
    (fprintf port "<p class='load-text'>Loading profile data...</p></section>")))

(define (run test-id p timeline-port sollya-reeval)
  (define operation-table
    (and (or (not test-id) (not (string->number test-id))) (make-operation-table test-id)))
  (define-values (expression-table expression-footer)
    (if (and p (or (not test-id) (string->number test-id)))
        (make-expression-table p test-id timeline-port sollya-reeval)
        (values #f #f)))
  (list operation-table expression-table expression-footer))

(define (html-add-plot port path #:width width #:height height)
  (when port
    (fprintf port (format "<img src=\"~a\" width=\"~a\" height=\"~a\">" path width height))))

(define (generate-html html-port profile-port operation-table expression-table expression-footer dir)
  (html-write html-port)

  (when operation-table
    (define cols
      '("Operation" ("Time, 256b" "µs") ("Slowdown" "×") ("Time, 4kb" "µs") ("Slowdown" "×")))
    (html-write-table html-port "Operation timing" cols)
    (for ([row (in-list operation-table)])
      (html-write-row html-port row))
    (html-end-table html-port))

  (when expression-table
    (define cols
      '("#" ("Total" "s")
            ("Compile" "s")
            "Valid"
            ("(s)" "s")
            "Invalid"
            ("(s)" "s")
            "Unable"
            ("(s)" "s")
            ("Memory" "MiB")
            "Baseline-valid, Rival-exit"))
    (html-write-table html-port "Expression timing" cols)
    (for ([row (in-list expression-table)])
      (html-write-row html-port row))
    (when expression-footer
      (html-write-footer html-port expression-footer))
    (html-end-table html-port))

  (when expression-table
    (html-add-plot html-port "ratio_plot_iter.png" #:width 400 #:height 250)
    (html-add-plot html-port "ratio_plot_precision.png" #:width 400 #:height 250)
    (html-add-plot html-port "ratio_plot_precision_base_norm.png" #:width 400 #:height 250)
    (html-add-plot html-port "point_graph.png" #:width 400 #:height 350)
    (html-add-plot html-port "cnt_per_iters_plot.png" #:width 400 #:height 300)
    (html-add-plot html-port "repeats_plot.png" #:width 400 #:height 300)
    (html-add-plot html-port "density_plot.png" #:width 400 #:height 300)
    (html-add-plot html-port "histogram_valid.png" #:width 650 #:height 275)
    (html-add-plot html-port "histogram_all.png" #:width 650 #:height 200))

  (when profile-port
    (html-write-profile html-port)))

(define (profile-json-renderer profile-port)
  (lambda (p order)
    (when profile-port
      (write-json (profile->json p) profile-port))))

(module+ main
  (require racket/cmdline)
  (define dir #f)
  (define html-port #f)
  (define timeline-port #f)
  (define profile-port #f)
  (define sollya-reeval #f)
  (define n #f)
  (command-line
   #:once-each
   [("--dir")
    fn
    "Directory to produce html outputs"
    (set! dir fn)
    (when dir
      (set! timeline-port
            (open-output-file (format "~a/timeline.json" dir) #:mode 'text #:exists 'replace))
      (set! html-port
            (open-output-file (format "~a/index.html" dir) #:mode 'text #:exists 'replace)))]
   [("--profile")
    fn
    "Produce a JSON profile"
    (set! profile-port (open-output-file fn #:mode 'text #:exists 'replace))]
   [("--id") ns "Run a single test" (set! n ns)]
   [("--sollya-reeval") "Reevaluate Sollya" (set! sollya-reeval #t)]
   #:args ([points "infra/points.json"])
   (match-define (list op-t ex-t ex-f)
     (if profile-port
         (profile #:order 'total
                  #:delay 0.001
                  #:render (profile-json-renderer profile-port)
                  (run n (open-input-file points) timeline-port sollya-reeval))
         (run n (open-input-file points) timeline-port sollya-reeval)))
   (when dir
     (generate-html html-port profile-port op-t ex-t ex-f dir))))

(define (point-bucketing timeline
                         rival-status
                         rival-time
                         rival-exs
                         baseline-status
                         baseline-time
                         baseline-exs
                         sollya-status
                         sollya-time
                         sollya-exs
                         baseline-precision
                         rival-iter)

  (define (status-subbucketing status exs)
    (cond
      [(or (equal? exs (fl 0.0)) (equal? exs (fl -0.0))) (format "~a-zero" status)]
      [(flinfinite? exs) (format "~a-inf" status)]
      [else (format "~a-real" status)]))

  (cond
    ; Rival has produced valid outcomes
    [(equal? rival-status 'valid)
     (cond
       ; Every tool have succeded
       ; These points will go into speed graph
       [(and (equal? 'valid sollya-status)
             (equal? 'valid baseline-status)
             (equal? rival-status 'valid)
             (> (*sampling-timeout*) sollya-time)
             (> (*sampling-timeout*) rival-time)
             (> (*sampling-timeout*) baseline-time))
        (timeline-push! timeline
                        'outcomes
                        (list "valid-sollya" rival-iter baseline-precision sollya-time))
        (timeline-push! timeline
                        'outcomes
                        (list "valid-baseline" rival-iter baseline-precision baseline-time))
        (timeline-push! timeline
                        'outcomes
                        (list "valid-rival" rival-iter baseline-precision rival-time))
        (if (or (fl= rival-exs sollya-exs)
                (and (fl= rival-exs (fl 0.0)) (fl= sollya-exs (fl -0.0)))
                (and (fl= rival-exs (fl -0.0)) (fl= sollya-exs (fl 0.0))))
            (timeline-push! timeline 'outcomes (list "sollya-correct-rounding" 0 0 0))
            (timeline-push! timeline 'outcomes (list "sollya-faithful-rounding" 0 0 0)))]

       ; Baseline and Rival have succeeded
       [(and (equal? 'valid baseline-status) (equal? rival-status 'valid))
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-rival+baseline" rival-exs)
                              rival-iter
                              baseline-precision
                              rival-time))]

       ; Baseline and Sollya have succeeded
       [(and (equal? 'valid sollya-status) (equal? 'valid baseline-status))
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-sollya+baseline" baseline-exs)
                              rival-iter
                              baseline-precision
                              sollya-time))]

       ; Sollya and Rival have succeeded
       [(and (equal? 'valid sollya-status) (equal? rival-status 'valid))
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-rival+sollya" rival-exs)
                              rival-iter
                              baseline-precision
                              rival-time))]

       ; Only Rival has succeeded
       [(equal? rival-status 'valid)
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-rival-only" rival-exs)
                              rival-iter
                              baseline-precision
                              rival-time))]

       ; Only Sollya has succeeded
       [(equal? 'valid sollya-status)
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-sollya-only" sollya-exs)
                              rival-iter
                              baseline-precision
                              sollya-time))]

       ; Only Baseline has succeeded
       [(equal? 'valid baseline-status)
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-baseline-only" baseline-exs)
                              rival-iter
                              baseline-precision
                              baseline-time))]

       ; timeout at all the tools
       [else
        (timeline-push! timeline
                        'outcomes
                        (list "exit-baseline" rival-iter baseline-precision baseline-time))
        (timeline-push! timeline
                        'outcomes
                        (list "exit-sollya" rival-iter baseline-precision sollya-time))
        (timeline-push! timeline
                        'outcomes
                        (list "exit-rival" rival-iter baseline-precision rival-time))])]

    ; Rival has exited
    [(equal? rival-status 'unsamplable)
     (cond
       ; Sollya and Baseline have succeeded
       [(and (equal? 'valid sollya-status) (equal? 'valid baseline-status))
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-sollya+baseline" baseline-exs)
                              rival-iter
                              baseline-precision
                              sollya-time))]

       ; Only Sollya has succeeded
       [(equal? 'valid sollya-status)
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-sollya-only" sollya-exs)
                              rival-iter
                              baseline-precision
                              sollya-time))]

       ; Only Baseline has succeeded
       [(equal? 'valid baseline-status)
        (timeline-push! timeline
                        'outcomes
                        (list (status-subbucketing "valid-baseline-only" baseline-exs)
                              rival-iter
                              baseline-precision
                              baseline-time))]

       ; Points that every tools fail to evaluate when the precision is unreacheble
       [else
        (timeline-push! timeline
                        'outcomes
                        (list "exit-baseline" rival-iter baseline-precision baseline-time))
        (timeline-push! timeline
                        'outcomes
                        (list "exit-sollya" rival-iter baseline-precision sollya-time))
        (timeline-push! timeline
                        'outcomes
                        (list "exit-rival" rival-iter baseline-precision rival-time))])]))
