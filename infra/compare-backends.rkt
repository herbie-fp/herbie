#lang racket

;; Reads two merged Herbie timeline.json files (one per backend) and emits a
;; side-by-side comparison of the egg vs. egglog costs using a nested-subset
;; view so every level of the breakdown is directly comparable:
;;
;;   egg:    ffi-calls ⊂ egg-herbie-wrapper ⊂ stage(taylor/rewrite) ⊂ total
;;   egglog: engine-wall ⊂ process-calls ⊂ egglog-herbie-wrapper ⊂ stage ⊂ total
;;
;; `process-wall` is the summed wall time of every egglog-send round-trip
;; (between writing the command and reading the `(done <nanos>)` terminator).
;; `engine-wall` is the summed nanos reported by the egglog-timed wrapper —
;; the time `parse_and_run_program` actually spent in-process. Subset holds
;; by construction because engine is measured inside each send.
;;
;; Usage:
;;   racket -y infra/compare-backends.rkt <egg-dir> <egglog-dir> <out-dir>
;;
;; NOTE: assumes both runs used the same seed and benchmark set, and that
;; `dump:egglog` was NOT set (it doubles IPC cost).

(require json
         racket/cmdline
         (only-in xml write-xexpr))

(define (read-timeline path)
  (call-with-input-file path read-json))

(define (collect-rows timeline field)
  (apply append
         (for/list ([group (in-list timeline)]
                    #:when (and (hash? group) (hash-has-key? group field)))
           (define v (hash-ref group field))
           (if (list? v)
               v
               '()))))

;; Group hash by pipeline stage. `series` is herbie's internal name for taylor.
;; Stage wall = live wall-clock time of all events matching `stage-type`;
;; we add gc-time back to keep live-wall measurements (like the egglog
;; surround) a strict subset: timeline-reattribute-gc strips GC from `time`.
(define (stage-total timeline stage-type)
  (for/sum ([group (in-list timeline)] #:when (and (hash? group)
                                                   (equal? (hash-ref group 'type #f) stage-type)))
    (+ (hash-ref group 'time 0) (hash-ref group 'gc-time 0))))

(define (rows-for-stage timeline stage-type field)
  ;; egg/egglog-phase and egglog-send events carry `stage` in-row, so a single
  ;; filter works whether or not the outer group type matches.
  (define stage-label
    (match stage-type
      [(or "rewrite" 'rewrite) "rewrite"]
      [(or "series" 'series "taylor") "taylor"]
      [s (format "~a" s)]))
  (for/list ([r (in-list (collect-rows timeline field))]
             #:when (equal? (stage-field-for field r) stage-label))
    r))

(define (stage-field-for field r)
  (match field
    ['egg-phase (second r)] ; (time stage phase)
    ['egglog-phase (second r)] ; (wall stage phase)
    ['egglog-send (list-ref r 3)])) ; (count wall engine stage phase cmd)

(define (egg-summary timeline stage-type)
  (define rows (rows-for-stage timeline stage-type 'egg-phase))
  (define (partition-surround rs)
    (partition (lambda (r) (equal? (third r) "surround")) rs))
  (define-values (surround-rows ffi-rows) (partition-surround rows))
  (define surround-ms (for/sum ([r (in-list surround-rows)]) (first r)))
  (define ffi-ms (for/sum ([r (in-list ffi-rows)]) (first r)))
  (hash 'stage-wall
        (stage-total timeline stage-type)
        'herbie-wrapper-wall
        surround-ms
        'ffi-wall
        ffi-ms))

(define (egglog-summary timeline stage-type)
  (define phase-rows (rows-for-stage timeline stage-type 'egglog-phase))
  (define send-rows (rows-for-stage timeline stage-type 'egglog-send))
  (define surround-rows (filter (lambda (r) (equal? (third r) "surround")) phase-rows))
  (define surround-ms (for/sum ([r (in-list surround-rows)]) (first r)))
  (define process-ms (for/sum ([r (in-list send-rows)]) (second r)))
  (define engine-ms (for/sum ([r (in-list send-rows)]) (third r)))
  (hash 'stage-wall
        (stage-total timeline stage-type)
        'herbie-wrapper-wall
        surround-ms
        'process-wall
        process-ms
        'engine-wall
        engine-ms))

(define (total-wall timeline)
  (for/sum ([group (in-list timeline)] #:when (and (hash? group)
                                                   (not (equal? (hash-ref group 'type #f) "gc"))))
    (+ (hash-ref group 'time 0) (hash-ref group 'gc-time 0))))

;; Nested rows: each row is (label, wall-ms, share-of-stage-%)
(define (nest-rows/egg egg)
  (define stage (hash-ref egg 'stage-wall))
  (define herbie (hash-ref egg 'herbie-wrapper-wall))
  (define ffi (hash-ref egg 'ffi-wall))
  (list (list "stage" stage (pct stage stage))
        (list "  egg-herbie wrapper" herbie (pct herbie stage))
        (list "    egg ffi calls" ffi (pct ffi stage))))

(define (nest-rows/egglog eg)
  (define stage (hash-ref eg 'stage-wall))
  (define herbie (hash-ref eg 'herbie-wrapper-wall))
  (define process (hash-ref eg 'process-wall))
  (define engine (hash-ref eg 'engine-wall))
  (list (list "stage" stage (pct stage stage))
        (list "  egglog-herbie wrapper" herbie (pct herbie stage))
        (list "    egglog process calls" process (pct process stage))
        (list "      engine (rust wall)" engine (pct engine stage))))

(define (pct part whole)
  (if (or (zero? whole) (not (real? part)))
      0
      (* 100 (/ part whole))))

(define (format-ms x)
  (real->decimal-string (if (real? x) x 0.0) 3))
(define (format-pct x)
  (real->decimal-string (if (real? x) x 0.0) 1))

;; Per-command histogram (still useful for spotting hot commands, but shown
;; separately from the main nested summary since it's diagnostic).
(define (send-histogram timeline)
  (define rows (collect-rows timeline 'egglog-send))
  (define h (make-hash))
  (for ([r (in-list rows)])
    (define k (list (list-ref r 3) (list-ref r 4) (list-ref r 5))) ; stage phase cmd
    (hash-update!
      h
      k
      (lambda (o)
        (for/list ([a (in-list o)]
                   [b (in-list (list (first r) (second r) (third r)))])
          (+ a b)))
      (lambda () (list 0 0 0))))
  (define entries
    (for/list ([(k v) (in-hash h)])
      (append k v)))
  (sort entries > #:key (lambda (row) (list-ref row 4)))) ; by wall

(define (write-csv path rows header)
  (call-with-output-file
    path
    #:exists 'replace
    (lambda (out)
      (displayln (string-join (map ~a header) ",") out)
      (for ([r (in-list rows)])
        (displayln (string-join (map (lambda (v)
                                       (cond
                                         [(and (real? v) (rational? v)) (real->decimal-string v 4)]
                                         [else (~a v)]))
                                     r)
                                ",")
                   out)))))

(define (write-nested-csv path egg-total egglog-total stages)
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (displayln "backend,stage,level,ms,share_of_stage_pct" out)
      (fprintf out "egg,TOTAL,total,~a,~a\n" (format-ms egg-total) "100.0")
      (fprintf out "egglog,TOTAL,total,~a,~a\n" (format-ms egglog-total) "100.0")
      (for ([pair (in-list stages)])
        (match-define (list stage-name egg eg) pair)
        (for ([r (in-list (nest-rows/egg egg))])
          (match-define (list label ms share) r)
          (fprintf out
                   "egg,~a,~a,~a,~a\n"
                   stage-name
                   (trim-indent label)
                   (format-ms ms)
                   (format-pct share)))
        (for ([r (in-list (nest-rows/egglog eg))])
          (match-define (list label ms share) r)
          (fprintf out
                   "egglog,~a,~a,~a,~a\n"
                   stage-name
                   (trim-indent label)
                   (format-ms ms)
                   (format-pct share)))))))

(define (trim-indent s)
  ;; Replace leading spaces with nesting count for CSV friendliness.
  (define n
    (for/or ([i (in-naturals)]
             [c (in-string s)]
             #:when (not (char=? c #\space)))
      i))
  (format "~a~a" (make-string (or n 0) #\space) (substring s (or n 0))))

;; Renders a side-by-side table that respects the fact that egg and egglog
;; have different innermost layers (egg bottoms out at ffi calls, egglog has
;; an extra engine-wall level below process calls). Rows:
;;   stage                    — both backends
;;   *-herbie wrapper         — both backends
;;   egg ffi calls            — egg only
;;   egglog process calls     — egglog only
;;   engine (rust wall)       — egglog only
(define (nested-table title egg eg)
  (define es (hash-ref egg 'stage-wall))
  (define ew (hash-ref egg 'herbie-wrapper-wall))
  (define ef (hash-ref egg 'ffi-wall))
  (define xs (hash-ref eg 'stage-wall))
  (define xw (hash-ref eg 'herbie-wrapper-wall))
  (define xp (hash-ref eg 'process-wall))
  (define xe (hash-ref eg 'engine-wall))
  (define (row label egg-ms eg-ms egg-share eg-share)
    `(tr (td ,label)
         (td ,(if egg-ms
                  (format-ms egg-ms)
                  "—"))
         (td ,(if eg-ms
                  (format-ms eg-ms)
                  "—"))
         (td ,(string-join (filter values
                                   (list (and egg-share (format "egg ~a%" (format-pct egg-share)))
                                         (and eg-share (format "egglog ~a%" (format-pct eg-share)))))
                           " / "))))
  `(table (tr (th ((colspan "4")) ,title))
          (tr (th "Level") (th "egg ms") (th "egglog ms") (th "% of stage"))
          ,(row "stage" es xs (pct es es) (pct xs xs))
          ,(row "  herbie wrapper" ew xw (pct ew es) (pct xw xs))
          ,(row "    egg ffi calls" ef #f (pct ef es) #f)
          ,(row "    egglog process calls" #f xp #f (pct xp xs))
          ,(row "      engine (rust wall)" #f xe #f (pct xe xs))))

(define (html-report total-egg total-egglog stage-summaries hist-rows)
  `(html (head (meta ([charset "utf-8"]))
               (title "Egg vs. Egglog Backend Comparison")
               (style "body{font-family:sans-serif;margin:2em;}"
                      "table{border-collapse:collapse;margin-bottom:2em;}"
                      "th,td{border:1px solid #ccc;padding:4px 8px;text-align:right;}"
                      "th:first-child,td:first-child{text-align:left;font-family:monospace;}"
                      "h2{margin-top:2em;}"))
         (body (h1 "Egg vs. Egglog Backend Comparison")
               (h2 "Totals")
               (table (tr (th "Backend") (th "Total wall"))
                      (tr (td "egg") (td ,(format-ms total-egg)))
                      (tr (td "egglog") (td ,(format-ms total-egglog))))
               ,@(for/list ([p (in-list stage-summaries)])
                   (match-define (list name egg eg) p)
                   `(div (h2 ,(format "Stage: ~a" name))
                         ,(nested-table (format "~a stage breakdown" name) egg eg)))
               (h2 "Egglog send histogram")
               (table (tr (th "Stage")
                          (th "Phase")
                          (th "Command")
                          (th "Count")
                          (th "Wall")
                          (th "Engine"))
                      ,@(for/list ([r (in-list hist-rows)]
                                   [_ (in-range 30)])
                          (match-define (list stage phase cmd count wall engine) r)
                          `(tr (td ,stage)
                               (td ,phase)
                               (td ,(~a cmd))
                               (td ,(~a count))
                               (td ,(format-ms wall))
                               (td ,(format-ms engine))))))))

(define (write-html path report)
  (call-with-output-file path
    #:exists 'replace
    (lambda (out)
      (displayln "<!DOCTYPE html>" out)
      (write-xexpr report out))))

(module+ main
  (command-line
    #:args (egg-dir egglog-dir out-dir)
    (unless (directory-exists? out-dir)
      (make-directory out-dir))
    (define egg-tl (read-timeline (build-path egg-dir "timeline.json")))
    (define egglog-tl (read-timeline (build-path egglog-dir "timeline.json")))
    (define total-egg (total-wall egg-tl))
    (define total-egglog (total-wall egglog-tl))
    (define stages
      (for/list ([s (in-list '(("rewrite" "rewrite") ("taylor" "series")))])
        (match-define (list display-name group-type) s)
        (list display-name (egg-summary egg-tl group-type) (egglog-summary egglog-tl group-type))))
    (define hist (send-histogram egglog-tl))
    (write-nested-csv (build-path out-dir "nested.csv") total-egg total-egglog stages)
    (write-csv (build-path out-dir "egglog_sends.csv")
               hist
               '("stage" "phase" "cmd" "count" "wall_ms" "engine_ms"))
    (write-html (build-path out-dir "summary.html") (html-report total-egg total-egglog stages hist))
    (printf "wrote nested breakdown + ~a send rows to ~a\n" (length hist) out-dir)))
