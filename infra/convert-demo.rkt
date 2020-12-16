#lang racket

(require json)
(require "../src/programs.rkt")


(define op-map
  (make-hash
   `((expt . pow)
     (sqr . exp2)
     (abs . fabs))))

(define version-10-constants
  (make-hash
   `((e . E)
     (pi . PI))))

(define (format-op expr)
  (if (equal? (first expr) 'cube)
      (cons 'pow (append (rest expr) (list 3)))
      (cons (hash-ref op-map (first expr) (first expr)) (rest expr))))


(define (format-expr is-version-10 expr)
  (match expr
    [(? string?)
     (let ([parsed (string->number expr)])
       (if parsed
           parsed
           (raise (error "string that is not a num"))))]
    [(list op args ...)
     (format-op (cons op (map (curry format-expr is-version-10) args)))]
    [(? symbol?)
     (if is-version-10
         (hash-ref version-10-constants expr expr)
         expr)]
    [else
     expr]))


(define (read-expr expr-string is-version-10)
  (format-expr is-version-10 (call-with-input-string expr-string read)))

(define (make-fpcore expr)
  (let ([vars (free-variables expr)])
    (format "(FPCore (~a) ~a)"
            (string-join (map symbol->string vars) " ")
            expr)))

(define (convert-file file-name output-file existing-set is-version-10)
  (define file-port (open-input-file file-name))
  (define tests (hash-ref (read-json file-port) 'tests))
  (define exprs-unfiltered
    (for/list ([test tests])
      (read-expr (hash-ref test 'input) is-version-10)))
  (define exprs
    (for/set ([expr exprs-unfiltered]
              #:when (not (set-member? existing-set expr)))
      expr))
  (for ([expr (in-set exprs)])
    (fprintf output-file "~a\n"
             (make-fpcore expr)))
  exprs)
  

(define (convert-files json-files output-files expr-set is-version-10)
  (unless (empty? json-files)
    (define json-file (first json-files))
    
    (define sub-path
      (path-replace-extension
       (file-name-from-path (string->path json-file)) ".fpcore"))
    
    (define output-file
      (open-output-file (first output-files)
                        #:exists 'replace))
    (fprintf (current-output-port) "Creating file ~a\n" (path->string sub-path))
    
    (define new-expr-set
      (set-union expr-set (convert-file json-file output-file expr-set is-version-10)))
    
    (convert-files (rest json-files) (rest output-files) new-expr-set #f)))
  
(module+ main
  (define rebuilding? #f)
  (command-line 
   #:program "convert-demo"
   #:args (input-json output-json)
   (convert-files (list input-json) (list output-json) (set) #t)))

