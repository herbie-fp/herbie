#lang racket

(provide startIteration
         endIteration
         startLocalize
         startSubexprs
         addExprs
         
         egraphPrint
         addLocalize
         )

(define iter-num 1)

(define egraphJson (make-hash))
(define curLocalize "")

(define (startIteration) (hash-set! egraphJson iter-num (make-hash)))

(define (egraphPrint)
  (displayln egraphJson))

(define (endIteration) (set! iter-num (+ iter-num 1)))

(define (startLocalize key)
    (set! curLocalize key)
    (hash-set! (getIteration) key (make-hash)))

(define (getIteration) 

(hash-ref egraphJson iter-num))


(define (getlocalizeDict) 

  (hash-ref (getIteration) curLocalize)
  )

(define (startSubexprs) 
 
  (hash-set! (getlocalizeDict) "subexpressions" (make-hash))
  )


(define (getsubexpressionDict) 
  (hash-ref (getlocalizeDict) "subexpressions"))

(define (addExprs key exprs)
  (hash-set! (getIteration) key exprs)
  )

(define (addSubexprss subexprss)
  
  (hash-set! (getsubexpressionDict) 1 subexprss)

)

(define (addLocalize key exprs) (hash-set! (getlocalizeDict) key exprs))


