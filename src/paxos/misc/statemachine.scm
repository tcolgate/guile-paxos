(define-module (paxos net coroutine)
   #:export (
     call-with-yield)
   #:export-syntax (
     with-yield)
   #:use-module (ice-9 format)
   #:use-module (ice-9 streams)
   #:use-module (paxos misc coroutines)
   )
 

; This macro is taken pretty much wholesale from
; http://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/paper.pdf
; by http://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/paper.pdf
(define-syntax automaton
  (syntax-rules (:)
    ((_ initstate 
        (statename : response ...) 
        ...)
     (let-syntax ((process-state
                  (syntax-rules (accept ->)
                    ((_ accept)
                     (lambda(stream)
                       (cond 
                         ((stream-null? stream) #t)
                         (else #f))))
                    ((_ (label -> target) (...  ...))
                     (lambda(stream)
                       (cond
                         ((stream-null? stream) #f)
                         (else
                           (case (stream-car stream)
                             ((label) (target (stream-cdr stream)))
                             (... ...) 
                             (else #f)))))))))

       (letrec ((statename (process-state response ...))
                 ...)
          initstate )))))

;(define test 
;  (automaton init
;    (init : (1 -> more))
;    (more : (2 -> more)
;            (3 -> more)
;            (4 -> end))
;    (end : accept)))

