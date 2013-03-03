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
(define-syntax automaton
  (syntax-rules (:)
    ((_ initstate current next 
        (statename : response ...) 
        ...)
     (let-syntax ((process-state
                  (syntax-rules (accept abort ->)
                   ((_ accept)
                     (lambda(state)
                       (values #t state)))
                   ((_ abort)
                     (lambda(state)
                       (values #f state)))
                  ((_ (label -> target) (...  ...))
                     (lambda(state)
                       (let ((c (current state)))
                         (cond
                           ((equal? label c) (target (next state)))
                           (... ...)
                           (else (values #f state))))))
                  ((_ (label -> target) (...  ...) -> fallback)
                     (lambda(state)
                       (let ((c (current state)))
                         (cond
                           ((equal? label c) (target (next state)))
                           (... ...)
                           (else (fallback (next state))))))))))

       (letrec ((statename (process-state response ...))
                 ...)
          initstate )))))

;(define test 
;  (automaton init stream-car stream-cdr
;    (init  : (1 -> more))
;    (more  : (2 -> more)
;             (3 -> more)
;             (4 -> end)
;             -> other)
;    (other : (5 -> fail)
;             (6 -> end)
;             (7 -> init))
;    (fail  : abort)
;    (end   : accept)))

