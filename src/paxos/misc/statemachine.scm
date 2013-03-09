(define-module (paxos misc statemachine)
   #:export-syntax (
     automaton)
   #:use-module (ice-9 format)
   #:use-module (ice-9 streams)
   #:use-module (ice-9 pretty-print)
   #:use-module (paxos misc coroutines)
   )
 

; This macro is taken pretty much wholesale from
; http://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/paper.pdf
(define-syntax automaton
  (syntax-rules (:)
    ((_ initstate current next 
        (statename : response ...) 
        ...)
     (letrec-syntax 
       ((process-transition-test
          (syntax-rules ()
                        ((_ C label)
                         (equal? label C)))) 
        (process-transition-action
          (syntax-rules ()
                        ((_ state target)
                         (target (next state)))))
        (process-state
          (syntax-rules (accept abort ->)
                        ((_ accept)
                         (lambda(state)
                           (values #t state)))
                        ((_ abort)
                         (lambda(state)
                           (values #f state)))
                        ((_  (label -> target) (... ...))
                         (lambda(state)
                           (let ((c (current state)))
                             (cond
                               ((process-transition-test c label) 
                                (process-transition-action state target)) 
                               (... ...)
                               (else (values #f state))))))
                        ((_  (label -> target) (... ...) -> fallback)
                         (lambda(state)
                           (let ((c (current state)))
                             (cond
                               ((process-transition-test c label) 
                                (process-transition-action state target)) 
                               (... ...)
                               (else (fallback (next state))))))))))

       (letrec ((statename (process-state response ...))
                ...)
         initstate )))))



