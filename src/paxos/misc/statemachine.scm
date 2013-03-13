(define-module (paxos misc statemachine)
   #:export-syntax (
     automaton)
   #:use-module (srfi srfi-1)
   #:use-module (ice-9 format)
   #:use-module (ice-9 streams)
   #:use-module (ice-9 pretty-print)
   #:use-module (paxos misc coroutines)
   )
 

; This macro started life as:
; http://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/paper.pdf
; The hooks format is:
; (lambda(srcinfo targetinfo state)
;   ...
;   state)
(define-syntax automaton
  (syntax-rules (:)
    ((_ initstate current next empty? isequal?
        (statename : response ...) 
        ...)
     (letrec-syntax 
       ((process-transition-test
          (syntax-rules ()
                        ((_ C label)
                         (isequal? label C)))) 
        (process-transition-action
          (syntax-rules ()
                        ((_ state targetfunc targetsym srcfunc srcsym hooks)
                         (targetfunc 
                           (next 
                             (fold 
                               apply 
                               state 
                               hooks
                               (make-list (length hooks) (cons srcsym srcfunc))
                               (make-list (length hooks) (cons targetsym targetfunc)) 
                               ))))))
        (process-state
          (syntax-rules (accept abort ->)
                        ((_ srcfunc srcsym accept)
                         (lambda(state)
                           (values #t state (cons srcsym srcfunc))))
                        ((_ srcfunc srcsym abort)
                         (lambda(state)
                           (values #f state (cons srcsym srcfunc))))
                        ((_ srcfunc srcsym  (label -> target hooks (... ...)) (... ...))
                         (lambda(state)
                           (if (empty? state)
                             (values #f state (cons srcsym srcfunc))
                             (let ((c (current state)))
                               (cond
                                 ((process-transition-test c label)
                                  (process-transition-action 
                                    state target (quote target) srcfunc srcsym (list hooks (... ...))))
                                 (... ...)
                                 (else (values #f state)))))))
                        ((_ srcfunc srcsym (label -> target hooks (... ...)) (... ...) -> fallback)
                         (lambda(state)
                           (if (empty? state)
                             (values #f state (cons srcsym srcfunc))
                             (let ((c (current state)))
                               (cond
                                 ((process-transition-test c label)
                                  (process-transition-action 
                                    state target (quote target) srcfunc srcsym (list hooks (... ...))))
                                 (... ...)
                                 (else (fallback (next state)))))))))))

       (letrec ((statename (process-state statename (quote statname) response ...))
                ...)
         initstate )))))


