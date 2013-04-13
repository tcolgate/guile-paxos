        (define-module (paxos misc statemachine)
           #:export-syntax (
             automaton)
           #:use-module (srfi srfi-1)
           #:use-module (srfi srfi-11)
           #:use-module (ice-9 match)
           #:use-module (ice-9 format)
           #:use-module (ice-9 streams)
           #:use-module (ice-9 pretty-print)
           #:use-module (paxos misc coroutines))


        ; This macro started life as:
        ; http://cs.brown.edu/~sk/Publications/Papers/Published/sk-automata-macros/paper.pdf
        ; The hooks format is:
        ; (lambda(srcinfo targetinfo state)
        ;   ...
        ;   state)
        ;
        ;   The ladder generation makes this all a lot uglier
        ;   the pmatch could probably be avoided with better syntax-rules use
        ;   It needs breaking up a bit
        ;
        (define-syntax automaton
          (syntax-rules
            ()
            ((_ forms ...)
             (letrec-syntax

               ((process-automaton
                  (syntax-rules (:hooks)
                                ((_ initstate current next empty? isequal?
                                    (sn : srs (... ...))
                                    (... ...))
                                 (process-automaton initstate current next empty? isequal?
                                                    (sn : srs (... ...))
                                                    (... ...)
                                                    :hooks () ))
                                ((_ initstate current next empty? isequal?
                                    (sn : srs (... ...))
                                    (... ...)
                                    :hooks (ghooks (... ...))) 
                                 (expand-all-states
                                   initstate
                                   (sn (process-state-responses 
                                         sn current next empty? isequal? 
                                         (list ghooks (... ...)) srs (... ...))) 
                                   (... ...)))))

                (expand-all-states
                  (syntax-rules ()
                                ((_ initstate (name func) (... ...))
                                 (build-automaton-letrec initstate
                                                         (name (... ...))
                                                         (func (... ...))))))

                (build-automaton-letrec
                  (syntax-rules ()
                                ((_ initstate (sn (... ...)) (func (... ...)))
                                 (letrec ((sn func)
                                          (... ...))
                                   initstate))))

                (process-transition-test
                  (syntax-rules
                    ()
                    ((_ C label isequal?)
                     (isequal? label C))))

                (process-transition-action
                  (syntax-rules
                    (abort)
                    ((_ state abort src current next hooks)
                     (values #f state (cons (quote src) src))) 
                    ((_ state target src current next hooks)
                     (target
                       (next (call-hooks state target src current next hooks)))) ))

                (call-hooks
                  (syntax-rules
                    ()
                    ((_ state target src current next hooks)
                     (fold
                       (lambda(h s t c)
                         (h s t c))
                       state
                       hooks
                       (make-list (length hooks) (cons (quote src) src))
                       (make-list (length hooks) (cons (quote target) target))))))

                (process-state-responses
                  (syntax-rules
                    (accept abort alias invalid -> :hooks)
                    ((_ sn
                        current next empty? isequal? ghooks
                        accept)
                     (lambda(state)
                       (values #t state (cons (quote sn) sn))))

                    ((_ sn
                        current next empty? isequal? ghooks
                        abort)
                     (lambda(state)
                       (values #f state (cons (quote sn) sn))))

                    ((_ sn
                        current next empty? isequal? ghooks
                        alias target)
                     (lambda(state)
                       (target state)))

                    ((_ sn
                        current next empty? isequal? ghooks
                        (label -> target hooks (... ...)) (... ...))
                     (process-state-responses
                       sn
                       current next empty? isequal? ghooks
                       (label -> target hooks (... ...)) (... ...)
                       -> abort))

                    ((_ sn
                        current next empty? isequal? ghooks
                        (label -> target hooks (... ...)) (... ...)
                        -> fallback fbhooks (... ...))
                     (lambda(state)
                       (if (empty? state)
                         (values #f state (cons (quote sn) sn))
                         (cond
                           ((process-transition-test
                              (current state) label isequal?)
                            (process-transition-action
                              state target sn current next (append ghooks (list hooks (... ...)))))
                           (... ...)
                           (else
                             (process-transition-action
                               state fallback sn current next (list fbhooks (... ...)))))))))))

               (process-automaton forms ...)))))

;(let
;                         ((specs (match (syntax->datum stx)
;                                        ((_ _ _ _ _ _
;                                            (spec (... ...))
;                                            (... ...))
;                                         spec))))
;                         (let* ((initname (syntax->datum (syntax initstate)))
;                                (states (append-map ; (statename (lambda...))
;                                          (lambda(spec)
;                                            (let-values
;                                              (((sn n b a srs)
;                                                (match spec
;                                                       ((sn ': srs (... ...))
;                                                        (values sn #f #f #f srs))
;                                                       ((sn n b a ': srs (... ...))
;                                                        (values sn n b a srs)))))
;                                              (let ((sn/stx (datum->syntax stx sn))
;                                                    (srs/stx (datum->syntax stx srs)))
;                                                (if (and (integer? n)
;                                                         (>= n 0))
;                                                  ; This specifies a ladder of states
;                                                  (let* ((before (list (datum->syntax stx b)))
;                                                         (after  (list (datum->syntax stx a)))
;                                                         (intermediate
;                                                           (let loop ((N n) (acc '()))
;                                                             (if (> N 0)
;                                                               (loop
;                                                                 (- N 1)
;                                                                 (append
;                                                                   (list
;                                                                     (datum->syntax
;                                                                       stx
;                                                                       (string->symbol
;                                                                         (string-append
;                                                                           (symbol->string sn)
;                                                                           "/"
;                                                                           (number->string N)))))
;                                                                   acc))
;                                                               acc)))
;                                                         (headsym (datum->syntax
;                                                                    stx
;                                                                    (string->symbol
;                                                                      (string-append
;                                                                        (symbol->string sn)
;                                                                        "/head"))))
;                                                         (tailsym (datum->syntax
;                                                                    stx
;                                                                    (string->symbol
;                                                                      (string-append
;                                                                        (symbol->string sn)
;                                                                        "/tail"))))
;                                                         (prevsym (datum->syntax
;                                                                    stx
;                                                                    (string->symbol
;                                                                      (string-append
;                                                                        (symbol->string sn)
;                                                                        "/prev"))))
;                                                         (nextsym (datum->syntax
;                                                                    stx
;                                                                    (string->symbol
;                                                                      (string-append
;                                                                        (symbol->string sn)
;                                                                        "/next"))))
;                                                         (allstates (append before intermediate after))
;                                                         ; The head of the ladder is the second state
;                                                         (headtarg  (list-ref allstates 1))
;                                                         ; The tail of the ladder is the penultimate state
;                                                         (tailtarg  (list-ref allstates (+ n 1))))
;                                                    (append
;                                                      (list
;                                                        (cons headsym
;                                                              #`(process-state-responses
;                                                                  #,headsym
;                                                                  (quote #,headsym)
;                                                                  current next empty? isequal?
;                                                                  alias #,headtarg))
;
;                                                        (cons tailsym
;                                                              #`(process-state-responses
;                                                                  #,tailsym
;                                                                  (quote #,tailsym)
;                                                                  current next empty? isequal?
;                                                                  alias #,tailtarg)))
;                                                      (let loop ((N 1)
;                                                                 (acc '()))
;                                                        (if (<= N n)
;                                                          (loop
;                                                            (+ N 1)
;                                                            (append
;                                                              acc
;                                                              (let ((lcurr (list-ref allstates N))
;                                                                    (lprev (list-ref allstates (- N 1)))
;                                                                    (lnext (list-ref allstates (+ N 1))))
;                                                                (list
;                                                                  (cons
;                                                                    lcurr
;                                                                    #`(let
;                                                                        ((#,prevsym (lambda(s)(#,lprev s)))
;                                                                         (#,nextsym (lambda(s)(#,lnext s)))
;                                                                         (#,sn/stx  (lambda(s)(#,lcurr s))))
;                                                                        (process-state-responses
;                                                                          #,sn/stx
;                                                                          (quote #,sn/stx)
;                                                                          current next empty? isequal?
;                                                                          #,@srs/stx)))))))
;                                                          acc))))
;                                                  ; An individual state
;                                                  (list
;                                                    (cons sn/stx
;                                                        #`(process-state-responses
;                                                            #,sn/stx
;                                                            (quote #,sn/stx)
;                                                            current next empty? isequal?
;                                                            #,@srs/stx)))))))
;                                          specs))
;                                (names (map car states))
;                                (funcs (map cdr states)))
;
;                           #`(build-automaton-letrec
;                               ; This  "recasts" this variable into the current syntax
;                               ; environment, it works anyway!
;                               #,(datum->syntax stx (syntax->datum (syntax initstate)))
;                               #,names #,funcs)))
