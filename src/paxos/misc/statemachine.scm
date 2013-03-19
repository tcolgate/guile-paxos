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
        (define-syntax automaton
          (syntax-rules
            ()
            ((_ forms ...)
             (letrec-syntax
               ((process-automaton
                  (lambda (stx)
                    (syntax-case
                      stx ()
                      ((_ initstate current next empty? isequal?
                          (statespec (... ...))
                          (... ...))
                       (let
                         ((specs (match (syntax->datum stx)
                                        ((_ _ _ _ _ _
                                            (spec (... ...))
                                            (... ...))
                                         spec))))
                         (let* ((initname (syntax->datum (syntax initstate)))
                                (states (map ; (statename (lambda...))
                                          (lambda(spec)
                                            (let-values
                                              (((sn n b a srs)
                                                (match spec
                                                       ((sn ': srs (... ...))
                                                        (values
                                                          (datum->syntax stx sn)
                                                          #f #f #f
                                                          (datum->syntax stx srs)))
                                                       ((sn n b a ': srs (... ...))
                                                        (values
                                                          (datum->syntax stx sn)
                                                          n
                                                          b
                                                          a
                                                          (datum->syntax stx srs))))))

                                              (if (and (integer? n)
                                                       (> n 0))
                                                ; This specifies a ladder of states
                                                (cons sn
                                                      #`(process-state-responses
                                                          #,sn
                                                          (quote #,sn)
                                                          current next empty? isequal?
                                                          #,@srs))
                                                ; An individual state
                                                (cons sn
                                                      #`(process-state-responses
                                                          #,sn
                                                          (quote #,sn)
                                                          current next empty? isequal?
                                                          #,@srs)))))
                                          specs))
                                (names (map car states))
                                (funcs (map cdr states)))

                           #`(build-automaton-letrec
                               ; This  "recasts" this variable into the current syntax
                               ; environment, it works anyway!
                               #,(datum->syntax stx (syntax->datum (syntax initstate)))
                               #,names #,funcs)))))))

                (build-automaton-letrec
                  (lambda (stx)
                    (syntax-case stx
                                 ()
                                 ((_ initstate (sns (... ...)) (funcs (... ...)))
                                  #`(letrec ((sns funcs)
                                             (... ...))
                                      initstate)))))

                (process-transition-test
                  (syntax-rules
                    ()
                    ((_ C label isequal?)
                     (isequal? label C))))

                (process-transition-action
                  (syntax-rules
                    ()
                    ((_ state targetfunc targetsym srcfunc srcsym next hooks)
                     (targetfunc
                       (next
                         (fold
                           apply
                           state
                           hooks
                           (make-list (length hooks) (cons srcsym srcfunc))
                           (make-list (length hooks) (cons targetsym targetfunc))))))))

                (process-state-responses
                  (lambda(stx)
                    (syntax-case
                      stx (accept abort alias ->)
                      ((_ srcfunc
                          srcsym
                          current next empty? isequal?
                          accept)
                       #`(lambda(state)
                           (values #t state (cons srcsym srcfunc))))

                      ((_ srcfunc
                          srcsym
                          current next empty? isequal?
                          abort)
                       #`(lambda(state)
                           (values #f state (cons srcsym srcfunc))))

                      ((_ srcfunc
                          srcsym
                          current next empty? isequal?
                          alias target)
                       #`(lambda(state)
                           (target state)))

                      ((_ srcfunc
                          srcsym
                          current next empty? isequal?
                          (label -> target hooks (... ...)) (... ...))
                       #`(process-state-responses
                           srcfunc
                           srcsym
                           current next empty? isequal?
                           (label -> target hooks (... ...)) (... ...)
                           -> #f))

                      ((_ srcfunc
                          srcsym
                          current next empty? isequal?
                          (label -> target hooks (... ...)) (... ...)
                          -> fallback)
                       #`(lambda(state)
                           (if (empty? state)
                             (values #f state (cons srcsym srcfunc))
                             (let ((c (current state)))
                               (cond
                                 ((process-transition-test
                                    c label isequal?)
                                  (process-transition-action
                                    state target (quote target) srcfunc srcsym next (list hooks (... ...))))
                                 (... ...)
                                 (else
                                   #,(if (eq? #f (let* ((l (syntax->datum stx))
                                                        (len (length l)))
                                                   (list-ref l (- len 1))))
                                       #`(values #f state)
                                       #`(fallback (next state)))))))))))))

               (process-automaton forms ...)))))


