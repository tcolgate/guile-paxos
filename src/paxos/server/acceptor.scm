(define-module (paxos server acceptor)
   #:export (
     start-acceptor)
   #:use-module (paxos misc utils)
   #:use-module (paxos misc statemachine)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))


(define-syntax accepter-autommata
  (lambda (stx)
    "Build an acceptor automata for a corpus of N nodes"
    (let* ((count         (majority (cadr (syntax->datum stx))))
           (learnstates   (let loop ((N   (- count 1))
                                     (acc '()))
                            (if (> N 0)
                              (loop (- N 1) (append (list (string->symbol
                                                            (string-append
                                                              "learn"
                                                              (number->string N))))
                                                    acc))
                              acc)))
           (states (append (list 'init) learnstates (list 'active))))
      (syntax-case stx ()
         ((_ N forms ...)
          #``(automaton init stream-car stream-cdr stream-null?
                        #,@(map (lambda (sym) (datum->syntax stx sym)) states)))))))

(define* (acceptor-loop)
  (let ((sck  (init-client))
        (buf  (make-bytevector 64)))
    (while #t
      (let ((count (recv! sck buf)))
      (format #t (utf8->string buf))))))

(define* (start-acceptor)
  "Start an acceptor"
  (format #t "~a~%" (accepter-autommata 9))
  )

(start-acceptor)

; (automaton ... ... ... ...
;   (init : ('hello -> learn1)
;           -> init)
;   (learn1 : ('hello -> learn2)
;            ('goodbye -> init)
;           -> init)
;   (learn2 : ('hello -> active)
;            ('goodbye -> learn1)
;           -> init)
;   (active : ('goodbye -> learn2)
;           ...
;           -> init)
;   )
