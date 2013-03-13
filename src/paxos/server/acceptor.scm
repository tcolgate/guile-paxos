(define-module (paxos server acceptor)
   #:export (
     start-acceptor)
   #:use-module (paxos misc utils)
   #:use-module (paxos misc statemachine)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))


(use-modules (ice-9 pretty-print))

(define-syntax accepter-autommata
  (lambda (stx)
    "Build an acceptor automata for a corpus of N nodes"
    (let* ((nodes         (cadr (syntax->datum stx)))
           (maj           (majority nodes))
           (learnstates   (let loop ((N   (- maj 1))
                                     (acc '()))
                            (if (> N 0)
                              (loop (- N 1) (append (list (string->symbol
                                                            (string-append
                                                              "learn"
                                                              (number->string N))))
                                                    acc))
                              acc))) 
           (initstates    (append (list 'init) learnstates))
           (activestates  (let loop ((N   (- nodes maj))
                                     (acc '()))
                            (if (>= N 0)
                              (loop (- N 1) (append (list (string->symbol
                                                            (string-append
                                                              "active"
                                                              (number->string N))))
                                                    acc))
                              acc)))
           (allstates (append initstates activestates)))
      (syntax-case stx ()
         ((_ N forms ...)
          #``(automaton init stream-car stream-cdr stream-null? equal?
              (init : ('hello -> #,(datum->syntax stx (cadr allstates)))
                      -> init)
              #,@(map (lambda (prev curr next) 
                         #`(#,(datum->syntax stx curr) : 
                            ('hello -> accept)
                            ('goodbye -> abort))) 
                     allstates
                     (cdr allstates)
                     (cddr allstates))
;              #,@(map (lambda (sym) 
;                        (datum->syntax stx sym)) 
;                      activestates)
              (stop : accept)
              (fail : abort)))))))

(define* (acceptor-loop)
  (let ((sck  (init-client))
        (buf  (make-bytevector 64)))
    (while #t
      (let ((count (recv! sck buf)))
      (format #t (utf8->string buf))))))

(define* (start-acceptor)
  "Start an acceptor"
  (pretty-print (accepter-autommata 5)))

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
;   (active0 : ('goodbye -> learn2
;               'hello   -> active1))
;   (active1 : ('goodbye -> active0
;               'hello   -> active2))
;   (active2 : ('goodbye -> active1)
;           ...
;           -> init)
;   )
