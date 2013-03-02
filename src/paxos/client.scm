(define-module (paxos client)
   #:export (
     client-loop)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (client-loop)
  (let ((sck  (init-client))
        (buf  (make-bytevector 64)))
    (while #t 
      (let ((count (recv! sck buf))) 
      (format #t (utf8->string buf))))))

