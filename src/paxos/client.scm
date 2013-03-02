(define-module (paxos client)
   #:export (
     client-loop)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (client-loop)
  (let ((rcv  (make-mcast-reciever)))
    (while #t 
      (let ((buf (rcv))) 
        (format #t (utf8->string buf))
        (sleep 1)))))

