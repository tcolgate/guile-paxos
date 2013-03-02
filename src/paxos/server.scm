(define-module (paxos server)
   #:export (
     sender-loop)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (sender-loop)
  (let ((sck  (init-sender))
        (addr (inet-pton AF_INET "224.0.1.1" )) 
        (port (htons 1234)))
    (while #t 
      (sendto sck (string->utf8 "This is my message to you\n") AF_INET addr port)
      (sleep 1))))
