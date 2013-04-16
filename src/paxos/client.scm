(define-module (paxos client)
   #:export (
     client-loop)
   #:use-module (rnrs bytevectors)
   #:use-module (srfi srfi-41)
   #:use-module (paxos net mcast)
   #:use-module (paxos net datagram-stream))

(define* (client-loop)
  (let ((rcv  (make-datagram-stream (make-mcast-reciever #:blocking #t))))
    (let loop ((thing (stream-car rcv)))
      (format #t "Data: ~a~%" (utf8->string thing)) 
      (loop (stream-cdr rcv)))))

