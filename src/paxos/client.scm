(define-module (paxos client)
   #:export (
     client-loop)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (client-loop)
  (let ((rcv  (make-mcast-reciever #:blocking #f)))
    (while #t 
      (let ((buf (rcv))) 
        (if (not (eq? buf #f)) 
          (format #t "Data: ~a~%" (utf8->string buf))
          (format #t "no data~%")) 
        (sleep 1)))))

