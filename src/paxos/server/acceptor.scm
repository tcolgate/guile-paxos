(define-module (paxos server acceptor)
   #:export (
     start-acceptor)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (acceptor-loop)
  (let ((sck  (init-client))
        (buf  (make-bytevector 64)))
    (while #t
      (let ((count (recv! sck buf)))
      (format #t (utf8->string buf))))))

(define* (start-acceptor)
  "Start an acceptor"
  (acceptor-loop))
