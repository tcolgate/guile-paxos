(define-module (paxos net datagram-stream)
   #:export (
     make-datagram-stream)
   #:use-module (rnrs bytevectors)
   #:use-module (ice-9 format)
   #:use-module (srfi srfi-41)
   #:use-module (paxos net mcast))

(define* (make-datagram-stream  recver )
  "make-datagram-stream recver "
  (define-stream (strm recv)
    (let ((data (recv)))
      (cond 
        ((eof-object? data) stream-null)
        ((eq? data #f) (call/cc (lambda (k) k)))
        (else (stream-cons
                data
                (strm recv)))))) 
  (strm recv))
