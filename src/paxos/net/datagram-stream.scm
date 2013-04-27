(define-module (paxos net datagram-stream)
   #:export (
     make-datagram-stream)
   #:use-module (rnrs bytevectors)
   #:use-module (ice-9 format)
   #:use-module (srfi srfi-41)
   #:use-module (paxos net mcast))


; This is largely stolen from srfi-41

(define-stream (make-datagram-stream  recv)
  "make-datagram-stream recver "
  (stream-let loop ((data (recv)))
              (cond
                ((eof-object? data)
                 stream-null)
                (else
                  (stream-cons
                    data
                    (loop (recv)))))))
