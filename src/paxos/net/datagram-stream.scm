(library
  (paxos net datagram-stream)
  (export
    make-datagram-stream)
  (import
    (paxos net mcast)
    (rnrs bytevectors)
    (srfi srfi-41))


  ; This is largely stolen from srfi-41
  (define-stream
    (make-datagram-stream  recv)
    "make-datagram-stream recver "
    (stream-let loop ((data (recv)))
                (cond
                  ((eof-object? data)
                   stream-null)
                  (else
                    (stream-cons
                      data
                      (loop (recv))))))))
