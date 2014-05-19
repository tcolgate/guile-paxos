(library
  (paxos server)
  (export
    start-server)
  (import
    (paxos misc utils)
    (paxos net mcast)
    (rnrs)
    (rnrs bytevectors)
    (ice-9 optargs)
    (ice-9 format))

  (define*
    (sender-loop)
    (let ((snd  (make-mcast-sender)))
      (let loop ((i 0))
        (snd
          (string->utf8 (number->string i)))
        (sleep 1)
        (loop (if (> i 5)
                0
                (+ i 1))))))

  (define*
    (start-server
      #:key (acceptors 3)
      (acceptorsGroup "224.0.1.1")
      (proposersGroup "224.0.1.2")
      (learnersGroup  "224.0.1.2"))
    "Start a paxos service with a stated number of acceptors"
    (format #t "Start cluster of ~d needing atleast ~d~%" acceptors (majority acceptors))
    (sender-loop)))
