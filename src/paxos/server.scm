(define-module (paxos server)
   #:export (
     start-server)
   #:use-module (ice-9 format)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

(define* (sender-loop)
  (let ((snd  (make-mcast-sender)))
    (while #t 
      (snd (string->utf8 "This is my message to you\n"))
      (sleep 1))))

(define (majority N)
  (inexact->exact  (if (odd? N) 
   (+ (/ N 2) 0.5) 
   (+ (/ N 2) 1))))

(define* (start-server #:key (acceptors 3))
  "Start a paxos service with a stated number of acceptors"
  (format #t "Start cluster of ~d needing atleast ~d~%" acceptors (majority acceptors))
  (sender-loop))
