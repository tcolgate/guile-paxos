(define-module (paxos server learner)
   #:export (
     start-learner)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))

