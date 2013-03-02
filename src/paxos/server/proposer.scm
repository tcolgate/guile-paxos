(define-module (paxos server proposer)
   #:export (
     start-proposer)
   #:use-module (rnrs bytevectors)
   #:use-module (paxos net mcast))


