(use-modules (paxos net mcast)
             (srfi srfi-11))


(test-begin "MCast tests") 

(test-equal "No tests yet" #t #t)

(define passed
  (if (test-passed? (test-runner-current)) 
    0 
    1)) 

(test-end "MCast tests") 

(exit passed)


