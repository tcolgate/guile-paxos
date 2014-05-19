(import
  (paxos net mcast)
  (rnrs)
  (srfi srfi-11))

(test-begin "MCast tests")

(test-equal "No tests yet" #t #t)

(define passed
  (if (eq? (test-runner-fail-count (test-runner-current)) 0)
    0
    1))

(test-end "MCast tests")

(exit passed)


