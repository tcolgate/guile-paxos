(import
  (paxos misc utils)
  (rnrs)
  (srfi srfi-11))

(test-begin "Utils tests")

(test-equal "majority - 1" (majority 1) 1)
(test-equal "majority - 2" (majority 2) 2)
(test-equal "majority - 3" (majority 3) 2)
(test-equal "majority - 4" (majority 4) 3)
(test-equal "majority - 5" (majority 5) 3)
(test-equal "majority - 6" (majority 6) 4)
(test-equal "majority - 7" (majority 7) 4)
(test-equal "majority - 8" (majority 8) 5)

(define passed
  (if (eq? (test-runner-fail-count (test-runner-current)) 0)
    0
    1))

(test-end "Utils tests")

(exit passed)


