(define-module (paxos misc utils)
   #:export (
     majority))

(define (majority N)
  "Return the number of votes needed for a majority in a group os size N"
  (inexact->exact  (if (odd? N)
   (+ (/ N 2) 0.5)
   (+ (/ N 2) 1))))

