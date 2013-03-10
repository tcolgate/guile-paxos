(use-modules (paxos misc coroutines)
             (srfi srfi-11))


(test-begin "Coroutine tests") 

(let ((generate (lambda (traverse collection)
                  (with-yield yield
                              (traverse yield collection)))))
  (let*-values (((call2 value1)(generate for-each '(three blind mice)))
                ((call3 value2)(call2))
                ((call4 value3)(call3)))
               (test-equal "generate - call1" value1 'three)
               (test-equal "generate - call2" value2 'blind)
               (test-equal "generate - call3" value3 'mice)))

(define passed
  (if (eq? (test-runner-fail-count (test-runner-current)) 0)
    0
    1))

(test-end "Coroutine tests") 

(exit passed)


