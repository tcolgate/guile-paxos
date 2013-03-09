(use-modules (paxos misc statemachine)
             (ice-9 streams))


(test-begin "Statemachine tests") 

(test-equal "simple pass" 
            #t
            ((automaton init stream-car stream-cdr
                        (init  : (1 -> more))
                        (more  : (2 -> more)
                               (3 -> more)
                               (4 -> end)
                               -> other)
                        (other : (5 -> fail)
                               (6 -> end)
                               (7 -> init))
                        (fail  : abort)
                        (end   : accept))
            (list->stream (list 1 2 3 3 4))))

(test-equal "simple fail" 
            #f
            ((automaton init stream-car stream-cdr
                        (init  : (1 -> more))
                        (more  : (2 -> more)
                               (3 -> more)
                               (4 -> end)
                               -> other)
                        (other : (5 -> fail)
                               (6 -> end)
                               (7 -> init))
                        (fail  : abort)
                        (end   : accept))
            (list->stream (list 1 2 3 9 5))))

(define passed
  (if (test-passed? (test-runner-current)) 
    0 
    1)) 

(test-end "Statemachine tests") 

(exit passed)


