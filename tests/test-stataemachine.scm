(use-modules (paxos misc statemachine)
             (ice-9 streams))


(test-begin "Statemachine tests")

; simple - matches a 1 [234] 5
(let ((simple (automaton init stream-car stream-cdr stream-null? equal?
                         (init : (1 -> more)
                               -> init)
                         (more : (2 -> more)
                               (3 -> more)
                               (4 -> more)
                               (5 -> end)
                               -> init)
                         (end   : accept))))
  (test-equal "simple pass"
              #t
              (simple (list->stream (list 1 2 3 4 2 5))))
  (test-equal "simple pass - late start"
              #t
              (simple (list->stream (list 9 9 9 1 2 3 4 2 5))))
  (test-equal "simple pass - restart"
              #f
              (simple (list->stream (list 1 2 3 9 9 1 2 3 4))))
  (test-equal "simple fail - short"
              #f
              (simple (list->stream (list 1 2 3 3 4)))))

(test-equal "simple abort fail"
            #f
            ((automaton init stream-car stream-cdr stream-null? equal?
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

(test-equal "simple alias test"
            #t
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init       : (1 -> more))
                        (more       : (2 -> more)
                                      (3 -> more/end)
                                      (4 -> end)
                                      -> other)
                        (more/end   : alias more)
                        (fail       : abort)
                        (end        : accept))
            (list->stream (list 1 2 3 3 4))))

(test-equal "simple ladder test - empty"
            #t
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init           : (1 -> l/head)
                                          -> init)
                        (l 0 init end   : (2 -> l/next)
                                          (3 -> l/prev)
                                          (4 -> end))
                        (fail           : abort)
                        (end            : accept))
            (list->stream (list 0 0 1 2 2 2 2 2 2))))

(test-equal "simple ladder test - 4 rung -  (1)"
            #t
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init           : (1 -> l/head)
                                          -> init)
                        (l 3 init end   : (2 -> l/next)
                                          (3 -> l/prev)
                                          (4 -> end))
                        (fail           : abort)
                        (end            : accept))
            (list->stream (list 0 0 1 4))))

(test-equal "simple ladder test - 4 rung - run out (2)"
            #t
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init           : (1 -> l/head)
                                          -> init)
                        (l 3 init end   : (2 -> l/next)
                                          (3 -> l/prev)
                                          (4 -> end))
                        (fail           : abort)
                        (end            : accept))
            (list->stream (list 0 0 1 2 2 2 2 4))))

(test-equal "simple ladder test - 4 rung - early quite (3)"
            #t
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init           : (1 -> l/head)
                                          -> init)
                        (l 3 init end   : (2 -> l/next)
                                          (3 -> l/prev)
                                          (4 -> end))
                        (fail           : abort)
                        (end            : accept))
            (list->stream (list 0 0 4 1 2 4))))

(let* ((v #f)
       (f (lambda (p n s) (format #t "INSIDE: ~a ~a ~s ~%" p n s) s))
       (hooks (automaton init stream-car stream-cdr stream-null? equal?
                         (init : 
                               (1 -> more)
                               -> init)
                         (more : 
                               (2 -> more)
                               (3 -> more)
                               (4 -> more)
                               (5 -> end)
                               -> init)
                         (end  : accept))))
  (test-equal "hook test"
              #t
              (begin
                (hooks (list->stream (list 1 2 3 4 2 5)))
                v)))

(define passed
  (if (eq? (test-runner-fail-count (test-runner-current)) 0)
    0
    1))

(test-end "Statemachine tests")

(exit passed)


