(import 
  (paxos misc statemachine)
  (paxos misc coroutines)
  (srfi srfi-11)
  (srfi srfi-41))

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
  (test-equal "simple pass with excess"
              #t
              (simple (list->stream (list 1 5 7 7 7 7))))
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

(test-equal "simple abort on invalid"
            #f
            ((automaton init stream-car stream-cdr stream-null? equal?
                        (init  : (1 -> more))
                        (more  : (2 -> more)
                               (3 -> more)
                               (4 -> end))
                        (end   : accept))
            (list->stream (list 1 2 9 5))))

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

;(test-equal "simple ladder test - empty"
;            #t
;            ((automaton init stream-car stream-cdr stream-null? equal?
;                        (init           : (1 -> l/head)
;                                          -> init)
;                        (l 0 init end   : (2 -> l/next)
;                                          (3 -> l/prev)
;                                          (4 -> end))
;                        (fail           : abort)
;                        (end            : accept))
;            (list->stream (list 0 0 1 2 2 2 2 2 2))))
;
;(test-equal "simple ladder test - 4 rung -  (1)"
;            #t
;            ((automaton init stream-car stream-cdr stream-null? equal?
;                        (init           : (1 -> l/head)
;                                          -> init)
;                        (l 3 init end   : (2 -> l/next)
;                                          (3 -> l/prev)
;                                          (4 -> end))
;                        (fail           : abort)
;                        (end            : accept))
;            (list->stream (list 0 0 1 4))))
;
;(test-equal "simple ladder test - 4 rung - run out (2)"
;            #t
;            ((automaton init stream-car stream-cdr stream-null? equal?
;                        (init           : (1 -> l/head)
;                                          -> init)
;                        (l 3 init end   : (2 -> l/next)
;                                          (3 -> l/prev)
;                                          (4 -> end))
;                        (fail           : abort)
;                        (end            : accept))
;            (list->stream (list 0 0 1 2 2 2 2 4))))
;
;(test-equal "simple ladder test - 4 rung - early quite (3)"
;            #t
;            ((automaton init stream-car stream-cdr stream-null? equal?
;                        (init           : (1 -> l/head)
;                                          -> init)
;                        (l 3 init end   : (2 -> l/next)
;                                          (3 -> l/prev)
;                                          (4 -> end))
;                        (fail           : abort)
;                        (end            : accept))
;            (list->stream (list 0 0 4 1 2 4))))

(let* ((v1 #f)
       (v2 #f)
       (f (lambda (p n s) (format #t "LOCAL: ~a ~a ~a~%" p n s)(set! v1 #t) s))
       (g (lambda (p n s) (format #t "GLOBAL: ~a ~a ~a~%" p n s) (set! v2 #t) s))
       (hooks (automaton init stream-car stream-cdr stream-null? equal?
                         (init :
                               (1 -> more f)
                               -> init)
                         (more :
                               (2 -> more f)
                               (3 -> more )
                               (4 -> more f)
                               (5 -> end)
                               -> init)
                         (end  : accept)
                         :hooks (g))))
  (test-equal "hook test"
              #t
              (begin
                (hooks (list->stream (list 1 2 3 4 2 5)))
                (and v1 v2))))

(let ((simple (lambda (start)
                (with-yield
                  yield
                  ((automaton
                    init
                    (lambda (v) (format #t "CURR: ~a ~%" v) v)
                    (lambda (v)
                      (format #t "NEXT: ~a yielding~%" v)
                      (let ((n (yield)))
                        (format #t "back with ~a~%" n)
                        n))
                    (lambda (v)  (not v))
                    equal?
                    (init : (1 -> more)
                          -> init)
                    (more : (2 -> more)
                          (3 -> more)
                          (4 -> more)
                          (5 -> end)
                          -> init)
                    (end   : accept)
                    :hooks ((lambda (p n s) (format #t "GLOBAL: ~a ~a ~a~%" p n s) s))) start)))))

  (test-equal "co-routine test"
              #t
              (let* ((call2 (simple 1))
                     (call3 (call2  5))
                     (call4 (call3  #f))) ; this call is a bit redundant as we've passed already
                (format #t "results: ~a~%" (list call2 call3 call4))
                call4)))


(define passed
  (if (eq? (test-runner-fail-count (test-runner-current)) 0)
    0
    1))

(test-end "Statemachine tests")

(exit passed)

