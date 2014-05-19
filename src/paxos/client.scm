(library
  (paxos client)
  (export
    client-loop)
  (import
    (paxos misc coroutines)
    (paxos net mcast)
    (paxos net datagram-stream)
    (paxos misc statemachine)
    (rnrs)
    (rnrs bytevectors)
    (ice-9 optargs))

  (define*
    (client-loop)
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

      (let* ((sck (make-mcast-reciever #:blocking #t))
             (read (lambda ()
                     (format #t "try and read ~%")
                     (let ((d (sck)))
                       (format #t "GOT ~a~%" d)
                       d))))
        (let loop ((call (simple (read))))
          (format #t "LOOP: ~a~%" call)
          (cond
            ((precedure? call) (loop (call (read))))
            ((wq? call #t) (format #t "PASSED~%"))
            ((ew? call #f) (format #t "FAIL~%"))
            (else (format #t "UNKNOWN: ~a~%" call))))))))
