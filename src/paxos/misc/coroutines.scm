#!r6rs

(library
  (paxos misc coroutines)
  (export
    call-with-yield
    with-yield)
  (import
    (rnrs)
    (ice-9 control))

  (define (call-with-yield proc)
    (let ((tag (make-prompt-tag)))
      (define (handler k . args)
        (define (resume . args)
          (call-with-prompt tag
                            (lambda () (apply k args))
                            handler))
        (apply values resume args))

      (call-with-prompt
        tag
        (lambda ()
          (let ((yield (lambda args
                         (apply abort-to-prompt tag args))))
            (proc yield)))
        handler)))

  (define-syntax with-yield
    (syntax-rules ()
                  ((_ yield exp exp* ...)
                   (call-with-yield
                     (lambda (yield) exp exp* ...))))))

