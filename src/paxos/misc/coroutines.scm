(define-module (paxos misc coroutines)
   #:export (
     call-with-yield)
   #:export-syntax (
     with-yield)
   #:use-module (ice-9 format))

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
      (lambda (yield) exp exp* ...)))))

; (define (generate traverse collection)
;  (with-yield yield
;    (traverse yield collection)))
; 
;> (generate for-each '(three blind mice))
;$1 = #<procedure resume args>
;$2 = three
;> ($1)
;$3 = #<procedure resume args>
;$4 = blind
;> ($3)
;$5 = #<procedure resume args>
;$6 = mice
;> ($1)
;$7 = #<procedure resume args>
;$8 = blind
;> ($5)

