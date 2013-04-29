(define-module (paxos net msg-stream)
   #:export (
     make-msg-stream)
   #:use-module (rnrs bytevectors)
   #:use-module (ice-9 format)
   #:use-module (srfi srfi-9)     ; records
   #:use-module (srfi srfi-9 gnu) ; records extensions
   #:use-module (srfi srfi-41)    ; streams
   #:use-module (paxos net datagram-stream))


; This is largely stolen from srfi-41

(define-record-type :msg 
                    (msg type bv off)
                    msg?
                    (type get-msg-type)
                    (bv get-msg-bv)
                    (off get-msg-offset))

;(set-record-type-printer! :msg)

; We could use this to do some crypto verification of
; packats
(define-stream (make-msg-stream  dg-stream)
  "make-msg-stream recver "
  (let ((pkt-classifier  
          (lambda (pkt)
            (msg 
              (case (bytevector-u8-ref pkt 0) 
                ((1) 'paxos)
                ((49) 'aye)
                ((50) 'bee)
                ((51) 'sea)
                (else #f)) 
              pkt
              1))))
    (stream-filter
      (lambda (m) (not (eq? #f (get-msg-type m)))) ; Bin packets we didn't classify
      (stream-map 
        pkt-classifier
        dg-stream))))
