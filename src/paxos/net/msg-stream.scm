(library
  (paxos net msg-stream)
  (export
    make-msg-stream)
  (import
    (paxos net datagram-stream)
    (rnrs bytevectors)
    (srfi srfi-9)     ; records
    (srfi srfi-9 gnu) ; records extensions
    (srfi srfi-41))   ; streams

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
  (define-stream
    (make-msg-stream  dg-stream)
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
          dg-stream)))))
