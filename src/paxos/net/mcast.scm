(define-module (paxos net mcast)
   #:export (
     make-mcast-reciever make-mcast-sender)
   #:use-module (rnrs bytevectors)
   #:use-module (ice-9 format))

; SOCK_RDM could be interesting
;
(define IP_MULTICAST_LOOP 34)

(define* (make-mcast-reciever #:key (port 1234) (group "224.0.1.1") (blocking #f))
  (let ((sck       (socket PF_INET SOCK_DGRAM 0)) 
        (addr      (inet-pton AF_INET group )) 
        (nport     (htons port))
        (recvflags (if blocking 0 (logior MSG_DONTWAIT MSG_PEEK))))
    (setsockopt sck SOL_SOCKET SO_REUSEADDR 1)
    (setsockopt sck IPPROTO_IP IP_ADD_MEMBERSHIP (cons addr INADDR_ANY))
    (setsockopt sck IPPROTO_IP IP_MULTICAST_LOOP 1)
    (if (not blocking)
      (fcntl sck F_SETFL (logior 
                            (fcntl sck F_GETFL) 
                            O_NONBLOCK))) 
    (bind sck AF_INET INADDR_ANY port)
    (lambda*(#:key (maxsize 64))
      (let ((buffer (make-bytevector maxsize 0)))
        (format #t "flags: ~d ~%" recvflags)
        (recvfrom! sck buffer recvflags)
        buffer))))

(define* (make-mcast-sender #:key (port 1234) (group "224.0.1.1") (scope 4) (blocking #f))
  (let ((sck   (socket PF_INET SOCK_DGRAM 0)) 
        (addr  (inet-pton AF_INET group )) 
        (nport (htons port))
        (scope 4)) 
    (setsockopt sck IPPROTO_IP IP_MULTICAST_TTL scope)
    (if (not blocking)
      (fcntl sck F_SETFL (logior 
                            (fcntl sck F_GETFL) 
                            O_NONBLOCK)))(lambda (message)
      (if (bytevector? message)
        (let loop ((len  (bytevector-length message))
                   (gone 0))
          (let ((sent (sendto sck message AF_INET addr port)))
            (if (> len (+ gone sent))
              #f ; should loop round
              #t)))
        #f))))
