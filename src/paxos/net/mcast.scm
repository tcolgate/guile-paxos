(define-module (paxos net mcast)
   #:export (
     make-mcast-reciever make-mcast-sender)
   #:use-module (rnrs bytevectors)
   #:use-module (ice-9 format))

; SOCK_RDM could be interesting, for the moment we just use multicast

(define IP_MULTICAST_LOOP 34)

(define* (make-mcast-reciever #:key (port 1234) (group "224.0.1.1") (blocking #f))
  (let ((sck       (socket PF_INET SOCK_DGRAM 0)) 
        (addr      (inet-pton AF_INET group )) 
        (nport     (htons port)))
    (setsockopt sck SOL_SOCKET SO_REUSEADDR 1)
    (setsockopt sck IPPROTO_IP IP_ADD_MEMBERSHIP (cons addr INADDR_ANY))
    (setsockopt sck IPPROTO_IP IP_MULTICAST_LOOP 1)
    (bind sck AF_INET INADDR_ANY port)
    (lambda*(#:key (maxsize 64))
      (let ((buffer (make-bytevector maxsize 0)))
        ; This is a bit ugly, but guile reports EAGAIN and EWOULDBLOCK 
        ; as errors
        (if (not blocking)
          (let ((waiting (select (list sck) '() '() 0))) 
            (if (not (eq? (car waiting) '())) 
              (recv! sck buffer)
              #f))
          (recv! sck buffer)) 
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
