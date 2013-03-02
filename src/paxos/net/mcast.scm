(define-module (paxos net mcast)
   #:export (
     init-sender
     init-client)
   #:use-module (rnrs bytevectors))

; SOCK_RDM could be interesting
;
(define IP_MULTICAST_LOOP 34)

(define* (init-client)
  (let ((sck   (socket PF_INET SOCK_DGRAM 0)) 
        (addr  (inet-pton AF_INET "224.0.1.1" )) 
        (port  (htons 1234))
        (scope 4)) 
    (setsockopt sck SOL_SOCKET SO_REUSEADDR 1)
    (setsockopt sck IPPROTO_IP IP_ADD_MEMBERSHIP (cons addr INADDR_ANY))
    (setsockopt sck IPPROTO_IP IP_MULTICAST_TTL scope)
    (setsockopt sck IPPROTO_IP IP_MULTICAST_LOOP 1)
    (bind sck AF_INET INADDR_ANY port)
    sck))


(define* (init-sender)
  (let ((sck   (socket PF_INET SOCK_DGRAM 0)) 
        (addr  (inet-pton AF_INET "224.0.1.1" )) 
        (port  (htons 1234))
        (scope 4)) 
    sck))
