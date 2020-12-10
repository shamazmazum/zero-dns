(in-package :zero-dns)

(defparameter *sending-interval* 30
  "Interval between bDNS messages in seconds.")

(defparameter *time-to-live* 120
  "Time before mDNS entry expiration.")

(defparameter *multicast-address* "239.192.20.1:5354"
  "Multicast address and port for Zero DNS messages.")

(defparameter *query-socket* #p"/var/run/zero-dns/zero-dns.sock"
  "Pathname to UNIX domain socket for DNS queries.")

(define-condition zdns-condition ()
  ())

(define-condition zdns-error (zdns-condition simple-error)
  ())
