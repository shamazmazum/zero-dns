(in-package :zero-dns)

(defparameter *sending-interval* 30
  "Interval between bDNS messages in seconds")

(defparameter *time-to-live* 120
  "Time before mDNS entry expiration")

(defparameter *multicast-address* "239.192.20.1"
  "Multicast address for Zero DNS messages")

(defparameter *zdns-port* 5354
  "Port for sending and receiving Zero DNS messages")

(defparameter *query-port* 5355
  "Local port for DNS queries")

(define-condition zdns-condition ()
  ())

(define-condition zdns-error (zdns-condition simple-error)
  ())
