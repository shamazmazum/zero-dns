(in-package :mdns)

(defparameter *sending-interval* 30
  "Interval between bDNS messages in seconds")

(defparameter *time-to-live* 120
  "Time before mDNS entry expiration")

(defparameter *mdns-port* 5354
  "Port for sending and receiving mDNS messages")

(defparameter *query-port* 5355
  "Local port for DNS queries")

(define-condition mdns-condition ()
  ())

(define-condition mdns-error (mdns-condition simple-error)
  ())
