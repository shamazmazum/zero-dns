(in-package :zero-dns)

(defparameter *sending-interval* 30
  "Interval between two ZeroDNS messages in seconds.")

(defparameter *time-to-live* 120
  "Time before ZeroDNS entry expiration in seconds.")

(defparameter *multicast-address* "239.192.20.1:5354"
  "Multicast address and port for Zero DNS messages.")

(defparameter *socket-directory* #p"/var/run/zero-dns/"
  "Directory where ZeroDNS will create UNIX domain sockets for DNS
queries.")

(define-condition zdns-condition ()
  ())

(define-condition zdns-error (error zdns-condition)
  ()
  (:documentation "General ZeroDNS error"))

(define-condition zdns-simple-error (zdns-error simple-error)
  ())

(define-condition zdns-iface-down (zdns-error)
  ((iface :reader  iface-down-iface
          :initarg :iface))
  (:documentation "Interface is down or non-existent")
  (:report (lambda (c s)
             (format s "Interface ~a is down"
                     (iface-down-iface c)))))
