(in-package :zero-dns)

(defconstant +proto-version+ 0)
(defconstant +header+ #x62444e53
  "String 'bDNS'")

(alex:define-constant +quit-message+ "quit"
  :test #'equal
  :documentation "ZMQ quit message for worker threads.")

(alex:define-constant +stop-io-message+ "stop-io"
  :test #'equal
  :documentation "ZMQ quit message for I/O threads.")

(defun find-iface (iface-name)
  "Find an interface with name IFACE-NAME"
  (find iface-name (get-ip-interfaces)
        :key  #'ip-interface-name
        :test #'string=))

(defun addr-in-network-p (ip-addr network)
  (multiple-value-bind (network-addr mask)
      (sera:deconstruct network)
    (every #'=
           (map 'vector #'logand ip-addr mask)
           (map 'vector #'logand network-addr mask))))

(defun check-iface-running (iface-name)
  "Return interface if it exists and is running, signal an error"
  (let ((iface (find-iface iface-name)))
    (when (or (not iface)
              (not (ip-interface-address iface))
              ;; RUNNING flag is 1 on most platforms
              (evenp (ip-interface-flags iface)))
      (error 'zdns-iface-down
             :iface iface-name))
    iface))

(defun gethostname ()
  ;; Works on SBCL
  (machine-instance))

(defun format-zdns-message (hostname ip-addr)
  (flexi-streams:with-output-to-sequence (out)
    (write-ub32/be +header+ out)
    (write-byte +proto-version+ out)
    (write-sequence
     (flexi-streams:string-to-octets hostname)
     out)
    (write-byte 0 out)
    (write-sequence ip-addr out)))

(defun parse-zdns-message (octets)
  (flexi-streams:with-input-from-sequence (input octets)
    (if (/= (read-ub32/be input) +header+)
        (error 'zdns-simple-error
               :format-control "Not a zero-dns message"))
    (let ((version (read-byte input)))
      (if (/= version +proto-version+)
          (error 'zdns-simple-error
                 :format-control "Unknown proto version: ~d"
                 :format-arguments (list version))))
    (values
     (flexi-streams:octets-to-string
      (loop
         for byte = (read-byte input)
         until (zerop byte)
         collect byte))
     (let ((ip-addr (make-array 4 :element-type '(unsigned-byte 8))))
       (if (/= (read-sequence ip-addr input) 4)
           (error 'zdns-simple-error
                  :format-control "Message is too short"))
       ip-addr))))
