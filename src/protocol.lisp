(in-package :mdns)

(defconstant +proto-version+ 0)
(defconstant +header+ #x62444e53
  "String 'bDNS'")

(defun get-iface-info (iface-name)
  (let ((info (find iface-name (get-ip-interfaces)
                    :key #'ip-interface-name
                    :test #'string=)))
    (if (not info)
        (error 'mdns-error
               :format-control "No such interface: ~s"
               :format-arguments (list iface-name)))
    info))

(defun gethostname ()
  ;; Works on SBCL
  (machine-instance))

(defun format-mdns-message (hostname ip-addr)
  (flexi-streams:with-output-to-sequence (out)
    (write-ub32/be +header+ out)
    (write-byte +proto-version+ out)
    (write-sequence
     (flexi-streams:string-to-octets hostname)
     out)
    (write-byte 0 out)
    (write-sequence ip-addr out)))

(defun parse-mdns-message (octets)
  (flexi-streams:with-input-from-sequence (input octets)
    (if (/= (read-ub32/be input) +header+)
        (error 'mdns-error
               :format-control "Not a mDNS message"))
    (let ((version (read-byte input)))
      (if (/= version +proto-version+)
          (error 'mdns-error
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
           (error 'mdns-error
                  :format-control "Message is too short"))
       ip-addr))))
