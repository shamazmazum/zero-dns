(in-package :zero-dns)

(defconstant +proto-version+ 0)
(defconstant +header+ #x62444e53
  "String 'bDNS'")

(defun get-running-iface (iface-name)
  "Return interface if it exists and is running, otherwise return NIL."
  (let ((iface (find iface-name (get-ip-interfaces)
                     :key #'ip-interface-name
                     :test #'string=)))
    (when (and iface
               ;; RUNNING flag is 1 on most platforms
               (oddp (ip-interface-flags iface)))
      iface)))

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
        (error 'zdns-error
               :format-control "Not a zero-dns message"))
    (let ((version (read-byte input)))
      (if (/= version +proto-version+)
          (error 'zdns-error
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
           (error 'zdns-error
                  :format-control "Message is too short"))
       ip-addr))))
