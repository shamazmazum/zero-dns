(in-package :mdns)

(defun format-ip-address (address)
  (declare (type (vector (unsigned-byte 8))))
  (format nil "~{~d~^.~}"
          (coerce address 'list)))

(defstruct dns-entry
  (hostname "" :type string)
  (ip-addr #() :type vector)
  (time 0 :type (integer 0)))

(defclass bookkeeper ()
  ((entries  :initform nil
             :type list
             :accessor bookkeeper-entries)
   (lifetime :initform *time-to-live*
             :type (integer 0)
             :reader bookkeeper-entry-lifetime)))

(defgeneric add-entry (bookkeeper entry)
  (:documentation "Add DNS entry to the bookkeeper"))

(defgeneric prune-entries (bookkeeper)
  (:documentation "Remove old entries from the bookkeeper"))

(defgeneric dns-lookup (bookkeeper hostname)
  (:documentation "Get IP address of the host with name hostname"))

(defgeneric reverse-dns-lookup (bookkeeper addr)
  (:documentation "Get hostname of the host with IP address addr"))



(defmethod add-entry ((bookkeeper bookkeeper) dns-entry)
  (with-accessors ((entries bookkeeper-entries))
      bookkeeper
    (setf entries (delete (dns-entry-hostname dns-entry)
                          entries
                          :test #'string=
                          :key  #'dns-entry-hostname))
    (push dns-entry entries)))

(defmethod prune-entries ((bookkeeper bookkeeper))
  (setf (bookkeeper-entries bookkeeper)
        (remove-if
         (lambda (entry)
           (> (get-universal-time)
              (+ (bookkeeper-entry-lifetime bookkeeper)
                 (dns-entry-time entry))))
         (bookkeeper-entries bookkeeper))))

(defmethod dns-lookup ((bookkeeper bookkeeper) hostname)
  (declare (type string hostname))
  (let ((entry (find hostname (bookkeeper-entries bookkeeper)
                     :test #'string=
                     :key  #'dns-entry-hostname)))
    (if entry (dns-entry-ip-addr entry))))

(defmethod reverse-dns-lookup ((bookkeeper bookkeeper) addr)
  (declare (type string addr))
  (let ((entry (find addr (bookkeeper-entries bookkeeper)
                     :test #'equalp
                     :key  (alexandria:compose #'format-ip-address
                                               #'dns-entry-ip-addr))))
    (if entry (dns-entry-hostname entry))))
