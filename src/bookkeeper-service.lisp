(in-package :mdns)

(defun answer-with-error (socket)
  (pzmq:send socket "ERROR"))

(defun format-ip-address (address)
  (declare (type (or null (vector (unsigned-byte 8)))))
  (format nil "~@[~{~d~^.~}~]"
          (coerce address 'list)))

(defconstant +forward-opcode+ 0
  "Do forward name resolution")

(defconstant +reverse-opcode+ 1
  "Do reverse name resolution")

(defun answer-query (bookkeeper query-socket)
  (let ((query (pzmq:recv-octets query-socket)))
    (when (< (length query) 1)
      (return-from answer-query
        (answer-with-error query-socket)))
    (let ((opcode (aref query 0)))
      (cond
      ((= opcode +forward-opcode+)
       (let ((ip-addr
              (dns-lookup bookkeeper
                          (flexi-streams:octets-to-string query :start 1))))
         (pzmq:send query-socket ip-addr)))
      ((= opcode +reverse-opcode+)
       (let ((hostname (reverse-dns-lookup bookkeeper (subseq query 1))))
         (pzmq:send query-socket hostname)))
      (t (answer-with-error query-socket))))))

(defun start-bookkeeper (zmq-context iface)
  (declare (ignore iface))
  (flet ((bookkeeper-fun ()
           (let ((pzmq:*default-context* zmq-context)
                 (bookkeeper (make-instance 'bookkeeper)))
             (pzmq:with-sockets ((control-socket    :sub)
                                 (bookkeeper-socket :pair)
                                 (query-socket      :rep))
               (pzmq:connect control-socket "inproc://control")
               (pzmq:bind bookkeeper-socket "inproc://bookkeeper")
               (pzmq:bind query-socket (format nil "tcp://*:~d"
                                               *query-port*))
               (pzmq:with-poll-items items (control-socket
                                            bookkeeper-socket
                                            query-socket)
                 (loop
                    (pzmq:poll items 1000)
                    ;; Check messages from control
                    (when (member :pollin (pzmq:revents items 0))
                      (let ((msg (pzmq:recv-string control-socket)))
                        (if (string= msg "quit")
                            (return nil))))
                    ;; New mDNS entry
                    (when (member :pollin (pzmq:revents items 1))
                      (flexi-streams:with-input-from-sequence
                          (input (pzmq:recv-octets bookkeeper-socket))
                        (let ((dns-entry (cl-store:restore input)))
                          (add-entry bookkeeper dns-entry))))
                    ;; Answer query
                    (when (member :pollin (pzmq:revents items 2))
                      (answer-query bookkeeper query-socket))
                    ;; Prune old entries
                    (prune-entries bookkeeper)))))))
    (prog1
        (make-thread #'bookkeeper-fun :name "Bookkeeper thread")
      ;; Wait for bookkeeper thread to start
      (sleep 1))))
