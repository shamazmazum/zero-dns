(in-package :mdns)

(defun start-receiver (zmq-context iface)
  (flet ((receiver-fun ()
           (let ((pzmq:*default-context* zmq-context))
             (pzmq:with-sockets ((multicast-socket  :sub)
                                 (control-socket    :sub)
                                 (bookkeeper-socket :pair))
               (pzmq:bind multicast-socket
                          (format nil "norm://~a;239.192.20.1:~d"
                                  iface *mdns-port*))
               (pzmq:connect control-socket "inproc://control")
               (pzmq:connect bookkeeper-socket "inproc://bookkeeper")
               (pzmq:with-poll-items items (control-socket multicast-socket)
                 (loop
                    (pzmq:poll items)
                    ;; Check messages from control
                    (when (member :pollin (pzmq:revents items 0))
                      (let ((msg (pzmq:recv-string control-socket)))
                        (if (string= msg "quit")
                            (return nil))))
                    ;; Received mDNS message
                    (when (member :pollin (pzmq:revents items 1))
                      (let ((msg (pzmq:recv-octets multicast-socket)))
                        (handler-case
                            (multiple-value-bind (hostname ip-addr)
                                (parse-mdns-message msg)
                              (pzmq:send bookkeeper-socket
                                         (flexi-streams:with-output-to-sequence (output)
                                           (cl-store:store
                                            (make-dns-entry :hostname hostname
                                                            :ip-addr ip-addr
                                                            :time (get-universal-time))
                                            output))))
                          (mdns-error ()
                            (format t "Invalid mDNS message~%")))
                        (force-output)))))))))
    (make-thread #'receiver-fun :name "Receiver thread")))
