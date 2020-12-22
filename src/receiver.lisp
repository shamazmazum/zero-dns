(in-package :zero-dns)

(defun start-receiver (iface)
  (flet ((receiver-fun ()
           (pzmq:with-sockets ((multicast-socket  :sub)
                               (control-socket    :sub)
                               (bookkeeper-socket :pair))
             (pzmq:bind multicast-socket
                        (format nil "norm://~a;~a"
                                iface *multicast-address*))
             (pzmq:connect control-socket "inproc://control")
             (pzmq:connect bookkeeper-socket "inproc://bookkeeper")
             (pzmq:with-poll-items items ((control-socket   :pollin)
                                          (multicast-socket :pollin))
               (loop
                  (pzmq:poll items)
                  ;; Check messages from control
                  (when (member :pollin (pzmq:revents items 0))
                    (let ((msg (pzmq:recv-string control-socket)))
                      (if (string= msg "quit")
                          (return nil))))
                  ;; Received Zero DNS message
                  (when (member :pollin (pzmq:revents items 1))
                    (let ((msg (pzmq:recv-octets multicast-socket)))
                      (handler-case
                          (multiple-value-bind (hostname ip-addr)
                              (parse-zdns-message msg)
                            (pzmq:send bookkeeper-socket
                                       (flexi-streams:with-output-to-sequence (output)
                                         (cl-store:store
                                          (make-dns-entry :hostname hostname
                                                          :ip-addr ip-addr
                                                          :time (get-universal-time))
                                          output))))
                        ((or zdns-error end-of-file) ()
                          (format t "Invalid Zero DNS message~%")))
                      (force-output))))))))
    (make-thread #'receiver-fun
                 :name "Receiver thread"
                 :initial-bindings (acons 'pzmq:*default-context*
                                          pzmq:*default-context*
                                          *default-special-bindings*))))
