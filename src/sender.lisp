(in-package :zero-dns)

(defun start-sender (iface)
  (let ((ip-addr (ip-interface-address
                  (get-iface-info iface)))
        (hostname (gethostname)))
    (flet ((sender-fun ()
             (let ((time (get-universal-time)))
               (pzmq:with-sockets ((multicast-socket :pub)
                                   (control-socket   :sub))
                 (pzmq:connect multicast-socket
                               (format nil "norm://~a;~a"
                                       iface *multicast-address*))
                 (pzmq:connect control-socket "inproc://control")
                 (loop
                    ;; Check messages from control
                    (pzmq:with-poll-items items ((control-socket :pollin))
                      (pzmq:poll items 500)
                      (when (member :pollin (pzmq:revents items 0))
                        (let ((msg (pzmq:recv-string control-socket)))
                          (if (string= msg "quit")
                              (return nil)))))
                    ;; Send multicast message which contains our
                    ;; hostname and IP address if needed.
                    (when (> (get-universal-time)
                             (+ time *sending-interval*))
                      (setq time (get-universal-time))
                      (pzmq:send multicast-socket
                                 (format-zdns-message hostname ip-addr))))))))
      (make-thread #'sender-fun
                   :name "Sender thread"
                   :initial-bindings (acons 'pzmq:*default-context*
                                            pzmq:*default-context*
                                            *default-special-bindings*)))))
