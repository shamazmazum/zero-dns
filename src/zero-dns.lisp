(in-package :zero-dns)

(defun zero-dns (iface)
  (pzmq:with-context (ctx :max-sockets 32)
    (pzmq:with-socket control-socket :pub
      (pzmq:bind control-socket "inproc://control")
      (let ((threads
             (flet ((start-service (start-function)
                      (funcall start-function pzmq:*default-context* iface)))
               (mapcar #'start-service (list #'start-bookkeeper
                                             #'start-sender
                                             #'start-receiver)))))
        (handler-case
            (loop (sleep 10))
          (sb-sys:interactive-interrupt ()
            (pzmq:send control-socket "quit")
            (map nil #'join-thread threads)))))))
