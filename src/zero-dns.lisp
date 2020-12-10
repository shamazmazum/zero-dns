(in-package :zero-dns)

(defun zero-dns (iface &optional daemonized)
  (pzmq:with-context (ctx :max-sockets 32)
    (pzmq:with-socket control-socket :pub
      (pzmq:bind control-socket "inproc://control")
      (let ((threads
             (flet ((start-service (start-function)
                      (funcall start-function pzmq:*default-context* iface)))
               (mapcar #'start-service (list #'start-bookkeeper
                                             #'start-sender
                                             #'start-receiver)))))
        (flet ((stop-fn (signal info ctx)
                 (declare (ignore signal info ctx))
                 (pzmq:send control-socket "quit")
                 (map nil #'join-thread threads)
                 (if daemonized (uiop:quit))))
          (when daemonized
            (sb-sys:enable-interrupt sb-posix:sigterm #'stop-fn)
            (sb-sys:enable-interrupt sb-posix:sigint  #'stop-fn))
          ;; SB-SYS:INTERACTIVE-INTERRUPT can be signalled only when
          ;; we are not daemonized. Just call the same function,
          ;; STOP-FN, to do cleanup.
          (handler-case
              (loop (sleep 10))
            (sb-sys:interactive-interrupt ()
              (stop-fn nil nil nil))))))))

(defun main ()
  (when (/= (length sb-ext:*posix-argv*) 2)
    (format t "Usage: zero-dns <iface>~%")
    (uiop:quit 1))
  (daemonize)
  (zero-dns (second sb-ext:*posix-argv*) t))
