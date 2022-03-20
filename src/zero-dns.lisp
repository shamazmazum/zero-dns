(in-package :zero-dns)

(defun check-socket-directory ()
  "Check that the socket directory is assessible for writing"
  (let ((tmpname (merge-pathnames
                  #p"tmp"
                  (truename *socket-directory*))))
    (let ((stream (open tmpname
                        :direction :io
                        :if-does-not-exist :create
                        :if-exists :supersede)))
      (close stream))
    (delete-file tmpname)))

(defun zero-dns (iface &key daemonize)
  "Start ZeroDNS service.on interface IFACE. If DAEMONIZE is T
daemonize the process."
  (check-socket-directory)
  (check-iface-running iface)
  (when daemonize
    (daemonize))
  (pzmq:with-context (ctx :max-sockets 32)
    (pzmq:with-socket control-socket :pub
      (pzmq:bind control-socket "inproc://control")
      (let ((threads
             (mapcar (alex:rcurry #'funcall iface)
                     (list #'start-bookkeeper
                           #'start-sender
                           #'start-receiver))))
        (labels ((stop ()
                   (pzmq:send control-socket +quit-message+)
                   (map nil #'join-thread threads))
                 (stop-handler (signal info ctx)
                   (declare (ignore signal info ctx))
                   (stop)))
          (when daemonize
            (sb-sys:enable-interrupt sb-posix:sigterm #'stop-handler)
            (sb-sys:enable-interrupt sb-posix:sigint  #'stop-handler))
          ;; SB-SYS:INTERACTIVE-INTERRUPT can be signalled only when
          ;; we are not daemonized. Just call the same function,
          ;; STOP, to do cleanup.
          (handler-case
              (loop
                 (when (notany #'thread-alive-p threads)
                   (return))
                 (sleep 1)
                 (check-iface-running iface))
            ((or sb-sys:interactive-interrupt zdns-iface-down) ()
              (stop))))))))
