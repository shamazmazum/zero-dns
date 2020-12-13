(in-package :zero-dns)

(defun ensure-socket-accessible ()
  ;; KLUDGE: try to create regular file with name *QUERY-SOCKET*
  ;; Created regular file will be overwritten by ZeroMQ.
  (let ((directory (make-pathname
                    :directory (pathname-directory *query-socket*))))
    (ensure-directories-exist directory)
    (if (probe-file *query-socket*)
        (delete-file *query-socket*))
    (let ((stream (open *query-socket*
                        :direction :io
                        :if-does-not-exist :create
                        :if-exists :supersede)))
        (close stream))))

(defun zero-dns (iface &key daemonize)
  "Start ZeroDNS service.on interface IFACE. If DAEMONIZE is T
daemonize the process."
  (ensure-socket-accessible)
  (when daemonize
    (daemonize))
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
                 (map nil #'join-thread threads)))
          (when daemonize
            (sb-sys:enable-interrupt sb-posix:sigterm #'stop-fn)
            (sb-sys:enable-interrupt sb-posix:sigint  #'stop-fn))
          ;; SB-SYS:INTERACTIVE-INTERRUPT can be signalled only when
          ;; we are not daemonized. Just call the same function,
          ;; STOP-FN, to do cleanup.
          (handler-case
              (loop
                 while (some #'thread-alive-p threads)
                 do (sleep 1))
            (sb-sys:interactive-interrupt ()
              (stop-fn nil nil nil))))))))
