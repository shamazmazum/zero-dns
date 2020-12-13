(in-package :zero-dns)

(defun ensure-socket-accessible (&key quit-on-error)
  ;; KLUDGE: try to create regular file with name *QUERY-SOCKET*
  ;; If we are not successful print a message and optionally quit.
  ;; Created regular file will be overwritten by ZeroMQ.
  (let ((directory (make-pathname
                    :directory (pathname-directory *query-socket*))))
  (flet ((print-error-and-maybe-quit (c)
           (princ c *error-output*)
           (terpri *error-output*)
           (format *error-output*
                   "Socket ~a is not accessible, check permissions~%"
                   *query-socket*)
           (if quit-on-error
               (uiop:quit 1)
               (abort))))
    (handler-bind
        ((sb-int::file-error #'print-error-and-maybe-quit))
      (ensure-directories-exist directory)
      (if (probe-file *query-socket*)
          (delete-file *query-socket*))
      (let ((stream (open *query-socket*
                          :direction :io
                          :if-does-not-exist :create
                          :if-exists :supersede)))
        (close stream))))))

(defun zero-dns (iface &key daemonize)
  "Start ZeroDNS service.on interface IFACE. If DAEMONIZE is T run
service as a daemon."
  (ensure-socket-accessible :quit-on-error daemonize)
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
                 (map nil #'join-thread threads)
                 (if daemonize (uiop:quit))))
          (when daemonize
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
  ;; Toplevel entry point
  (when (/= (length sb-ext:*posix-argv*) 2)
    (format *error-output* "Usage: zero-dns <iface>~%")
    (uiop:quit 1))
  (zero-dns (second sb-ext:*posix-argv*) :daemonize t))
