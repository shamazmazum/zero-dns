(in-package :zero-dns)

(defun ensure-socket-accessible (&key quit-on-error)
  (let ((directory (make-pathname
                    :directory (pathname-directory *query-socket*))))
  (flet ((rant-and-exit (c)
           (princ c *error-output*)
           (terpri *error-output*)
           (format *error-output*
                   "Socket ~a is not accessible, check permissions~%"
                   *query-socket*)
           (if quit-on-error
               (uiop:quit 1)
               (abort))))
    (handler-bind
        ((sb-int::file-error #'rant-and-exit))
      (ensure-directories-exist directory)
      (if (probe-file *query-socket*)
          (delete-file *query-socket*))
      (let ((stream (open *query-socket*
                          :direction :io
                          :if-does-not-exist :create
                          :if-exists :supersede)))
        (close stream))))))

(defun zero-dns (iface &key daemonize)
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
  (when (/= (length sb-ext:*posix-argv*) 2)
    (format *error-output* "Usage: zero-dns <iface>~%")
    (uiop:quit 1))
  (zero-dns (second sb-ext:*posix-argv*) :daemonize t))
