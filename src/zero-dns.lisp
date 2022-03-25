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

(defun start-io (iface)
  (list
   (start-sender   iface)
   (start-receiver iface)))

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
      (let ((bookkeeper-thread (start-bookkeeper iface))
            io-threads)
        (labels ((stop ()
                   (pzmq:send control-socket +quit-message+)
                   (map nil #'join-thread
                        (cons bookkeeper-thread io-threads)))
                 (stop-handler (signal info ctx)
                   (declare (ignore signal info ctx))
                   (stop)))
          (when daemonize
            (sb-sys:enable-interrupt sb-posix:sigterm #'stop-handler))
          ;; I handled SB-SYS:INTERACTIVE-INTERRUPT earlier to
          ;; gracefully quit when C-c is pressed in the REPL. This
          ;; does not work in SLIME though, so handle only
          ;; ZDNS-IFACE-DOWN. REPL is used only for debugging anyway.
          (handler-case
              (loop
                 (sleep 1)
                 (let ((iface (check-iface-running iface)))
                   (cond
                     ;; All threads were stopped, exit loop
                     ((notany #'thread-alive-p
                               (cons bookkeeper-thread io-threads))
                      (return))
                     ;; Start I/O threads if in the home network
                     ((or (null *network*)
                          (addr-in-network-p (ip-interface-address iface) *network*))
                      (when (not io-threads)
                        (setq io-threads (start-io (ip-interface-name iface)))))
                     (t
                      ;; We are not in the home network, stop I/O threads
                      (when io-threads
                        (pzmq:send control-socket +stop-io-message+)
                        (setq io-threads nil))))))
            (zdns-iface-down ()
              (stop))))))))
