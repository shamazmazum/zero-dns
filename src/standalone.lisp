(in-package :zero-dns)

;; Startup sanity checks

;; Rules to parse IP address and port
(esrap:defrule number (+ (digit-char-p character))
  (:lambda (list) (parse-integer (esrap:text list))))

(esrap:defrule ip-addr (and number #\. number #\. number #\. number)
  (:lambda (list) (cons :ip-addr
                        (mapcar (lambda (fn) (funcall fn list))
                                (list #'first #'third #'fifth #'seventh)))))

(esrap:defrule port number
  (:lambda (port) (cons :port port)))

(esrap:defrule ip-addr-and-port (and ip-addr #\: port)
  (:lambda (list)
    (mapcar (lambda (fn) (funcall fn list))
            (list #'first #'third))))

(defun ip-addr-and-port-p (string)
  "Check if given string is a combination of IPv4 address and port."
  (handler-case
      (let ((parsed (esrap:parse 'ip-addr-and-port string)))
        ;; Check that the port is unprivileged
        (and (> (cdr (assoc :port parsed)) 1024)
             (every (lambda (x) (< x 256))
                    (cdr (assoc :ip-addr parsed)))))
    (esrap:esrap-parse-error () ())))

;; Command line arguments parsing
(defmacro documentation-with-default (symbol)
  `(format nil "~a Default: ~a"
           (documentation ',symbol 'variable)
           ,symbol))

(opts:define-opts
  (:name        :address
   :description (documentation-with-default *multicast-address*)
   :short       #\a
   :long        "address"
   :arg-parser  #'identity
   :meta-var    "ADDRESS")
  (:name        :sending-interval
   :description (documentation-with-default *sending-interval*)
   :short       #\i
   :long        "sending-interval"
   :arg-parser  #'parse-integer
   :meta-var    "SECONDS")
  (:name        :time-to-live
   :description (documentation-with-default *time-to-live*)
   :short       #\t
   :long        "time-to-live"
   :arg-parser  #'parse-integer
   :meta-var    "SECONDS")
  (:name        :query-socket
   :description (documentation-with-default *query-socket*)
   :short       #\s
   :long        "query-socket"
   :arg-parser  #'identity
   :meta-var    "SOCKET")
  (:name        :daemonize
   :description "Run as a daemon"
   :short       #\d
   :long        "daemonize")
  (:name        :help
   :description "Print this help and exit"
   :short       #\h
   :long        "help"))

(defun describe-and-quit (&optional condition)
  "Print usage and quit."
  (when condition
    (princ condition *error-output*)
    (terpri *error-output*))
  (opts:describe :usage-of "zero-dns" :args "iface")
  (uiop:quit (if condition 1 0)))

(defun parse-arguments-or-signal ()
  "Parse command line arguments or signal ZDNS-ERROR on failure"
  (multiple-value-bind (options arguments)
      (opts:get-opts)
    (let ((sending-interval  (getf options :sending-interval *sending-interval*))
          (time-to-live      (getf options :time-to-live     *time-to-live*))
          (multicast-address (getf options :address          *multicast-address*))
          (query-socket      (getf options :query-socket     *query-socket*))
          (help              (getf options :help))
          (daemonize         (getf options :daemonize)))

      (if help (describe-and-quit))

      (if (/= (length arguments) 1)
          (error 'zdns-error
                 :format-control "You must specify a network interface for ZeroDNS to run on."))

      (if (not (ip-addr-and-port-p multicast-address))
          (error 'zdns-error
                 :format-control "Not valid address and port: ~a"
                 :format-arguments (list multicast-address)))

      (setq *sending-interval*  sending-interval
            *time-to-live*      time-to-live
            *multicast-address* multicast-address
            *query-socket*      query-socket)
      (values
       (first arguments)
       daemonize))))

(defun main ()
  (handler-bind
      (((or zdns-error
            opts:missing-arg
            opts:arg-parser-failed
            opts:unknown-option
            sb-int::file-error)
        #'describe-and-quit))
    (multiple-value-bind (interface daemonize)
        (parse-arguments-or-signal)
      (zero-dns interface :daemonize daemonize)
      (uiop:quit))))
