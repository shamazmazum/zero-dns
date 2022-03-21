(in-package :zero-dns-app)

(define-condition no-interface (error) ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "You must specify a network interface for ZeroDNS to work with.")))
  (:documentation "Signalled when wrong number of mandatory arguments
is given"))

;; Startup sanity checks

;; Rules to parse IP address and port
(esrap:defrule number (+ (digit-char-p character))
  (:lambda (list) (parse-integer (esrap:text list))))

(esrap:defrule ip-addr (and number #\. number #\. number #\. number)
  (:lambda (list)
    (let ((ip-addr (mapcar (alex:rcurry #'funcall list)
                           (list #'first #'third #'fifth #'seventh))))
      (when (notevery (alex:rcurry #'< 256) ip-addr)
        (error "Wrong IP address: ~a" ip-addr))
      ip-addr)))

(esrap:defrule port number
  (:lambda (port)
    ;; Check that the port is unprivileged
    (when (< port 1024)
      (error "Please specify an unprivileged port: ~d" port))
    port))

(esrap:defrule mask number
  (:lambda (mask)
    ;; Check that MASK is really a network mask
    (when (> mask 32)
      (error "Please specify a correct network mask: ~d" mask))
    mask))

(esrap:defrule ip-addr-and-port (and ip-addr #\: port)
  (:lambda (list)
    (cons (first list)
          (third list))))

(esrap:defrule network (or (and ip-addr #\/ mask) "any")
  (:lambda (list)
    (when (listp list)
      (cons (first list)
            (third list)))))

(defun format-address (address)
  (format nil "~{~d~^.~}:~d"
          (car address)
          (cdr address)))

(defun args->network (parsed-args)
  (when parsed-args
    (destructuring-bind (ip-addr . mask) parsed-args
      (let ((mask (logand
                   (1- (ash 1 32))
                   (lognot (1- (ash 1 (- 32 mask)))))))
        (network (apply #'vector ip-addr)
                 (apply #'vector
                        (mapcar
                         (lambda (pos)
                           (ldb (byte 8 pos) mask))
                         '(24 16 8 0))))))))

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
   :arg-parser  (alex:compose #'format-address
                              (alex:curry #'esrap:parse 'ip-addr-and-port))
   :meta-var    "ADDRESS")
  (:name        :network
   :description (documentation-with-default *network*)
   :short       #\n
   :long        "network"
   :arg-parser  (alex:compose #'args->network
                              (alex:curry #'esrap:parse 'network))
   :meta-var    "NETWORK")
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
  (:name        :socket-directory
   :description (documentation-with-default *socket-directory*)
   :short       #\q
   :long        "socket-directory"
   :arg-parser  #'identity
   :meta-var    "DIRECTORY")
  (:name        :daemonize
   :description "Run as a daemon"
   :short       #\d
   :long        "daemonize")
  (:name        :help
   :description "Print this help and exit"
   :short       #\h
   :long        "help"))

(defun describe-and-quit (&key condition force-describe)
  "Print usage and quit."
  (when condition
    (princ condition *error-output*)
    (terpri *error-output*))
  (when (or (typep condition 'opts:troublesome-option)
            force-describe)
    (opts:describe :usage-of "zero-dns" :args "iface"))
  (uiop:quit (if condition 1 0)))

(defun parse-arguments-or-signal ()
  "Parse command line arguments or signal ZDNS-ERROR on failure"
  (multiple-value-bind (options arguments)
      (opts:get-opts)
    (let ((sending-interval  (getf options :sending-interval *sending-interval*))
          (time-to-live      (getf options :time-to-live     *time-to-live*))
          (multicast-address (getf options :address          *multicast-address*))
          (network           (getf options :network          *network*))
          (socket-directory  (getf options :socket-directory *socket-directory*))
          (help              (getf options :help))
          (daemonize         (getf options :daemonize)))

      (when help
        (describe-and-quit :force-describe t))

      (when (/= (length arguments) 1)
        (error 'no-interface))

      (setq *sending-interval*  sending-interval
            *time-to-live*      time-to-live
            *multicast-address* multicast-address
            *network*           network
            *socket-directory*  socket-directory)
      (values
       (first arguments)
       daemonize))))

(defun main ()
  (handler-bind
      (((or no-interface
            opts:troublesome-option
            sb-int::file-error)
         (lambda (c) (describe-and-quit :condition c))))
    (multiple-value-bind (interface daemonize)
        (parse-arguments-or-signal)
      (zero-dns interface :daemonize daemonize)
      (uiop:quit))))
