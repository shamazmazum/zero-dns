(defpackage zero-dns
  (:use #:cl
        #:nibbles
        #:ip-interfaces
        #:bordeaux-threads
        #:cl-daemonize)
  (:export #:zero-dns
           #:main
           ;; Parameters
           #:*sending-interval*
           #:*time-to-live*
           #:*multicast-address*
           #:*zdns-port*
           #:*query-port*))
