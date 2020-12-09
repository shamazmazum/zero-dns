(defpackage zero-dns
  (:use #:cl
        #:nibbles
        #:ip-interfaces
        #:bordeaux-threads)
  (:export #:zero-dns
           ;; Parameters
           #:*sending-interval*
           #:*time-to-live*
           #:*multicast-address*
           #:*zdns-port*
           #:*query-port*))
