(defpackage zero-dns
  (:use #:cl
        #:nibbles
        #:ip-interfaces
        #:bordeaux-threads
        #:cl-daemonize)
  (:export #:zero-dns
           ;; Conditions
           #:zdns-error
           ;; Parameters
           #:*sending-interval*
           #:*time-to-live*
           #:*multicast-address*
           #:*socket-directory*))
