(defpackage zero-dns
  (:use #:cl
        #:nibbles
        #:ip-interfaces
        #:bordeaux-threads
        #:cl-daemonize)
  (:local-nicknames (:alex :alexandria)
                    (:sera :serapeum))
  (:export #:zero-dns
           ;; Conditions
           #:zdns-error
           ;; Types
           #:network
           ;; Parameters
           #:*network*
           #:*sending-interval*
           #:*time-to-live*
           #:*multicast-address*
           #:*socket-directory*))
