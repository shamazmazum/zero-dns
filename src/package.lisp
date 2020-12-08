(defpackage mdns
  (:use #:cl
        #:nibbles
        #:ip-interfaces
        #:bordeaux-threads)
  (:export #:mdns
           #:tell-ip-addr
           #:tell-hostname))
