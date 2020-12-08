(defsystem :mdns
  :name :mdns
  :description "mDNS service"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :version "0.1"
  :depends-on (:flexi-streams
               :nibbles
               :ip-interfaces
               :pzmq
               :cl-store
               :bordeaux-threads)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "definitions")
               (:file "protocol")
               (:file "bookkeeper")
               (:file "bookkeeper-service")
               (:file "sender")
               (:file "receiver")
               (:file "mdns")))
