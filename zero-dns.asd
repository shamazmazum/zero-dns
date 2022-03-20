(defsystem :zero-dns
  :name :zero-dns
  :description "Simple DNS service on top of ZeroMQ"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :version "0.1"
  :depends-on (:flexi-streams
               :alexandria
               :nibbles
               :ip-interfaces
               :pzmq
               :cl-store
               :cl-daemonize
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
               (:file "zero-dns")))

(defsystem :zero-dns/executable
  :pathname "app/"
  :serial t
  :components ((:file "package")
               (:file "standalone"))
  :depends-on (:zero-dns
               :alexandria
               :unix-opts
               :esrap)
  :build-operation program-op
  :build-pathname "zero-dns"
  :entry-point "zero-dns-app:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
