;;;; info.read-eval-print.web-server.asd

(asdf:defsystem :info.read-eval-print.web-server
  :serial t
  :depends-on (:iolib
               :quek)
  :components ((:file "package")
               (:file "server")))

