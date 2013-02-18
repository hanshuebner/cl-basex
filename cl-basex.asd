
(defsystem :cl-basex
    :description "Client interface to the BaseX database"
  :author "Andy Chambers"
  :serial t
  :depends-on (:usocket
               :md5
               :babel)
  :components ((:file "client")))
