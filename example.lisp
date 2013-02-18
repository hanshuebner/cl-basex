;; This example shows how database commands can be executed.
;;
;; (C) Andy Chambers, Formedix Ltd 2010, BSD License
;; (C) Hans Hübner, 2013

(defpackage :basex-user
 (:use :cl))

(in-package :basex-user)

(time
 (let ((session (basex:connect)))
   (multiple-value-bind (result info)
       (basex:execute session "xquery 1 to 10")
     (format t "result: ~A~%info: ~A~%" result info))
   (basex:disconnect session)))
