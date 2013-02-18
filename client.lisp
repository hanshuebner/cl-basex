;; Lisp client for BaseX.
;; Works with BaseX 6.1
;;
;; Documentation: http://docs.basex.org/wiki/Clients
;; 
;; (C) Andy Chambers, Formedix Ltd 2010, BSD License

(defpackage :basex
  (:use :cl)
  (:export #:connect
           #:execute
           #:disconnect
           #:*db-host*
           #:*db-port*
           #:*db-user*
           #:*db-password*
           #:login-failed
           #:host
           #:port
           #:user
           #:execute-error
           #:query
           #:result
           #:info))

(in-package :basex)

(defconstant +default-read-buffer-size+ 65536)

(defparameter *db-host* "localhost")
(defparameter *db-port* 1984)
(defparameter *db-user* "admin")
(defparameter *db-password* "admin")

(define-condition login-failed (error)
  ((host :initarg :host :reader host)
   (port :initarg :port :reader port)
   (user :initarg :user :reader user))
  (:report (lambda (c stream)
             (format stream "could not log in to BaseX on ~A:~A (user ~S)"
                     (host c) (port c) (user c)))))

(define-condition execute-error (error)
  ((query :initarg :query :reader query)
   (result :initarg :result :reader result)
   (info :initarg :info :reader info))
  (:report (lambda (c stream)
             (format stream "query execution failed~%query ~A~%result ~A~%info ~A"
                     (query c) (result c) (info c)))))

(defclass session ()
  ((socket :initarg :socket :reader socket)))

(defun connect (&key
                (host *db-host*)
                (port *db-port*)
                (user *db-user*)
                (password *db-password*))
  (let ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (handler-bind
        ((login-failed (lambda (e)
                         (setf (slot-value e 'host) host
                               (slot-value e 'port) port
                               (slot-value e 'user) user)))
         (error (lambda (e)
                  (declare (ignore e))
                  (usocket:socket-close socket))))
      (hand-shake socket user password)
      (make-instance 'session :socket socket))))

(defmethod disconnect ((self session))
  (with-slots (socket) self
    (ignore-errors
     (write-null-terminated "exit" socket)
     (usocket:socket-close socket))))

(defun hand-shake (socket user password)
  (declare (optimize debug))
  (labels ((md5 (str)
             (string-downcase (with-output-to-string (s)
                                (loop for hex across (md5:md5sum-string str)
                                      do (format s "~2,'0x" hex)))))
           (auth-token (password timestamp)
             (md5 (format nil "~a~a"
                          (md5 password) timestamp))))

    (let* ((ts (read-null-terminated socket))
           (auth (auth-token password ts)))
      (write-null-terminated user socket)
      (write-null-terminated auth socket)
      (force-output (usocket:socket-stream socket))
      (unless (zerop (read-byte (usocket:socket-stream socket)))
        (error 'login-failed :user user)))))

(defun read-null-terminated (socket)
  (let ((buf (make-array +default-read-buffer-size+ :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for byte = (read-byte (usocket:socket-stream socket))
          until (zerop byte)
          do (vector-push-extend byte buf))
    (babel:octets-to-string buf :encoding :utf-8)))

(defun write-null-terminated (string socket)
  (loop for byte across (babel:string-to-octets string :encoding :utf-8)
        do (write-byte byte (usocket:socket-stream socket)))
  (write-byte 0 (usocket:socket-stream socket)))

(defmethod execute ((self session) query)
  (with-slots (socket) self
    (write-null-terminated query socket)
    (force-output (usocket:socket-stream socket))
    (let* ((result (read-null-terminated socket))
           (info (read-null-terminated socket)))
      (unless (eql 0 (read-byte (usocket:socket-stream socket)))
        (error 'execute-error :query query
                              :result result
                              :info info))
      (values result info))))


