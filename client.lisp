                                        ; Lisp client for BaseX.
                                        ; Works with BaseX 6.1
                                        ;
                                        ; Documentation: http://docs.basex.org/wiki/Clients
                                        ; 
                                        ; (C) Andy Chambers, Formedix Ltd 2010, BSD License

(defpackage :basex
  (:use :cl :usocket)
  (:export :session
   :execute
           :close-session
   :info
           :result))

(in-package :basex)

(defconstant +default-read-buffer-size+ 65536)

(defclass session ()
  ((host :initarg :host :initform "localhost")
   (port :initarg :port :initform 1984)
   (user :initarg :user :initform "admin")
   (pw :initarg :pw :initform "admin")
   (sock :initform nil)
   (result :initform nil :accessor result)
   (info :initform nil :accessor info)))

(defmethod initialize-instance :after ((self session) &key)
  (with-slots (host port user pw sock) self
    (setf sock (socket-connect host port :element-type '(unsigned-byte 8)))
    (unless (hand-shake self)
      (error "Could not initiate connection"))))

(defun hand-shake (session)
  (declare (optimize debug))
  (labels ((md5 (str)
             (string-downcase (with-output-to-string (s)
                                (loop for hex across
                                              (md5:md5sum-string str)
                                      do (format s "~2,'0x" hex)))))
           (auth-token (pw timestamp)
             (md5 (format nil "~a~a"
                          (md5 pw) timestamp))))

    (with-slots (user pw sock) session
      (let* ((ts (read-null-terminated (socket-stream sock)))
             (auth (auth-token pw ts)))
        (write-null-terminated user (socket-stream sock))
        (write-null-terminated auth (socket-stream sock))
        (force-output (socket-stream sock))
        (eq 0 (read-byte (socket-stream sock)))))))

(defun read-null-terminated (in)
  (let ((buf (make-array +default-read-buffer-size+ :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for byte = (read-byte in)
          until (zerop byte)
          do (vector-push-extend byte buf))
    (babel:octets-to-string buf :encoding :utf-8)))

(defun write-null-terminated (string out)
  (loop for byte across (babel:string-to-octets string :encoding :utf-8)
        do (write-byte byte out))
  (write-byte 0 out))

(defmethod execute ((self session) query)
  (with-slots (sock) self
    (let ((stream (socket-stream sock)))
      (write-null-terminated query stream)
      (force-output stream)
      (setf (result self) (read-null-terminated stream)
            (info self) (read-null-terminated stream))
      (eq 0 (read-byte (socket-stream sock))))))

(defmethod open-session ((self session))
  (unwind-protect
       (unless (hand-shake self)
         (error "Could not open session"))
    (close-session self)))

(defmethod close-session ((self session))
  (with-slots (sock) self
    (write-null-terminated "exit" (socket-stream sock))
    (socket-close sock)))
