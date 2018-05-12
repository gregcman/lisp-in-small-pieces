(defpackage #:lisp-in-small-pieces
  (:use :cl)
  (:shadow
   #:defun
   #:defparameter
   #:fmakunbound
   #:makunbound
   )
  (:nicknames #:lisp))
(in-package #:lisp)
(defmacro defun (name args &body body)
  `(progn (cl:defun ,name ,args ,@body)
	  (setf (gethash ',name *function-namespace*) t)
	  (setf (get
		 ',name
		 'defun)
		'(lambda ,args ,@body))))

(defmacro defparameter (name value)
  `(progn (cl:defparameter ,name ,value)
	  (setf (gethash ',name *variable-namespace*) t)
	  (setf (get ',name 'defparameter)
		',value)))

(defparameter *function-namespace* (make-hash-table :test 'eq))
(defparameter *variable-namespace* (make-hash-table :test 'eq))

(defun fmakunbound (name)
  (remf name 'defun)
  (remhash name *function-namespace*)
  (cl:fmakunbound name))
(defun makunbound (name)
  (remf name 'defparameter)
  (remf name 'defvar)
  (remhash name *variable-namespace*)
  (cl:makunbound name))

(setf *print-case* :downcase)
