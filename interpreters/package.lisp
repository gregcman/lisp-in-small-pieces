(defpackage #:lisp-in-small-pieces
  (:use :cl)
  (:shadow
   #:defun
   #:defparameter
   #:fmakunbound
   #:makunbound
   #:boundp
   #:fboundp
   )
  (:nicknames #:lisp))
(in-package #:lisp)
(defmacro defun (name args &body body)
  `(progn (cl:defun ,name ,args ,@body)
	  (setf (gethash ',name *function-namespace*) t)
	  (setf (get
		 ',name
		 'defun)
		'(lambda ,args ,@body))
	  (setf (get
		 ',name
		 'arity)
		,(length args))))

(defmacro defparameter (name value)
  `(progn (cl:defparameter ,name ,value)
	  (setf (gethash ',name *variable-namespace*) t)
	  (setf (get ',name 'defparameter)
		',value)))

(defparameter *variable-namespace* (make-hash-table :test 'eq))
(defparameter *function-namespace* (make-hash-table :test 'eq))

(defparameter *not-found* (cons "not" "found"))
(defun prop-exists (name indicator)
  (not (eq *not-found* (get name indicator *not-found*))))
(defun boundp (name)
  (or (prop-exists name 'defvar)
      (prop-exists name 'defparameter)))
(defun fboundp (name)
  (prop-exists name 'defun))
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
(setf *print-circle* t)
