(in-package :lisp)

(defun static-wrong (message &rest args)
  (print args)
  (error message))

(defun wrong (message &rest args)
  (print args)
  (error message))

(defun signal-exception (message args)
  (declare (ignore message))
  (print args))
