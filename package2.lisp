(defpackage #:lisp-in-small-pieces
  (:use #:cl #:utility)
  (:nicknames #:lisp))
(in-package #:lisp)
(setf *print-case* :downcase)
(setf *print-circle* t)

(defparameter *dir* (filesystem-util:this-directory))
(defparameter *scheme-test-file* (filesystem-util:rebase-path "scheme.tst" *dir*))
