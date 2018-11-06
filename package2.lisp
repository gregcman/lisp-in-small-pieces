(defpackage #:lisp-in-small-pieces
  (:use #:cl #:utility #:scheme2common-lisp)
  (:nicknames #:lisp))
(in-package #:lisp)
(setf *print-case* :downcase)
(setf *print-circle* t)

(defparameter *dir* (asdf:system-source-directory :lisp-in-small-pieces))
(defparameter *scheme-test-file* (rebase-path "scheme.tst" *dir*))
