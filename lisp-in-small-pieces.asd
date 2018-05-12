(asdf:defsystem #:lisp-in-small-pieces
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:file "other")
   #+nil
   (:file "lisp-in-small-pieces")

   (:file "lisp-in-small-pieces2")))

