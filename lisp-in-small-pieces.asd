(asdf:defsystem #:lisp-in-small-pieces
  :depends-on (#:utility
	       #:scheme2common-lisp)
  :serial t
  :components
  ((:file "package2")
   (:file "other2")
   (:file "tester")
   (:file "chap6d")
   (:file "chap7d")
   #+nil
   (:file "package")
   #+nil
   (:module "interpreters"
    #+nil
    (:file "lisp-in-small-pieces")
    (:file "other")
    (:file "lisp-in-small-pieces2"))))

