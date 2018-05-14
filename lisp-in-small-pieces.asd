(asdf:defsystem #:lisp-in-small-pieces
  :depends-on (#:utility
	       #:filesystem-util)
  :serial t
  :components
  ((:file "package2")
   (:file "other2")
   (:file "tester")
   (:file "chap6d-used")
   (:file "chap7d")
   (:file "chap7f")
   #+nil
   (:file "package")
   #+nil
   (:module "interpreters"
    #+nil
    (:file "lisp-in-small-pieces")
    (:file "other")
    (:file "lisp-in-small-pieces2"))))

