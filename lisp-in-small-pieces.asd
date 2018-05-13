(asdf:defsystem #:lisp-in-small-pieces
  :depends-on ()
  :serial t
  :components
  ((:file "package2")
   (:file "chap7d")
   (:file "chap6d")
   #+nil
   (:file "package")
   #+nil
   (:module "interpreters"
    #+nil
    (:file "lisp-in-small-pieces")
    (:file "other")
    (:file "lisp-in-small-pieces2"))))

