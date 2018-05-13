(in-package :lisp)

(macrolet ((alias (scheme-name cl-name)
	     `(defun ,scheme-name (obj)
		(,cl-name obj))))
  (alias atom? atom)
  (alias symbol? symbolp)
  (alias pair? consp)
  (alias null? null))
