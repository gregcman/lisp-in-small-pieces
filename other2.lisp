(in-package :lisp)

(macrolet ((alias (scheme-name cl-name)
	     `(defun ,scheme-name (obj)
		(,cl-name obj))))
  (alias atom? atom)
  (alias symbol? symbolp)
  (alias pair? consp)
  (alias null? null))

(defun param-names (params)
  (mapcar (lambda (x)
	    (if (symbolp x)
		x
		(first x)))
	  params))

(defmacro named-let (name params &body body)
  (let* ((param-names (param-names params))
	 (rec-param-names (mapcar (lambda (x) (gensym (string x)))
				  param-names)))
    (with-gensyms (start)
      `(let ,params
	 (tagbody
	    ,start
	    (flet ((,name ,rec-param-names
		     (setf (values ,@param-names)
			   (values ,@rec-param-names))
		     (go ,start)))
	      ,@body))))))

