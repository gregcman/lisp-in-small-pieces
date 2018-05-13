(in-package :lisp)

(defmacro alias (scheme-name cl-name &optional (arity 1))
  (let ((params (make-gensym-list arity "obj")))
    `(defun ,scheme-name ,params
       (,cl-name ,@params))))
(alias atom? atom)
(alias symbol? symbolp)
(alias pair? consp)
(alias null? null)
(alias eq? eq 2)

(alias newline terpri 0)
(alias pp pprint 1)

(defmacro alias2 (scheme-name cl-name)
  `(defmacro ,scheme-name (&rest rest)
     `(,',cl-name ,@rest)))

(alias2 set! setq)
(alias2 begin progn)

(defmacro set! (var value)
  `(setq ,var ,value))

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

(defun assq (object alist)
  (assoc object alist :test 'eq))

(defun make-vector (length &optional (obj nil))
  (make-array length :initial-element obj))

(defun vector-ref (array index)
  (aref array index))
(defun vector-set! (array index value)
  (setf (aref array index) value))

(defun static-wrong (message &rest args)
  (print args)
  (error message))

(defun wrong (message &rest args)
  (print args)
  (error message))

(defun signal-exception (mess))

(defconstant +true+ t)
(defconstant +false+ nil)

(defmacro letrec (bindings &body body)
  (let ((vars (param-names bindings))
	(acc (list (quote progn))))
    (mapc (lambda (binding var)
	    (when (consp binding)
	      (let ((initial-form (cdr binding)))
		(when initial-form
		  (push `(setf ,var ,@initial-form) acc)))))
	  bindings
	  vars)
    `(let ,vars
       ,(nreverse acc)
       ,@body)))

#+nil
(letrec ((a 7)
	 (b (* a a))
	 (c (lambda () (print (list a b)))))
  (funcall c))
