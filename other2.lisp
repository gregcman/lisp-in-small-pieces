(in-package :lisp)

(defmacro alias0 (scheme-name cl-name)
  `(eval-always
     (setf (symbol-function ',scheme-name)
	   (function ,cl-name))))

(alias0 pp pprint)

(etouq
  (cons
   'progn
   (mapcar
    (lambda (x) (cons 'alias0 x))
    '((atom? atom)
      (symbol? symbolp)
      (pair? consp)   
      (null? null)
      (eq? eq)
      (equal? equal)
      (integer? integerp)
      (for-each mapc)
      (assq (lambda (item alist) (assoc item alist :test 'eq)))
      (reverse! nreverse)
      
      ;;(assoc (lambda (item alist) (assoc item alist :test equal)))
      (assv assoc)
      (display princ)
      (eqv? eql)
      (list-ref nth)
      (list-tail nthcdr)
      
      ;;(map mapcar)
      ;;(member (lambda (item list) (member item list :test equal)))
      (memq (lambda (item list) (member item list :test 'eq)))
      (memv member)
      (newline terpri)
      (set-car! rplaca)
      (set-cdr! rplacd)
      (vector-ref aref)
      (vector-set! (lambda (array index value) (setf (aref array index) value)))
      (string-ref aref)
      (string-set! vector-set!)
      (symbol->string intern)
      (vector-length array-total-size)
      
      ;;(write prin1)
      ))))

(defmacro alias2 (scheme-name cl-name)
  `(defmacro ,scheme-name (&rest rest)
     `(,',cl-name ,@rest)))

(alias2 set! setq)
(alias2 begin progn)

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
	 (block exit
	   (tagbody
	      ,start
	      (return-from exit
		(flet ((,name ,rec-param-names
			 (setf (values ,@param-names)
			       (values ,@rec-param-names))
			 (go ,start)))
		  ,@body))))))))

(defun make-vector (length &optional (obj nil))
  (make-array length :initial-element obj))

(defun static-wrong (message &rest args)
  (print args)
  (error message))

(defun wrong (message &rest args)
  (print args)
  (error message))

(defun signal-exception (message args)
  (declare (ignore message))
  (print args))

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
