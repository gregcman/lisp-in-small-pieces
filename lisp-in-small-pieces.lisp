(in-package #:lisp)
;;;;values
(defparameter *the-false-value* nil)
(defun truep (value)
  (not (eq *the-false-value* value)))
(defun booleanp (value)
  (or (eq *the-false-value* value)
      (eq t value)))

(defparameter *empty-progn* 69)

;;;;environments
;;;;represented as alists where each cell is (symbol . value)
(defparameter *initial-environment* '())
(defun lookup (id env)
  (if (consp env)
      (if (eq (car (car env)) id)
	  (cdr (car env))
	  (lookup id (cdr env)))
      (error "no such binding ~s" id)))
(defun update! (id env value)
  (if (consp env)
      (if (eq (car (car env)) id)
	  (progn (rplacd (car env) value)
		 value)
	  (update! id (cdr env) value))
      (error "no such binding ~s" id)))
(defun extend (env variables values)
  (cond ((consp variables)
	 (if (consp values)
	     (cons (cons (car variables)
			 (car values))
		   (extend env
			   (cdr variables)
			   (cdr values)))
	     (error "too few values")))
	((null variables)
	 (if (null values)
	     env
	     (error "too many values")))
	((symbolp variables)
	 (cons (cons variables
		     values)
	       env))))

;;;;evaluation -> dynamic, no closures
(defun %eval (form env)
  (if (atom form)
      (if (symbolp form)
	  (lookup form env)	  
	  form)
      (case (car form)
	(quote (car (cdr form)))
	(if (if (truep (%eval (car (cdr form))
			      env))
		(%eval (car (cdr (cdr form)))
		       env)
 		(%eval (car (cdr (cdr (cdr form))))
		       env)))
	(progn (evaluate-progn (cdr form)
			       env))
	(setq (update! (car (cdr form))
		       env
		       (%eval (car (cdr (cdr form)))
			      env)))
	(lambda (make-function
		 (car (cdr form))
		 (cdr (cdr form))
		 env))
	(otherwise (%apply (%eval (car form) env)
			   (evlis (cdr form) env))))))
(defun evlis (params env)
  (if (consp params)
      (let ((arg0 (%eval (car params) env)))
	(cons arg0
	      (evlis (cdr params) env)))
      nil))
(defun evaluate-progn (body env)
  (if (consp body)
      (if (consp (cdr body))
	  (progn (%eval (car body)
			env)
		 (evaluate-progn (cdr body)
				 env))
	  (%eval (car body) env))
      *empty-progn*))

(defun %apply (function args)
  (if (functionp function)
      (funcall function args)
      (error "not a function ~s" function)))

(defun make-function (parameters body env)
  (lambda (values)
    (evaluate-progn body
		    (extend env
			    parameters
			    values))))
;;;;evaluation.d
;;;;closures are created with the "function" special form
(defun %eval.d (form env)
  (if (atom form)
      (if (symbolp form)
	  (lookup form env)	  
	  form)
      (case (car form)
	(quote (car (cdr form)))
	(if (if (truep (%eval.d (car (cdr form))
				env))
		(%eval.d (car (cdr (cdr form)))
			 env)
		(%eval.d (car (cdr (cdr (cdr form))))
			 env)))
	(progn (evaluate-progn.d (cdr form)
				 env))
	(setq (update! (car (cdr form))
		       env
		       (%eval.d (car (cdr (cdr form)))
				env)))
	(function
	 (let* ((f (car (cdr form)))
		(fun (make-function.d
		      (car (cdr f))
		      (cdr (cdr f))
		      env)))
	   (d.make-closure fun env)))
	(lambda (make-function.d
		 (car (cdr form))
		 (cdr (cdr form))
		 env))
	(otherwise (%apply.d (%eval.d (car form) env)
			     (evlis (cdr form) env)
			     env)))))
(defun evlis.d (params env)
  (if (consp params)
      (let ((arg0 (%eval.d (car params) env)))
	(cons arg0
	      (evlis.d (cdr params) env)))
      nil))
(defun evaluate-progn.d (body env)
  (if (consp body)
      (if (consp (cdr body))
	  (progn (%eval.d (car body)
			  env)
		 (evaluate-progn.d (cdr body)
				   env))
	  (%eval.d (car body)
		   env))
      *empty-progn*))

(defun %apply.d (function args env)
  (if (functionp function)
      (funcall function args env)
      (error "not a function ~s" function)))

(defun make-function.d (parameters body definition-env)
  (declare (ignore definition-env))
  (lambda (values current-env)
    (evaluate-progn body
		    (extend current-env
			    parameters
			    values))))

(defun d.make-closure (fun env)
  (lambda (values current-env)
    (declare (ignore current-env))
    (funcall fun values env)))

;;;;evaluation.s
;;;shallow binding is an implementation of dynamic binding
;;;where the symbol has a value cell that is changed and restored
(defun setvar (var new)
  (setf (get var 'apval) new))
(defun getvar (var)
  (get var 'apval))
(defun for-each (fun sequence &rest sequences)
  (apply #'map nil fun sequence sequences))
(defun s.make-function (variables body env)
  (declare (ignore env))
  (lambda (values current-env)
    (let ((old-bindings
	   (mapcar
	    (lambda (var val)
	      (let ((old-value (getvar var)))
		(setvar var val)
		(cons var old-value)))
	    variables
	    values)))
      (let ((result (evaluate-progn body current-env)))
	(for-each (lambda (b)
		    (setvar (car b)
			    (cdr b)))
		  old-bindings)
	result))))
(defun s.lookup (id env)
  (declare (ignore env))
  (getvar id))
(defun s.update! (id env value)
  (declare (ignore env))
  (setvar id value))


;;;misc
(defun %mapcar (function list)
  (if (consp list)
      (cons (funcall
	     function
	     (car list))
	    (%mapcar
	     function
	     (cdr list)))
      (quote ())))

(defparameter *global-environment*
  *initial-environment*)
(defparameter *void-value* 'void)

(defmacro definitial (name &optional (value nil value-supplied-p))
  `(progn (push (cons (quote ,name)
		      ,(if value-supplied-p
			   value
			   (list (quote quote) *void-value*)))
		*global-environment*)))

(defmacro defprimitive (name value arity)
  `(definitial ,name
       (lambda (values)
	 (if (= ,arity (length values))
	     (apply (function ,value) values)
	     (error "Incorrect arity ~s"
		    (list (quote ,name) values))))))

(definitial t t)
(definitial f *the-false-value*)
(definitial nil '())

(definitial foo)
(definitial fib)
(definitial bar)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive rplacd rplacd 2)
(defprimitive + + 2)
(defprimitive eq eq 2)
(defprimitive < < 2)

;;;type ":exit" to exit
(defun chapter1-scheme ()
  (labels ((toplevel ()
	     (let ((value (read)))
	       (unless (eq :exit value)
		 (print (%eval value *global-environment*))
		 (terpri)
		 (toplevel)))))
    (toplevel)))

;;;tests
#+nil
((lambda (a b c)
   	(+ c (a b)))
 (lambda (x)
      	(+ x x))
 2
 3)
