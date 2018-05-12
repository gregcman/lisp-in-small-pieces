(in-package :lisp)

;;;;f.eval
(defun %eval (form env fenv)
  (if (atom form)
      (if (symbolp form)
	  (lookup form env)	  
	  form)
      (case (car form)
	(quote (car (cdr form)))
	(if (if (truep (%eval (car (cdr form))
			      env
			      fenv))
		(%eval (car (cdr (cdr form)))
		       env
		       fenv)
 		(%eval (car (cdr (cdr (cdr form))))
		       env
		       fenv)))
	(progn (evaluate-progn (cdr form)
			       env
			       fenv))
	(setq (update! (car (cdr form))
		       env
		       (%eval (car (cdr (cdr form)))
			      env
			      fenv)))
	(lambda (make-function
		 (car (cdr form))
		 (cdr (cdr form))
		 env
		 fenv))
	(function
	 (cond ((symbolp (car (cdr form)))
		(lookup (car (cdr form))
			fenv))
	       (t (error "incorrect function ~a" (car (cdr form))))))
	(otherwise (evaluate-application
		    (car form)
		    (evlis (cdr form)
			   env
			   fenv)
		    env
		    fenv)))))

(defun evaluate-application (var params env fenv)
  (cond ((symbolp var)
	 (%apply (lookup var fenv)
		 params))
	((and (consp var)
	      (eq (car var)
		  'lambda))
	 (evaluate-progn
	  (cdr (cdr var))
	  (extend env (car (cdr var)) params)
	  fenv))
	(t (error "incorrect functional term ~s" var))))
(defun evlis (params env fenv)
  (if (consp params)
      (let ((arg0 (%eval (car params)
			 env
			 fenv)))
	(cons arg0
	      (evlis (cdr params)
		     env
		     fenv)))
      nil))
(defun evaluate-progn (body env fenv)
  (if (consp body)
      (if (consp (cdr body))
	  (progn (%eval (car body)
			env
			fenv)
		 (evaluate-progn (cdr body)
				 env
				 fenv))
	  (%eval (car body)
		 env
		 fenv))
      *empty-progn*))

(defun %apply (function args)
  (if (functionp function)
      (funcall function args)
      (error "not a function ~s" function)))

(defun make-function (parameters body env fenv)
  (lambda (values)
    (evaluate-progn body
		    (extend env
			    parameters
			    values)
		    fenv)))
