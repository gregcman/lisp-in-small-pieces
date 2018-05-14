(in-package :lisp)
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
#+nil
(defun invoke (f v*)
  (if (closure? f)
      (funcall (closure-code f) v* (closure-closed-environment f))
      (wrong "Not a function" f)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(defun SHALLOW-ARGUMENT-REF (j)
  (lambda () (activation-frame-argument *env* j)))

(defun PREDEFINED (i)
  (lambda () (predefined-fetch i)))

(defun DEEP-ARGUMENT-REF (i j)
  (lambda () (deep-fetch *env* i j)))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (lambda () (set-activation-frame-argument! *env* j (m))))

(defun DEEP-ARGUMENT-SET! (i j m)
  (lambda () (deep-update! *env* i j (m))))

(defun GLOBAL-REF (i)
  (lambda () (global-fetch i)))

;;; Note that we lost the name of the variable, it must be retrieved
;;; from elsewhere.   TOBEDONE

(defun CHECKED-GLOBAL-REF (i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable")
          v))))

(defun GLOBAL-SET! (i m)
  (lambda () (global-update! i (m))))

(defun CONSTANT (value)
  (lambda () value))

(defun ALTERNATIVE (m1 m2 m3)
  (lambda () (if (m1) (m2) (m3))))

(defun %SEQUENCE% (m m+)
  (lambda () (m) (m+)))

(defun TR-FIX-LET (m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (m+)))

(defun FIX-LET (m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (let ((result (m+)))
      (set! *env* (environment-next *env*))
      result)))

(defun CALL0 (address)
  (lambda () (address)))

(defun CALL1 (address m1)
  (lambda () (address (m1))))

(defun CALL2 (address m1 m2)
  (lambda () (let ((v1 (m1))) 
               (address v1 (m2)))))

(defun CALL3 (address m1 m2 m3)
  (lambda () (let* ((v1 (m1))
                    (v2 (m2)))
               (address v1 v2 (m3)))))

(defun FIX-CLOSURE (m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (labels ((the-function (v* sr)
		 (if (= (activation-frame-argument-length v*) arity+1)
		     (begin (set! *env* (sr-extend* sr v*))
			    (m+))
		     (wrong "Incorrect arity")))) 
	(make-closure
	 (function the-function)
	 *env*)))))

(defun NARY-CLOSURE (m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (labels ((the-function (v* sr)
		 (if (>= (activation-frame-argument-length v*) arity+1)
		     (begin 
		      (listify! v* arity)
		      (set! *env* (sr-extend* sr v*))
		      (m+))
		     (wrong "Incorrect arity")))) 
	(make-closure (function the-function) *env*)))))

(defun TR-REGULAR-CALL (m m*) 
  (lambda ()
    (let ((f (m)))
      (invoke f (m*)))))

(defun REGULAR-CALL (m m*)
  (lambda ()
    (let* ((f (m))
           (v* (m*))
           (sr *env*)
           (result (invoke f v*)))
      (set! *env* sr)
      result)))

(defun STORE-ARGUMENT (m m* rank)
  (lambda ()
    (let* ((v (m))
           (v* (m*)))
      (set-activation-frame-argument! v* rank v)
      v*)))

(defun CONS-ARGUMENT (m m* arity)
  (lambda ()
    (let* ((v (m))
           (v* (m*)))
      (set-activation-frame-argument! 
       v* arity (cons v (activation-frame-argument v* arity)))
      v*)))

(defun ALLOCATE-FRAME (size)
  (let ((size+1 (+ size 1)))
    (lambda ()
      (allocate-activation-frame size+1))))

(defun ALLOCATE-DOTTED-FRAME (arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        v*))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

(defmacro defprimitive1 (name value)
  `(definitial ,name
       (letrec ((arity+1 (+ 1 1))
		(behavior
		 (lambda (v* sr)
		   (if (= (activation-frame-argument-length v*) 
			  arity+1)
		       (,value (activation-frame-argument v* 0))
		       (wrong "Incorrect arity" ',name)))))
	 (description-extend! ',name `(function ,',value a))
	 (make-closure behavior sr.init))))
(defmacro defprimitive2 (name value)
  `(definitial ,name
      (letrec ((arity+1 (+ 2 1))
	       (behavior
		(lambda (v* sr)
		  (if (= (activation-frame-argument-length v*)
			 arity+1)
		      (value (activation-frame-argument v* 0) 
			     (activation-frame-argument v* 1))
		      (wrong "Incorrect arity" ',name)))))
	(description-extend! ',name `(function ,',value a b))
	(make-closure behavior sr.init))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive symbol? symbol? 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive display display 1)

;;; We do not need to save the register *env* since call/cc is not a
;;; primitive (it is defined by definitial and not by defprimitive)
;;; and non-primitive invokations are regularly handled.

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)))
    (make-closure
     (lambda (v* sr)
       (if (= arity+1 (activation-frame-argument-length v*))
           (call/cc
            (lambda (k)
              (invoke 
               (activation-frame-argument v* 0)
               (let ((frame (allocate-activation-frame (+ 1 1))))
                 (set-activation-frame-argument! 
                  frame 0
                  (make-closure
                   (lambda (values r)
                     (if (= (activation-frame-argument-length values)
                            arity+1)
                         (k (activation-frame-argument values 0))
                         (wrong "Incorrect arity" 'continuation)))
                   sr.init))
                 frame))))
           (wrong "Incorrect arity" 'call/cc)))
     sr.init)))

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)))
    (make-closure
     (lambda (v* sr)
       (if (>= (activation-frame-argument-length v*) arity+1)
           (let* ((proc (activation-frame-argument v* 0))
                  (last-arg-index
                   (- (activation-frame-argument-length v*) 2))
                  (last-arg 
                   (activation-frame-argument v* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)))
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument v* i)))
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)))
                 ((null? last-arg))
               (set-activation-frame-argument! 
                frame i (car last-arg)))
             (invoke proc frame))
           (wrong "Incorrect arity" 'apply)))
     sr.init)))

(definitial list (funcall (NARY-CLOSURE (SHALLOW-ARGUMENT-REF 0) 0)))


;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing

(defun chapter63-interpreter ()
  (labels ((toplevel ()
	     (set! *env* sr.init)
	     (display (funcall (meaning (read) r.init +true+)))
	     (toplevel))) 
    (toplevel)))

;;; This variant produces a table of symbols.

(defparameter sg.current.names (list 'foo))

(defun stand-alone-producer (e)
  (set! g.current (original.g.current))
  (let* ((m (meaning e r.init +true+))
         (size (length g.current))
         (global-names (mapcar (function car) (reverse g.current))))
    (lambda ()
      (set! sg.current (make-vector size undefined-value))
      (set! sg.current.names global-names)
      (set! *env* sr.init)
      (m))))

(defun CHECKED-GLOBAL-REF+ (i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" 
                 (list-ref sg.current.names i))
          v))))  

;;; this one requires to close the name of the variables that must be
;;; checked. To use it you must also change meaning-reference that calls it.

(defun CHECKED-GLOBAL-REF- (i n)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable" n)
          v)))) 

;;; retrofit for tests.
(setf (symbol-function 'CHECKED-GLOBAL-REF)
      (function CHECKED-GLOBAL-REF+))

(defun scheme6d ()
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   +true+
   (lambda (read print error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (set! *env* sr.init)
       (print (funcall (stand-alone-producer (read))))))))

(defun test-scheme6d (file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   +true+
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (check (funcall (stand-alone-producer (read))))))
   equal?))

;;; Pay attention to tail-rec in Scheme->C.

(defun bench6d (factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e r.init +true+)))
    (named-let rec ((factor factor))
      (set! *env* sr.init)
      (let ((v (m)))
        (let ((duration (- (get-internal-run-time) start)))
          (when (<= factor 1)
            (display (list duration v))
            (newline))))
      (if (> factor 1)
          (rec (- factor 1))))))

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The following code use 
;;; pp to pretty-print expressions,
;;; and eval for a local hack (should be made of macros instead).

(defparameter combinator-names
  '(SHALLOW-ARGUMENT-REF
    PREDEFINED
    DEEP-ARGUMENT-REF
    SHALLOW-ARGUMENT-SET!
    DEEP-ARGUMENT-SET!
    GLOBAL-REF
    CHECKED-GLOBAL-REF
    GLOBAL-SET!
    CONSTANT
    ALTERNATIVE
    %SEQUENCE%
    TR-FIX-LET
    FIX-LET
    CALL0
    CALL1
    CALL2
    CALL3
    FIX-CLOSURE
    NARY-CLOSURE
    TR-REGULAR-CALL
    REGULAR-CALL
    STORE-ARGUMENT
    CONS-ARGUMENT
    ALLOCATE-FRAME
    ALLOCATE-DOTTED-FRAME))

(let ((originals (mapcar (function symbol-function) combinator-names)))
  (defun install-regular-combinators ()
      (for-each (lambda (old-value name)
		  (eval `(set! ,name ',old-value)))
		originals
		combinator-names)))

(defun install-disassembling-combinators ()
  (for-each (lambda (name)
	      (eval `(set! ,name (lambda args (,name ,@args)))))
	    combinator-names))

(defun %disassemble% (e)
  (install-disassembling-combinators)
  (pp (meaning e r.init +true+))
  (install-regular-combinators)
  (newline))

;;; (disassemble '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
;;; (disassemble '(lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n))))
 
;;; end of chap6d.scm
