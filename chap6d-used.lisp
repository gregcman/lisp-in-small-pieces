(in-package :lisp)

;;;                      Threaded interpreter.
;;; Environment is held by a global variable. This is bad for //ism.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

(defparameter *env* '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)

(defun compute-kind (r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)))

(defun local-variable? (r i n)
  (and (pair? r)
       (named-let scan ((names (car r))
			(j 0))
	 (cond ((pair? names) 
		(if (eq? n (car names))
		    `(local ,i ,@j)
		    (scan (cdr names) (+ 1 j))))
	       ((null? names)
		(local-variable? (cdr r) (+ i 1) n))
	       ((eq? n names)
		`(local ,i ,@j))))))

(defun global-variable? (g n)
  (let ((var (assq n g)))
    (and (pair? var)
         (cdr var))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of local environments, they contain the values of
;;; the local variables (but global and predefined variables).
;;; Runtime environment or, activation frames, are represented by 
;;; vectors (named v*). They have the following structure:
;;;           +------------+
;;;           | next       |  ---> next V*
;;;           | argument 0 |  value of the first argument
;;;           | argument 1 |  value of the second argument
;;;           .            .
;;;           | free slot  |  Free slot for nary variable
;;;           +------------+
;;; The number of arguments can be extracted from the size of the
;;; activation frame.

;;; A direct implementation with inlined vectors is approximatively 
;;; 7 times faster under sci.
#+nil
(define-class environment Object 
  (next))
(progn
  (defclass environment ()
    ((next :initarg next)))
  (defun environment? (obj)
    (typep obj 'environment))
  (defun environment-next (obj)
    (slot-value obj 'next))
  (defun set-environment-next! (obj new)
    (setf (slot-value obj 'next) new))
  (defun make-environment (next)
    (make-instance 'environment
		   'next next)))
#+nil
(define-class activation-frame environment
  ((* argument)))

(progn
  (defclass activation-frame (environment)
    ((argument :initarg argument)))
  (defun activation-frame? (obj)
    (typep obj 'activation-frame))
  
  (defun activation-frame-argument (obj index)
    (aref (slot-value obj 'argument) index))
  (defun activation-frame-argument-length (obj)
    (array-total-size (slot-value obj 'argument)))
  (defun set-activation-frame-argument! (obj index new)
    (setf (aref (slot-value obj 'argument) index) new))

  (defun activation-frame-next (obj)
    (slot-value obj 'next))
  (defun set-activation-frame-next! (obj new)
    (setf (slot-value obj 'next) new))
  
  (defun allocate-activation-frame (n)
    (make-instance 'activation-frame
		   'argument (make-vector n))))

(defun sr-extend* (sr v*)
  (set-environment-next! v* sr)
  v*)

(defparameter sr.init '())

;;; Fetch the value of the Ith argument of the Jth frame.

(defun deep-fetch (sr i j)
  (if (= i 0)
      (activation-frame-argument sr j)
      (deep-fetch (environment-next sr)
		  (- i 1)
		  j)))

(defun deep-update! (sr i j v)
  (if (= i 0)
      (set-activation-frame-argument! sr j v)
      (deep-update! (environment-next sr)
		    (- i 1)
		    j
		    v)))

;;; R is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(defun r-extend* (r n*)
  (cons n* r))

(defparameter r.init '())

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.

(defparameter g.current '())

(defparameter sg.current (make-vector 100))

(defun g.current-extend! (n)
  (let ((level (length g.current)))
    (push (cons n `(global ,@level)) g.current)
    level))

(defun global-fetch (i)
  (vector-ref sg.current i))

(defun global-update! (i v)
  (vector-set! sg.current i v))

(defun g.current-initialize! (name)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((global)
           (vector-set! sg.current (cdr kind) undefined-value))
          (otherwise (static-wrong "Wrong redefinition" name)))
        (let ((index (g.current-extend! name)))
          (vector-set! sg.current index undefined-value))))
  name)

;;; This tag is used in the value cell of uninitialized variables.

(defparameter undefined-value (cons 'undefined 'value))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.

(defparameter g.init '())

(defparameter sg.init (make-vector 100))

(defun predefined-fetch (i)
  (vector-ref sg.init i))

(defun g.init-extend! (n)
  (let ((level (length g.init)))
    (push (cons n `(predefined ,@level)) g.init)
    level))

;;; Add that value is associated to name in the predefined global environment.

(defun g.init-initialize! (name value)
  (let ((kind (compute-kind r.init name)))
    (if kind
        (case (car kind)
          ((predefined)
           (vector-set! sg.init (cdr kind) value))
          (otherwise (static-wrong "Wrong redefinition" name)))
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value))))
  name)

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.
(defmacro definitial (name value)
  `(g.init-initialize! ',name ,value))

;;; Preserve the current modifiable global environment (containing a,
;;; b, foo, fact, fib etc.) All tests will be compiled in that environment.
(let ((g g.current))
  (defun original.g.current ()
    g))

(defmacro defprimitive (name value number)
  (ecase number
    (0 `(defprimitive0 ,name ,value))
    (1 `(defprimitive1 ,name ,value))
    (2 `(defprimitive2 ,name ,value))
    (3 `(defprimitive3 ,name ,value))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of functions. A redefinition with inlined vectors
;;; for more speed.

#+nil
(define-class closure Object
  (code
   closed-environment))
(progn
  (defclass closure ()
    ((code :initarg code)
     (closed-environment :initarg closed-environment)))
  (defun closure? (obj)
    (typep obj 'closure))
  (defun closure-code (obj)
    (slot-value obj 'code))
  (defun closure-closed-environment (obj)
    (slot-value obj 'closed-environment))
  (defun set-closure-code! (obj new)
    (setf (slot-value obj 'code) new))
  (defun set-closure-closed-environment! (obj)
    (setf (slot-value obj 'closed-environment) obj))
  (defun make-closure (code closed-environment)
    (make-instance 'closure
		   'code code
		   'closed-environment closed-environment)))


;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.

(defparameter desc.init '())

(defun description-extend! (name description)
  (push (cons name description) desc.init)
  name)

;;; Return the description or +false+ if absent.

(defun get-description (name)
  (let ((p (assq name desc.init)))
    (and (pair? p)
	 (cdr p))))
        


;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; SR is the representation of the local lexical environment
;;; TAIL? is a boolean that indicates if E is a terminal call (also
;;; means whether the *env* register should be restored or not).

(defun meaning (e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?))
      (case (car e)
        ((quote)  (meaning-quotation (cadr e) r tail?))
        ((lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)  (meaning-sequence (cdr e) r tail?))
        ((set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
        (otherwise     (meaning-application (car e) (cdr e) r tail?)))))

(defun meaning-reference (n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (SHALLOW-ARGUMENT-REF j)
                 (DEEP-ARGUMENT-REF i j))))
          ((global)
           (let ((i (cdr kind)))
             (CHECKED-GLOBAL-REF i)))
          ((predefined)
           (let ((i (cdr kind)))
             (PREDEFINED i))))
        (static-wrong "No such variable" n))))

(defun meaning-quotation (v r tail?)
  (CONSTANT v))

(defun meaning-alternative (e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r +false+))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)))
    (ALTERNATIVE m1 m2 m3)))

(defun meaning-assignment (n e r tail?) 
  (let ((m (meaning e r +false+))
        (kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local)
           (let ((i (cadr kind))
                 (j (cddr kind)))
             (if (= i 0)
                 (SHALLOW-ARGUMENT-SET! j m)
                 (DEEP-ARGUMENT-SET! i j m))))
          ((global)
           (let ((i (cdr kind)))
             (GLOBAL-SET! i m)))
          ((predefined)
           (static-wrong "Immutable predefined variable" n)))
        (static-wrong "No such variable" n))))

(defun meaning-sequence (e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?))
      (static-wrong "Illegal syntax: (begin)")))

(defun meaning*-single-sequence (e r tail?) 
  (meaning e r tail?))

(defun meaning*-multiple-sequence (e e+ r tail?)
  (let ((m1 (meaning e r +false+))
        (m+ (meaning-sequence e+ r tail?)))
    (%SEQUENCE% m1 m+)))

(defun meaning-abstraction (nn* e+ r tail?)
  (named-let parse ((n* nn*)
		    (regular '()))
    (cond
      ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
      ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
      (t (meaning-dotted-abstraction 
	  (reverse regular) n* e+ r tail?)))))

(defun meaning-fix-abstraction (n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 +true+)))
    (FIX-CLOSURE m+ arity)))

(defun meaning-dotted-abstraction (n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 +true+)))
    (NARY-CLOSURE m+ arity)))

;;; Application meaning.

(defun meaning-application (e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (or (= (length (cddr desc)) (length e*))
                                (static-wrong 
                                 "Incorrect arity for primitive" e)
                               ))))))
         (meaning-primitive-application e e* r tail?))
        ((and (pair? e)
              (eq? 'lambda (car e)))
         (meaning-closed-application e e* r tail?))
        (t (meaning-regular-application e e* r tail?))))

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(defun meaning-closed-application (e ee* r tail?)
  (let ((nn* (cadr e)))
    (named-let parse ((n* nn*)
		      (e* ee*)
		      (regular '()))
      (cond
	((pair? n*) 
	 (if (pair? e*)
	     (parse (cdr n*) (cdr e*) (cons (car n*) regular))
	     (static-wrong "Too less arguments" e ee*)))
	((null? n*)
	 (if (null? e*)
	     (meaning-fix-closed-application 
	      nn* (cddr e) ee* r tail?)
	     (static-wrong "Too much arguments" e ee*)))
	(t (meaning-dotted-closed-application 
	    (reverse regular) n* (cddr e) ee* r tail?))))))

(defun meaning-fix-closed-application (n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) +false+))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail? (TR-FIX-LET m* m+) 
        (FIX-LET m* m+))))

(defun meaning-dotted-closed-application (n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) +false+))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail? (TR-FIX-LET m* m+)
        (FIX-LET m* m+))))

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(defun meaning-primitive-application (e e* r tail?)
  (let* ((desc (get-description e))
         ;; desc = (function address . variables-list)
         (address (cadr desc))
         (size (length e*)))
    (case size
      ((0) (CALL0 address))
      ((1) 
       (let ((m1 (meaning (car e*) r +false+)))
         (CALL1 address m1)))
      ((2) 
       (let ((m1 (meaning (car e*) r +false+))
             (m2 (meaning (cadr e*) r +false+)))
         (CALL2 address m1 m2)))
      ((3) 
       (let ((m1 (meaning (car e*) r +false+))
             (m2 (meaning (cadr e*) r +false+))
             (m3 (meaning (caddr e*) r +false+)))
         (CALL3 address m1 m2 m3)))
      (otherwise (meaning-regular-application e e* r tail?)))))

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(defun meaning-regular-application (e e* r tail?)
  (let* ((m (meaning e r +false+))
         (m* (meaning* e* r (length e*) +false+)))
    (if tail? (TR-REGULAR-CALL m m*) (REGULAR-CALL m m*))))

(defun meaning* (e* r size tail?)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size tail?)
      (meaning-no-argument r size tail?)))

(defun meaning-dotted* (e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*) 
                                     r size arity tail?)
      (meaning-no-dotted-argument r size arity tail?)))

(defun meaning-some-arguments (e e* r size tail?)
  (let ((m (meaning e r +false+))
        (m* (meaning* e* r size tail?))
        (rank (- size (+ (length e*) 1))))
    (STORE-ARGUMENT m m* rank)))

(defun meaning-some-dotted-arguments (e e* r size arity tail?)
  (let ((m (meaning e r +false+))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (+ (length e*) 1))))
    (if (< rank arity) (STORE-ARGUMENT m m* rank)
        (CONS-ARGUMENT m m* arity))))

(defun meaning-no-argument (r size tail?)
  (ALLOCATE-FRAME size))

(defun meaning-no-dotted-argument (r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity))

;;; Gather into a list all arguments from arity+1 to the end of the
;;; activation frame and store this list into the arity+1th slot.

(defun listify! (v* arity)
  (named-let rec ((index (- (activation-frame-argument-length v*) 1))
		  (result '()))
    (if (= arity index)
        (set-activation-frame-argument! v* arity result)
        (rec (- index 1)
	     (cons (activation-frame-argument v* (- index 1))
		   result)))))
