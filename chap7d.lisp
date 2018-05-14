(in-package :lisp)
;;; Refinement of chap6d and chap7c. This interpreter introduces a
;;; *val* register and a *stack* to save/restore arguments that wait
;;; to be stored in an activation block. Functions now take their
;;; activation frame in the *val* register. Code is now a list of
;;; bytes.

;;; Load chap6d before.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The runtime machine

;(defparameter *env* +false+) ; already appears in chap6d
(defparameter *val* +false+)
(defparameter *fun* +false+)
(defparameter *arg1* +false+)
(defparameter *arg2* +false+)

(defparameter *pc* 0)
(defparameter *code* (vector 20))

(defparameter *constants* (vector))

;;; Some tests depend on 100 being the depth of the stack.
(defparameter *stack* (make-vector 100))
(defparameter *stack-index* 0)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(defun stack-push (v)
  (vector-set! *stack* *stack-index* v)
  (incf *stack-index*))
(defun stack-pop ()
  (decf *stack-index*)
  (vector-ref *stack* *stack-index*))
(defun save-stack ()
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy))
(defun restore-stack (copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*))
;;; Copy vector old[start..end[ into vector new[start..end[
(defun vector-copy! (old new start end)
  (named-let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)))))
(defun quotation-fetch (i)
  (vector-ref *constants* i))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

;;; make them inherit from invokable.
#+nil
(define-class primitive Object
  (address))
(progn
  (defclass primitive ()
    ((address :initarg address)))
  (defun primitive? (obj)
    (typep obj 'primitive))
  (defun primitive-address (obj)
    (slot-value obj 'address))
  (defun set-primitive-address! (obj new)
    (setf (slot-value obj 'address) new))
  (defun make-primitive (address)
    (make-instance 'primitive
		   'address address)))
#+nil
(define-class continuation Object
  (stack))
(progn
  (defclass continuation ()
    ((stack :initarg stack)))
  (defun continuation? (obj)
    (typep obj 'continuation))
  (defun continuation-stack (obj)
    (slot-value obj 'stack))
  (defun set-continuation-stack! (obj new)
    (setf (slot-value obj 'stack) new))
  (defun make-continuation (stack)
    (make-instance 'continuation
		   'stack stack)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

;;; This global variable holds at preparation time all the interesting
;;; quotations. It will be converted into *constants* for run-time.
;;; Quotations are not compressed and can appear multiply.

(defparameter *quotations* (list))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators that just expand into instructions.

(defun SHALLOW-ARGUMENT-SET! (j m)
  (append m
	  (SET-SHALLOW-ARGUMENT! j)))

(defun DEEP-ARGUMENT-SET! (i j m)
  (append m
	  (SET-DEEP-ARGUMENT! i j)))

(defun GLOBAL-SET! (i m)
  (append m
	  (SET-GLOBAL! i)))

;;; GOTO is not necessary if m2 is a tail-call but don't care.
;;; This one changed since chap7c.scm

(defun ALTERNATIVE (m1 m2 m3)
  (let ((mm2 (append m2 (GOTO (length m3)))))
    (append m1 (JUMP-FALSE (length mm2)) mm2 m3)))

(defun %SEQUENCE% (m m+)
  (append m
	  m+))

(defun TR-FIX-LET (m* m+)
  (append m*
	  (EXTEND-ENV)
	  m+))

(defun FIX-LET (m* m+)
  (append m*
	  (EXTEND-ENV)
	  m+
	  (UNLINK-ENV)))

(defun CALL0 (address)
  (INVOKE0 address))

(defun CALL1 (address m1)
  (append m1
	  (INVOKE1 address)))

(defun CALL2 (address m1 m2)
  (append m1
	  (PUSH-VALUE)
	  m2
	  (POP-ARG1)
	  (INVOKE2 address)))

(defun CALL3 (address m1 m2 m3)
  (append m1
	  (PUSH-VALUE) 
          m2
	  (PUSH-VALUE) 
          m3
	  (POP-ARG2)
	  (POP-ARG1)
	  (INVOKE3 address)))

(defun FIX-CLOSURE (m+ arity)
  (let* ((the-function
	  (append (ARITY=? (+ arity 1))
		  (EXTEND-ENV)
		  m+
		  (%RET%)))
         (the-goto (GOTO (length the-function))))
    (append (CREATE-CLOSURE (length the-goto))
	    the-goto
	    the-function)))

(defun NARY-CLOSURE (m+ arity)
  (let* ((the-function
	  (append (ARITY>=? (+ arity 1))
		  (PACK-FRAME! arity)
		  (EXTEND-ENV)
		  m+
		  (%RET%)))
         (the-goto (GOTO (length the-function))))
    (append (CREATE-CLOSURE (length the-goto))
	    the-goto
	    the-function)))

(defun TR-REGULAR-CALL (m m*)
  (append m
	  (PUSH-VALUE)
	  m*
	  (POP-FUNCTION)
	  (FUNCTION-GOTO)))

(defun REGULAR-CALL (m m*)
  (append m
	  (PUSH-VALUE)
	  m*
	  (POP-FUNCTION) 
          (PRESERVE-ENV)
	  (FUNCTION-INVOKE)
	  (RESTORE-ENV)))

(defun STORE-ARGUMENT (m m* rank)
  (append m
	  (PUSH-VALUE)
	  m*
	  (POP-FRAME! rank)))

(defun CONS-ARGUMENT (m m* arity)
  (append m
	  (PUSH-VALUE)
	  m*
	  (POP-CONS-FRAME! arity)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Instructions definers
;;; This uses the global fetch-byte function that increments *pc*.
(defparameter *instructions* (make-array 256 :initial-element nil))
(defparameter *instruction-names* (make-array 256 :initial-element nil))
(defparameter *instruction-arity* (make-array 256 :initial-element nil))
(defmacro define-instruction ((name &rest args) n &body body)
  (setf (aref *instruction-names* n) name)
  (setf (aref *instructions* n) `(lambda ,args ,@body))
  (setf (aref *instruction-arity* n) (length args)))
(defun instructionp (byte)
  (aref *instructions* byte))

(defmacro define-instruction-set ()
  `(defun dispatch-instruction (instruction)
     (case instruction
       ,@(let (acc)
	      (dotimes (byte 256)
		(when (instructionp byte)
		  (push `((,byte) (,(aref *instructions* byte)
				    ,@(make-list (aref *instruction-arity* byte)
						 :initial-element
						 '(fetch-byte))))
			acc)))
	      (nreverse acc)))))

#+nil
(defparameter *debugging* t)
(defun run ()
  (let ((instruction (fetch-byte)))

    #+nil
    (when *debugging*
      (inspect *val*)
      (print (aref *instructions* instruction))
      (format t ": ~s" (instruction-decode *code* (1- *pc*))))
    
    (dispatch-instruction instruction))
  (run))
(defun instruction-size (code pc)
  (let ((instruction (vector-ref code pc)))
    (+ 1 (aref *instruction-arity* instruction))))
(defun instruction-decode (code pc)
  (labels ((fetch-byte ()
	     (prog1 (vector-ref code pc)
	       (incf pc))))
    (let ((instruction (fetch-byte)))
      (let ((dump (list (aref *instruction-names* instruction))))
	(dotimes (x (aref *instruction-arity* instruction))
	  (push (fetch-byte) dump))
	(nreverse dump)))))

;;; Combinators

(defun check-byte (j)
  (unless (and (<= 0 j) (<= j 255))
    (static-wrong "Cannot pack this number within a byte" j)))
  
(defun SHALLOW-ARGUMENT-REF (j)
  (check-byte j)
  (case j
    ;;((0 1 2 3) (list (+ 1 j)))
    (otherwise (list 5 j))))

(defun PREDEFINED (i)
  (check-byte i)
  (case i
    ;; 0=\+true+, 1=\+false+, 2=(), 3=cons, 4=car, 5=cdr, 6=pair?, 7=symbol?, 8=eq?
    ;;((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (otherwise           (list 19 i))))

(defun DEEP-ARGUMENT-REF (i j)
  (list 6 i j))

(defun SET-SHALLOW-ARGUMENT! (j)
  (case j
    ;;((0 1 2 3) (list (+ 21 j)))
    (otherwise (list 25 j))))

(defun SET-DEEP-ARGUMENT! (i j)
  (list 26 i j))

(defun GLOBAL-REF (i)
  (list 7 i))

(defun CHECKED-GLOBAL-REF (i)
  (list 8 i))

(defun SET-GLOBAL! (i)
  (list 27 i))

(defun CONSTANT (value)
  (cond
    #+nil (((eq? value +true+)    (list 10))
	   ((eq? value +false+)    (list 11))
	   ((eq? value '())   (list 12))
	   ((equal? value -1) (list 80))
	   ((equal? value 0)  (list 81))
	   ((equal? value 1)  (list 82))
	   ((equal? value 2)  (list 83))
	   ((equal? value 4)  (list 84)))	
        ((and (integer? value)  ; immediate value
              (<= 0 value)
              (< value 255))
         (list 79 value))
        (t (EXPLICIT-CONSTANT value))))

(defun EXPLICIT-CONSTANT (value)
  (set! *quotations* (append *quotations* (list value)))
  (list 9 (- (length *quotations*) 1)))

;;; All gotos have positive offsets (due to the generation)

(defun GOTO (offset)
  (cond ((< offset 255) (list 30 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)))
           (list 28 offset1 offset2)))
        (t (static-wrong "too long jump" offset))))

(defun JUMP-FALSE (offset)
  (cond ((< offset 255) (list 31 offset))
        ((< offset (+ 255 (* 255 256))) 
         (let ((offset1 (modulo offset 256))
               (offset2 (quotient offset 256)))
           (list 29 offset1 offset2)))
        (t (static-wrong "too long jump" offset))))

(defun EXTEND-ENV ()
  (list 32))

(defun UNLINK-ENV ()
  (list 33))

(defun INVOKE0 (address)
  (case address
    ((read)    (list 89))
    ((newline) (list 88))
    (otherwise (static-wrong "Cannot integrate" address))))

#+nil
(defun INVOKE1 (address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    (otherwise (static-wrong "Cannot integrate" address))))

;;; The same one with other unary primitives.
(defun INVOKE1 (address)
  (case address
    ((car)     (list 90))
    ((cdr)     (list 91))
    ((pair?)   (list 92))
    ((symbol?) (list 93))
    ((display) (list 94))
    ((primitive?) (list 95))
    ((null?)   (list 96))
    ((continuation?) (list 97))
    ((eof-object?)   (list 98))
    (otherwise (static-wrong "Cannot integrate" address))))

(defun PUSH-VALUE ()
  (list 34)) 

(defun POP-ARG1 ()
  (list 35))

(defun INVOKE2 (address)
  (case address
    ((cons)     (list 100))
    ((eq?)      (list 101))
    ((set-car!) (list 102))
    ((set-cdr!) (list 103))
    ((+)        (list 104))
    ((-)        (list 105))
    ((=)        (list 106))
    ((<)        (list 107))
    ((>)        (list 108))
    ((*)        (list 109))
    ((<=)       (list 110))
    ((>=)       (list 111))
    ((remainder)(list 112))
    (otherwise (static-wrong "Cannot integrate" address))))

(defun POP-ARG2 ()
  (list 36))

(defun INVOKE3 (address)
  (static-wrong "No ternary integrated procedure" address))

(defun CREATE-CLOSURE (offset)
  (list 40 offset))

(defun ARITY=? (arity+1)
  (case arity+1
    ;;((1 2 3 4) (list (+ 70 arity+1)))
    (otherwise (list 75 arity+1))))

(defun %RET% ()
  (list 43))

(defun PACK-FRAME! (arity)
  (list 44 arity))

(defun ARITY>=? (arity+1)
  (list 78 arity+1))

(defun FUNCTION-GOTO ()
  (list 46))

(defun POP-FUNCTION ()
  (list 39))

(defun FUNCTION-INVOKE ()
  (list 45))

(defun PRESERVE-ENV ()
  (list 37))

(defun RESTORE-ENV ()
  (list 38))

(defun POP-FRAME! (rank)
  (case rank
    ;;((0 1 2 3) (list (+ 60 rank)))
    (otherwise (list 64 rank))))

(defun POP-CONS-FRAME! (arity) (list 47 arity))

(defun ALLOCATE-FRAME (size)
  (case size
    ;;((0 1 2 3 4) (list (+ 50 size)))
    (otherwise   (list 55 (+ size 1)))))

(defun ALLOCATE-DOTTED-FRAME (arity)
  (list 56 (+ arity 1)))

(defun FINISH ()
  (list 20))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Preserve the state of the machine ie the three environments.
(defun preserve-environment ()
  (stack-push *env*))
(defun restore-environment ()
  (set! *env* (stack-pop)))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
(defun fetch-byte ()
  (let ((byte (vector-ref *code* *pc*)))
    (incf *pc*)
    byte))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Disassemble code
(defun %disassemble% (code)
  (named-let rec ((result '())
		  (pc 0))
    (if (>= pc (vector-length code))
        (reverse! result)
        (rec (cons (instruction-decode code pc) result)
	     (+ pc (instruction-size code pc))))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; If tail? is +true+ then the return address is on top of stack so no
;;; need to push another one.
(defmethod invoke ((f t) tail?)
  (signal-exception +false+ (list "Not a function" f)))
(defmethod invoke ((f closure) tail?)
  (unless tail? (stack-push *pc*))
  (set! *env* (closure-closed-environment f))
  (set! *pc* (closure-code f)))
(defmethod invoke ((f primitive) tail?)
  (unless tail? (stack-push *pc*))
  (funcall (primitive-address f)))
(defmethod invoke ((f continuation) tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (begin
        (restore-stack (continuation-stack f))
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop)))
      (signal-exception +false+ (list "Incorrect arity" 'continuation))))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
(defmacro defprimitive0 (name value)
  `(definitial ,name
      (letrec ((arity+1 (+ 1 0))
	       (behavior
		(lambda ()
		  (if (= arity+1 (activation-frame-argument-length *val*))
		      (begin
		       (set! *val* (,value))
		       (set! *pc* (stack-pop)))
		      (signal-exception +true+ (list "Incorrect arity" ',name))))))
	(description-extend! ',name `(function ,',value))
	(make-primitive behavior))))
(defmacro defprimitive1 (name value)
  `(definitial ,name
       (letrec ((arity+1 (+ 1 1))
                (behavior
                 (lambda ()
                   (if (= arity+1 (activation-frame-argument-length *val*))
                       (let ((arg1 (activation-frame-argument *val* 0)))
                         (set! *val* (,value arg1))
                         (set! *pc* (stack-pop)))
                       (signal-exception +true+ (list "Incorrect arity" ',name))))))
         (description-extend! ',name `(function ,',value a))
         (make-primitive behavior))))
(defmacro defprimitive2 (name value)
  `(definitial ,name
     (letrec ((arity+1 (+ 2 1))
	      (behavior
	       (lambda ()
		 (if (= arity+1 (activation-frame-argument-length *val*))
		     (let ((arg1 (activation-frame-argument *val* 0))
			   (arg2 (activation-frame-argument *val* 1)))
		       (set! *val* (,value arg1 arg2))
		       (set! *pc* (stack-pop)))
		     (signal-exception +true+ (list "Incorrect arity" ',name))))))
       (description-extend! ',name `(function ,',value a b))
       (make-primitive behavior))))

(definitial t +true+)
(definitial f +false+)
(definitial nil '())
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
(defprimitive read read 0)
(defprimitive primitive? primitive? 1)
(defprimitive continuation? continuation? 1)
(defprimitive null? null? 1)
(defprimitive newline newline 0)
(defprimitive eof-object? eof-object? 1)

;;; The function which is invoked by call/cc always waits for an
;;; activation frame. 

(definitial call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)))
    (make-primitive
     (lambda ()
       (if (= arity+1 (activation-frame-argument-length *val*))
           (let ((f (activation-frame-argument *val* 0))
                 (frame (allocate-activation-frame (+ 1 1))))
             (set-activation-frame-argument! 
              frame 0 (make-continuation (save-stack)))
             (set! *val* frame)
             (set! *fun* f)             ; useful for debug
             (invoke f +true+))
           (signal-exception +true+ (list "Incorrect arity" 
                                      'call/cc)))))))

(definitial apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)))
    (make-primitive
     (lambda ()
       (if (>= (activation-frame-argument-length *val*) arity+1)
           (let* ((proc (activation-frame-argument *val* 0))
                  (args-number (activation-frame-argument-length *val*))
                  (last-arg-index (- args-number 2))
                  (last-arg (activation-frame-argument *val* last-arg-index))
                  (size (+ last-arg-index (length last-arg)))
                  (frame (allocate-activation-frame size)))
             (do ((i 1 (+ i 1)))
                 ((= i last-arg-index))
               (set-activation-frame-argument! 
                frame (- i 1) (activation-frame-argument *val* i)))
             (do ((i (- last-arg-index 1) (+ i 1))
                  (last-arg last-arg (cdr last-arg)))
                 ((null? last-arg))
               (set-activation-frame-argument! frame i (car last-arg)))
             (set! *val* frame)
             (set! *fun* proc)  ; useful for debug
             (invoke proc +true+))
           (signal-exception +false+ (list "Incorrect arity" 'apply)))))))

(definitial list
  (make-primitive
   (lambda ()
     (let ((args-number (- (activation-frame-argument-length *val*) 1))
           (result '()))
       (do ((i args-number (- i 1)))
           ((= i 0))
         (set! result (cons (activation-frame-argument *val* (- i 1)) 
                            result))) 
       (set! *val* result)
       (set! *pc* (stack-pop))))))

;;; Reserve some variables for future use in future chapters.
(defmacro defreserve (name)
  `(definitial ,name
       (make-primitive
	(lambda ()
	  (signal-exception +false+ (list "Not yet implemented" ',name))))))

(defreserve global-value)
(defreserve load)
(defreserve eval)
(defreserve eval/at)
(defreserve eval/b)
(defreserve enrich)
(defreserve procedure->environment)
(defreserve procedure->definition)
(defreserve variable-value)
(defreserve set-variable-value!)
(defreserve variable-defined?)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Use Meroon show functions to describe the inner working.

(defparameter *debug* +false+)

(defun show-registers (message)
  (when *debug* 
    (format +true+ "~%----------------~A" message)
    (format +true+ "~%ENV  = ")
    (show *env*)
    (format +true+ "~%VAL  = ")
    (show *val*)
    (format +true+ "~%FUN  = ")
    (show *fun*)
    (show-stack (save-stack))
    (format +true+
	    "~%(PC  = ~A), next INSTR to be executed = ~A~%" 
            *pc*
	    (instruction-decode *code* *pc*))))

(defun show-stack (stack)
  (let ((n (vector-length stack)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (format +true+ "~%STK[~A]= " i)
      (show (vector-ref *stack* i)))))

(defmethod show ((f t) &optional (stream *standard-output*))
  (format stream "~A" f))
(defmethod show ((f closure) &optional (stream *standard-output*))
  (format stream "#<Closure(pc=~A)>" (closure-code f)))
(defmethod show ((a activation-frame) &optional (stream *standard-output*))
  (display "[Frame next=" stream)
  (show (activation-frame-next a) stream)
  (display ", content=" stream)
  (do ((i 0 (+ 1 i)))
      ((= i (activation-frame-argument-length a)))
    (show (activation-frame-argument a i) stream)
    (display " & " stream))
  (display "]" stream))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(defun code-prologue ()
  (set! finish-pc 0)
  (FINISH))

(defun make-code-segment (m)
  (apply (function vector) (append (code-prologue) m (%RET%))))

(defun chapter7d-interpreter ()
  (labels ((toplevel ()
	     (display (funcall (stand-alone-producer7d (read)) 100))
	     (toplevel)))
    (toplevel))) 

(defun stand-alone-producer7d (e)
  (set! g.current (original.g.current))
  (set! *quotations* '())
  (let* ((code (make-code-segment (meaning e r.init +true+)))
         (start-pc (length (code-prologue)))
         (global-names (mapcar (function car) (reverse g.current)))
         (constants (apply (function vector) *quotations*)))
    (lambda (stack-size)
      (run-machine stack-size start-pc code 
                   constants global-names))))

(defun run-machine (stack-size pc code constants global-names)
  #+nil
  (when *debugging*
    (mapc (function print) (%disassemble% code)))
  (set! sg.current (make-vector (length global-names) 
                                undefined-value))
  (set! sg.current.names global-names)
  (set! *constants*   constants)
  (set! *code*        code)
  (set! *env*         sr.init)
  (set! *stack*       (make-vector stack-size))
  (set! *stack-index* 0)
  (set! *val*         'anything)
  (set! *fun*         'anything)
  (set! *arg1*        'anything)
  (set! *arg2*        'anything)
  (stack-push finish-pc)                ;  pc for FINISH
  (set! *pc*          pc)
  (call/cc
   (lambda (exit)
     (set! *exit* exit)
     (run))))

;;; Patch run to show registers in debug mode.

(let ((native-run (function run)))
  (setf (symbol-function 'run)
	(lambda ()
	  (when *debug* (show-registers ""))
	  (funcall native-run))))
(let ((native-run-machine (function run-machine)))
  (setf (symbol-function 'run-machine)
        (lambda (stack-size pc code constants global-names)
          (when *debug*                     ; DEBUG
            (format +true+ "Code= ~A~%" (%disassemble% code)))         
          (funcall native-run-machine stack-size pc code constants global-names))))
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Tests

(defun %eval (form)
  (funcall (stand-alone-producer7d form) 100)
  *val*)

(defun scheme7d ()
  (interpreter
   "Scheme? "  
   "Scheme= " 
   +true+
   (lambda (read print error)
     (setup-wrong-functions error)
     (lambda ()
       (funcall (stand-alone-producer7d (funcall read)) 100)
       (funcall print *val*)))))

(defun test-scheme7d (&optional (file *scheme-test-file*))
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   +true+
   (lambda (read check error)
     (setup-wrong-functions error)
     (lambda ()
       (funcall (stand-alone-producer7d (funcall read)) 100)
       (funcall check *val*)))
   (function equal?)))

(defun setup-wrong-functions (error)
  (setf (symbol-function 'signal-exception)
	(lambda (c &rest args)
	  #+nil
	  (declare (ignorable c))
	  (apply error args)))
  (setf (symbol-function 'wrong)
	(lambda (&rest args)
	  (format +true+ "
		>>>>>>>>>>>>>>>>>>RunTime PANIC<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" (activation-frame-argument *val* 1))
	  (apply error args)))
  (setf (symbol-function 'static-wrong)
	(lambda (&rest args)
	  (format +true+ "
		>>>>>>>>>>>>>>>>>>Static WARNING<<<<<<<<<<<<<<<<<<<<<<<<<
		~A~%" args)
	  (apply error args))))

;;; Missing global variables

(defparameter signal-exception 'wait)
(defparameter finish-pc 'wait)
(defparameter *exit* 'wait)

;;; end of chap7d.scm
