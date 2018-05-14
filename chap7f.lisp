(in-package :lisp)

;;; Instruction set.
;;; This file is read by chap7d.scm
(define-instruction (SHALLOW-ARGUMENT-REF j) 5 
  (set! *val* (activation-frame-argument *env* j)))

(define-instruction (DEEP-ARGUMENT-REF i j) 6 
  (set! *val* (deep-fetch *env* i j)))

(define-instruction (GLOBAL-REF i) 7 
  (set! *val* (global-fetch i)))

;(define-instruction (CHECKED-GLOBAL-REF i) 8 
;  (set! *val* (global-fetch i))
;  (if (eq? *val* undefined-value)
;    (signal-exception +true+ (list "Uninitialized global variable" i))
;    (vector-set! *code* (- *pc* 2) 7))) 

(define-instruction (CHECKED-GLOBAL-REF i) 8 
  (set! *val* (global-fetch i))
  (when (eq? *val* undefined-value)
    (signal-exception 
     +true+ (list "Uninitialized global variable" i))))

(define-instruction (CONSTANT i) 9 
  (set! *val* (quotation-fetch i)))

(define-instruction (PREDEFINED i) 19 
  (set! *val* (predefined-fetch i)))

(define-instruction (FINISH) 20 
  (funcall *exit* *val*))

(define-instruction (SET-SHALLOW-ARGUMENT! j) 25 
  (set-activation-frame-argument! *env* j *val*))

(define-instruction (SET-DEEP-ARGUMENT! i j) 26 
  (deep-update! *env* i j *val*))

(define-instruction (SET-GLOBAL! i) 27 
  (global-update! i *val*))

(define-instruction (LONG-GOTO offset1 offset2) 28 
  (let ((offset (+ offset1 (* 256 offset2))))
    (incf *pc* offset)))

(define-instruction (LONG-JUMP-FALSE offset1 offset2) 29 
  (let ((offset (+ offset1 (* 256 offset2))))
    (when (not *val*)
      (incf *pc* offset))))

(define-instruction (SHORT-GOTO offset) 30 
  (incf *pc* offset))

(define-instruction (SHORT-JUMP-FALSE offset) 31 
  (when (not *val*)
    (incf *pc* offset)))

(define-instruction (EXTEND-ENV) 32 
  (set! *env* (sr-extend* *env* *val*)))

(define-instruction (UNLINK-ENV) 33 
  (set! *env* (activation-frame-next *env*)))

(define-instruction (PUSH-VALUE) 34 
  (stack-push *val*))

(define-instruction (POP-ARG1) 35 
  (set! *arg1* (stack-pop)))

(define-instruction (POP-ARG2) 36 
  (set! *arg2* (stack-pop)))

(define-instruction (PRESERVE-ENV) 37 
  (preserve-environment))

(define-instruction (RESTORE-ENV) 38 
  (restore-environment))

(define-instruction (POP-FUNCTION) 39 
  (set! *fun* (stack-pop)))

(define-instruction (CREATE-CLOSURE offset) 40 
  (set! *val* (make-closure (+ *pc* offset) *env*)))

(define-instruction (%RET%) 43 
  (set! *pc* (stack-pop)))

(define-instruction (PACK-FRAME! arity) 44 
  (listify! *val* arity))

(define-instruction (FUNCTION-INVOKE) 45 
  (invoke *fun* +false+))

(define-instruction (FUNCTION-GOTO) 46 
  (invoke *fun* +true+))

(define-instruction (POP-CONS-FRAME! arity) 47 
  (set-activation-frame-argument! 
   *val* arity (cons (stack-pop)
                     (activation-frame-argument *val* arity))))

(define-instruction (ALLOCATE-FRAME size+1) 55
  (set! *val* (allocate-activation-frame size+1)))
(define-instruction (ALLOCATE-DOTTED-FRAME arity) 56 
  (let ((v* (allocate-activation-frame arity)))
    (set-activation-frame-argument! v* (- arity 1) '())
    (set! *val* v*)))

(define-instruction (POP-FRAME! rank) 64 
  (set-activation-frame-argument! *val* rank (stack-pop)))

(define-instruction (ARITY=? arity+1) 75 
  (unless (= (activation-frame-argument-length *val*) arity+1)
    (signal-exception +false+ (list "Incorrect arity"))))
(define-instruction (ARITY>=? arity+1) 78 
  (unless (>= (activation-frame-argument-length *val*) arity+1)
    (signal-exception +false+ (list "Too less arguments for a nary function"))))

(define-instruction (SHORT-NUMBER value) 79 
  (set! *val* value))

(define-instruction-set)
