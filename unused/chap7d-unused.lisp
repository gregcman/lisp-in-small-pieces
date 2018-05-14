#+nil
(define-class primitive Object
  (address))

#+nil
(define-class continuation Object
  (stack))

#+nil
(progn
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
	    (INVOKE3 address))))

(defun SHALLOW-ARGUMENT-REF (j)
  (check-byte j)
  (case j
    ((0 1 2 3) (list (+ 1 j)))
    (otherwise (list 5 j))))

(defun PREDEFINED (i)
  (check-byte i)
  (case i
    ;; 0=\+true+, 1=\+false+, 2=(), 3=cons, 4=car, 5=cdr, 6=pair?, 7=symbol?, 8=eq?
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (otherwise           (list 19 i))))

(defun SET-SHALLOW-ARGUMENT! (j)
  (case j
    ((0 1 2 3) (list (+ 21 j)))
    (otherwise (list 25 j))))

(defun CONSTANT (value)
  (cond
    ((eq? value +true+)    (list 10))
    ((eq? value +false+)    (list 11))
    ((eq? value '())   (list 12))
    ((equal? value -1) (list 80))
    ((equal? value 0)  (list 81))
    ((equal? value 1)  (list 82))
    ((equal? value 2)  (list 83))
    ((equal? value 4)  (list 84))	
    ((and (integer? value)  ; immediate value
	  (<= 0 value)
	  (< value 255))
     (list 79 value))
    (t (EXPLICIT-CONSTANT value))))

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

(defun INVOKE3 (address)
  (static-wrong "No ternary integrated procedure" address))

(defun ARITY=? (arity+1)
  (case arity+1
    ((1 2 3 4) (list (+ 70 arity+1)))
    (otherwise (list 75 arity+1))))

(defun POP-FRAME! (rank)
  (case rank
    ((0 1 2 3) (list (+ 60 rank)))
    (otherwise (list 64 rank))))

(defun ALLOCATE-FRAME (size)
  (case size
    ((0 1 2 3 4) (list (+ 50 size)))
    (otherwise   (list 55 (+ size 1)))))
