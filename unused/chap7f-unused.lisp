
#+nil
(progn
  (define-instruction (SHALLOW-ARGUMENT-REF0) 1 
    (set! *val* (activation-frame-argument *env* 0)))
  (define-instruction (SHALLOW-ARGUMENT-REF1) 2 
    (set! *val* (activation-frame-argument *env* 1)))
  (define-instruction (SHALLOW-ARGUMENT-REF2) 3 
    (set! *val* (activation-frame-argument *env* 2)))
  (define-instruction (SHALLOW-ARGUMENT-REF3) 4 
    (set! *val* (activation-frame-argument *env* 3))))

#+nil
(progn
  (define-instruction (PREDEFINED0) 10   ; \+true+ 
    (set! *val* +true+))
  (define-instruction (PREDEFINED1) 11   ; \+false+ 
    (set! *val* +false+))
  (define-instruction (PREDEFINED2) 12   ; () 
    (set! *val* '()))
  (define-instruction (PREDEFINED3) 13   ; cons 
    (set! *val* (predefined-fetch 3)))
  (define-instruction (PREDEFINED4) 14   ; car 
    (set! *val* (predefined-fetch 4)))
  (define-instruction (PREDEFINED5) 15   ; cdr
    (set! *val* (predefined-fetch 5)))
  (define-instruction (PREDEFINED6) 16   ; pair? 
    (set! *val* (predefined-fetch 6)))
  (define-instruction (PREDEFINED7) 17   ; symbol? 
    (set! *val* (predefined-fetch 7)))
  (define-instruction (PREDEFINED8) 18   ; eq? 
    (set! *val* (predefined-fetch 8))))

#+nil
(progn
  (define-instruction (SET-SHALLOW-ARGUMENT!0) 21 
    (set-activation-frame-argument! *env* 0 *val*))
  (define-instruction (SET-SHALLOW-ARGUMENT!1) 22 
    (set-activation-frame-argument! *env* 1 *val*))
  (define-instruction (SET-SHALLOW-ARGUMENT!2) 23 
    (set-activation-frame-argument! *env* 2 *val*))
  (define-instruction (SET-SHALLOW-ARGUMENT!3) 24 
    (set-activation-frame-argument! *env* 3 *val*)))

#+nil
(progn
  (define-instruction (ALLOCATE-FRAME1) 50 
    (set! *val* (allocate-activation-frame 1)))
  (define-instruction (ALLOCATE-FRAME2) 51 
    (set! *val* (allocate-activation-frame 2)))
  (define-instruction (ALLOCATE-FRAME3) 52 
    (set! *val* (allocate-activation-frame 3)))
  (define-instruction (ALLOCATE-FRAME4) 53 
    (set! *val* (allocate-activation-frame 4)))
  (define-instruction (ALLOCATE-FRAME5) 54
    (set! *val* (allocate-activation-frame 5))))

#+nil
(progn
  (define-instruction (POP-FRAME!0) 60 
    (set-activation-frame-argument! *val* 0 (stack-pop)))
  (define-instruction (POP-FRAME!1) 61 
    (set-activation-frame-argument! *val* 1 (stack-pop)))
  (define-instruction (POP-FRAME!2) 62 
    (set-activation-frame-argument! *val* 2 (stack-pop)))
  (define-instruction (POP-FRAME!3) 63 
    (set-activation-frame-argument! *val* 3 (stack-pop))))

#+nil
(progn
  (define-instruction (ARITY=?1) 71 
    (unless (= (activation-frame-argument-length *val*) 1)
      (signal-exception +false+ (list "Too much arguments for a thunk"))))
  (define-instruction (ARITY=?2) 72 
    (unless (= (activation-frame-argument-length *val*) 2)
      (signal-exception 
       +false+ (list "Incorrect arity for unary function"))))
  (define-instruction (ARITY=?3) 73 
    (unless (= (activation-frame-argument-length *val*) 3)
      (signal-exception +false+ (list "Incorrect arity for binary function"))))
  (define-instruction (ARITY=?4) 74 
    (unless (= (activation-frame-argument-length *val*) 4)
      (signal-exception +false+ (list "Incorrect arity for ternary function")))))

#+nil
(progn
  (define-instruction (CONSTANT-1)  80 
    (set! *val* -1))
  (define-instruction (CONSTANT0) 81 
    (set! *val* 0))
  (define-instruction (CONSTANT1) 82 
    (set! *val* 1))
  (define-instruction (CONSTANT2) 83 
    (set! *val* 2))
  (define-instruction (CONSTANT4) 84 
    (set! *val* 4)))

#+nil
(progn
  (define-instruction (CALL0-newline) 88
    (set! *val* (newline)))
  (define-instruction (CALL0-read) 89
    (set! *val* (read)))
  (define-instruction (CALL1-car) 90 
    (set! *val* (car *val*)))
  (define-instruction (CALL1-cdr) 91 
    (set! *val* (cdr *val*)))
  (define-instruction (CALL1-pair?) 92 
    (set! *val* (pair? *val*)))
  (define-instruction (CALL1-symbol?) 93 
    (set! *val* (symbol? *val*)))
  (define-instruction (CALL1-display) 94 
    (set! *val* (show *val*)))
  (define-instruction (CALL1-primitive?) 95
    (set! *val* (primitive? *val*)))
  (define-instruction (CALL1-null?) 96
    (set! *val* (null? *val*)))
  (define-instruction (CALL1-continuation?) 97
    (set! *val* (continuation? *val*)))
  (define-instruction (CALL1-eof-object?) 98
    (set! *val* (eof-object? *val*)))
  (define-instruction (CALL2-cons) 100 
    (set! *val* (cons *arg1* *val*)))
  (define-instruction (CALL2-eq?) 101 
    (set! *val* (eq? *arg1* *val*)))
  (define-instruction (CALL2-set-car!) 102 
    (set! *val* (set-car! *arg1* *val*)))
  (define-instruction (CALL2-set-cdr!) 103 
    (set! *val* (set-cdr! *arg1* *val*)))
  (define-instruction (CALL2-+) 104 
    (set! *val* (+ *arg1* *val*)))
  (define-instruction (CALL2--) 105 
    (set! *val* (- *arg1* *val*)))
  (define-instruction (CALL2-=) 106 
    (set! *val* (= *arg1* *val*)))
  (define-instruction (CALL2-<) 107 
    (set! *val* (< *arg1* *val*)))
  (define-instruction (CALL2->) 108 
    (set! *val* (> *arg1* *val*)))
  (define-instruction (CALL2-*) 109 
    (set! *val* (* *arg1* *val*)))
  (define-instruction (CALL2-<=) 110 
    (set! *val* (<= *arg1* *val*)))
  (define-instruction (CALL2->=) 111 
    (set! *val* (>= *arg1* *val*)))
  (define-instruction (CALL2-remainder) 112 
    (set! *val* (remainder *arg1* *val*))))

(define-instruction (DYNAMIC-REF index) 240
  (set! *val* (find-dynamic-value index)))

(define-instruction (DYNAMIC-POP) 241 
  (pop-dynamic-binding))

(define-instruction (DYNAMIC-PUSH index) 242 
  (push-dynamic-binding index *val*))

(define-instruction (NON-CONT-ERR) 245 
  (signal-exception +false+ (list "Non continuable exception continued")))

(define-instruction (PUSH-HANDLER) 246 
  (push-exception-handler))

(define-instruction (POP-HANDLER) 247 
  (pop-exception-handler))

(define-instruction (POP-ESCAPER) 250 
  (let* ((tag (stack-pop))
         (escape (stack-pop)))
    (restore-environment)))

(define-instruction (PUSH-ESCAPER offset) 251 
  (preserve-environment)
  (let* ((escape (make-escape (+ *stack-index* 3)))
         (frame (allocate-activation-frame 1)))
    (set-activation-frame-argument! frame 0 escape)
    (set! *env* (sr-extend* *env* frame))
    (stack-push escape)
    (stack-push escape-tag)
    (stack-push (+ *pc* offset))))

;;; Used by chap8d.scm (eval as a special form)

(define-instruction (COMPILE-RUN) 255
  (let ((v *val*)
        (r (stack-pop)))
    (if (program? v)
        (compile-and-run v r +false+)
        (signal-exception +true+ (list "Illegal program" v)))))

;;; Used by chap8h.scm (export special form)

(define-instruction (CREATE-1ST-CLASS-ENV) 254
  (create-first-class-environment *val* *env*))

(define-instruction (CHECKED-DEEP-REF i j) 253
  (set! *val* (deep-fetch *env* i j))
  (when (eq? *val* undefined-value)
    (signal-exception +true+ (list "Uninitialized local variable"))))

(define-instruction (CREATE-PSEUDO-ENV) 252
  (create-pseudo-environment (stack-pop) *val* *env*))

(define-instruction (SHADOW-REF i j) 231
  (shadowable-fetch *env* i j))

(define-instruction (SET-SHADOW! i j) 232
  (shadowable-update! *env* i j *val*))

;;; end of chap7f.scm
