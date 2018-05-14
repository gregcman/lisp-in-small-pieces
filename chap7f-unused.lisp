
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
