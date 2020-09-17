;;;; regmach4wasm.lisp

(in-package #:regmach4wasm)

;; -----------------------------------------------------------------------------
;; vm

;; (defstruct vm regfile (pc 0))

;; (defun new-vm () (vm-reset (make-vm)))

;; (defun vm-set-reg (vm reg val) (regfile-set-reg (vm-regfile vm) reg val))

;; (defun vm-reset (vm)
;;   (setf (vm-regfile vm) (make-regfile))
;;   (setf (vm-pc vm) 0)
;;   vm)

;; (defun exec-add (vm inst) 'add)
;; (defun exec-sub (vm inst) 'sub)

;; (defun exec-one (vm inst)
;;   (case (car inst)
;;     ('add (exec-add vm inst))
;;     ('sub (exec-sub vm inst))
;;     ))

;; (exec-one (make-vm) '(add r0 r1 r2))
;; (exec-one (make-vm) '(sub r0 r1 r2))

;; '((reset)
  
;;   label: 
;;   (heap-32 :heap1) ;; uses 
;;   (add f0 f1 f2)
;;   (add r1 r2 r3)
;;   (mv f0 r4 )
;;   )


;; -----------------------------------------------------------------------------
;; instructions

;; instructions are lists. The first item of the list is the opcode,
;; the rest of the list are arguments.


;; -----------------------------------------------------------------------------
;; microcode language


;; (defun eval-set-pc (vm inst)
;;   (setf (vm-pc vm)
;;         (eval-mc vm (cadr inst))))

;; (defun reg-to-num (reg)
;;   (check-type reg symbol)
;;   (case reg
;;     (r0 0) (r1 1) (r2 2) (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7) (r8 8) (r9 9)
;;     (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15) (r16 16) (r17 17) (r18 18) (r19 19)
;;     (r20 20) (r21 21) (r22 22) (r23 23) (r24 24) (r25 25) (r26 26) (r27 27) (r28 28) (r29 29)
;;     (r30 30) (r31 31)
;;     (otherwise (error (format nil "unknown register: ~a, expecting r0 - r31" reg)))))

;; (defun eval-set-reg (vm inst)
;;   (let ((reg (cadr inst))
;;         (val (caddr inst)))
;;     (check-type reg symbol)    
;;     (vm-set-reg vm (reg-to-num reg) (eval-mc vm val))))

;; (defun instruction-arguments (inst) (cdr inst))

;; (defun eval-op (vm op inst)
;;   (apply '+ (mapcar (lambda (expr) (eval-mc vm expr))
;;                     (instruction-arguments inst))))

;; (defvar int-registers '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9
;;                         r10 r11 r12 r13 r14 r15 r16 r17 r18 r19
;;                         r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31))

;; (defun register-p (reg)
;;   (if (member reg int-registers) t nil)) ;; return bool

;; (defun label? (expr) (keywordp expr))


;; ;; this is not the assembler!
;; ;; the beta emulator uses this.
;; ;;

;; (defun eval-mc (vm expr)
;;   ;; ((set-pc (+ pc 4))
;;   ;;  (set-reg rc (& ra rb)))
;;   (cond ((numberp expr) expr)
;;         ((label? expr) (eval-label expr))        
;;         ;; FANTASTIC. untagged labels encountered on the first pass
;;         ;; should be replaced with (label-pass-2 :keyword)
;;         ;; or maybe (pass2 :keyword)
;;         ;; symbol table         
;;         ((listp expr) (case (car expr)        
;;                         (+ (eval-op vm + expr))
;;                         (- (eval-op vm - expr))
;;                         (* (eval-op vm * expr))
;;                         (/ (eval-op vm / expr))
;;                         (set-pc (eval-set-pc vm expr))
;;                         (set-reg (eval-set-reg vm expr))))
;;         ((register-p expr) (reg-to-num expr))        
;;         (t (error (format nil "unhandled case in eval-mc: ~a" expr)))))


;; (defun eval-mc-prog (vm prog)
;;   (progn (mapcar (lambda (inst) (eval-mc vm inst)) prog)
;;          vm))

;; (eval-mc-prog (new-vm)
;;               `((set-pc 1)
;;                 (set-pc (+ 1 1))
;;                 (set-reg r1 42)
;;                 (set-pc (+ 1 2))))

;; (eval-mc (new-vm) '(set-reg r0 43))
;; (eval-mc (new-vm) '(set-pc 1))

;; (progn
;;   (setq vm (new-vm))
;;   (eval-mc vm '(set-reg r0 43))
;;   vm
;;   )

