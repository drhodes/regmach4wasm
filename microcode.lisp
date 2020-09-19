;;;; microcode.lisp
(in-package :regmach4wasm)

;; -----------------------------------------------------------------------------
;; mcvm

(defstruct mcvm regfile (pc 0))

(defun mcvm-reset (mcvm)
  (setf (mcvm-regfile mcvm) (make-regfile))
  (setf (mcvm-pc mcvm) 0)
  mcvm)

(defun new-mcvm () (mcvm-reset (make-mcvm)))

(new-mcvm)

(defun mcvm-set-reg (mcvm reg val)
  (regfile-set-reg (mcvm-regfile mcvm) reg val))

(defun mcvm-get-reg (mcvm reg)
  (regfile-get-reg (mcvm-regfile mcvm) reg))

(defun exec-add (mcvm inst) 'add)
(defun exec-sub (mcvm inst) 'sub)

(defun exec-one (mcvm inst)
  (case (car inst)
    ('add (exec-add mcvm inst))
    ('sub (exec-sub mcvm inst))
    ))

(exec-one (make-mcvm) '(add r0 r1 r2))
(exec-one (make-mcvm) '(sub r0 r1 r2))

;; -----------------------------------------------------------------------------
;; instructions
;;
;; instructions are lists. The first item of the list is the opcode,
;; the rest of the list are arguments.

;; -----------------------------------------------------------------------------
;; microcode language

;; this simple language doesn't have functions or recursion here is an
;; example of it executing within the context of the microcode
;; evaluator with an implicit instruction present.

;; (CMPLT RA RB RC)
;; '((set-pc (+ (get-pc) 4))
;;   (if (< (get-reg ra) (get-reg rb))
;;       (set-reg rc 1)
;;       (set-reg rc 0)))


(defvar int-registers '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9
                        r10 r11 r12 r13 r14 r15 r16 r17 r18 r19
                        r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31))

(defun eval-set-pc (mcvm inst)
  (setf (mcvm-pc mcvm)
        (eval-mc mcvm (cadr inst))))

;; -----------------------------------------------------------------------------
;; instructions

;;
;; all instruction documentation copied from 6.004 β documentation
;;

(defun instruction-lookup (opcode)
  (let ((mc-prog (gethash opcode *instructions*)))
    (if mc-prog mc-prog
        (error (format nil "Failed to locate opcode in *instruction* table: ~a" opcode)))))

(defun reg-from-inst (mcvm inst)
  "Remember there are two types of instructions OP and OPC."
  (cond
    ((jmp? inst) (jmp-ra inst))
    
    )
  ;; (ADD ra rb rc)
  ;; (ADDC ra lit rc)
  )

(defun reg-to-num (reg)
  "Register to number, converts symbol of format r15 to 15"
  (check-type reg symbol)
  (case reg
    (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7) (r8 8) (r9 9)
    (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15) (r16 16) (r17 17) (r18 18) (r19 19)
    (r20 20) (r21 21) (r22 22) (r23 23) (r24 24) (r25 25) (r26 26) (r27 27) (r28 28) (r29 29)
    (r30 30) (r31 31)
    (otherwise (error (format nil "unknown register: ~a, expecting r0 - r31" reg)))))

(defun eval-set-reg (mcvm inst)
  (let ((reg (cadr inst))
        (val (caddr inst)))
    (check-type reg symbol)    
    (mcvm-set-reg mcvm (reg-to-num reg) (eval-mc mcvm val))))

(defun instruction-arguments (inst) (cdr inst))

(defun eval-set-var (mcvm expr)
  (let ((name (cadr expr))
        (val (caddr expr)))
    (check-type name symbol)
    ;; need a symbol table   
    
    ))

(defun eval-op (mcvm op inst)
  (apply '+ (mapcar (lambda (expr) (eval-mc mcvm expr))
                    (instruction-arguments inst))))

(defun register-p (reg)
  (if (member reg int-registers) t nil)) ;; return bool

(defun eval-inc-pc (vm)
  (eval-mc vm '(set-pc (+ pc 4))))

(defun eval-get-pc (vm) (mcvm-pc vm))


;; (CMPLT RA RB RC)
;; '((set-pc (+ (get-pc) 4))
;;   (if (< (get-reg ra) (get-reg rb))
;;       (set-reg rc 1)
;;       (set-reg rc 0)))

;; this is not the assembler!
;; the beta emulator uses this.  So the mcvm
;; will probably need a copy of the symbol table.

(defun eval-mc (vm expr)
  ;; ((set-pc (+ pc 4))
  ;;  (set-reg rc (& ra rb)))
  (cond ((numberp expr) expr)
        ;; symbol table         
        ((listp expr) (case (car expr)        
                        (+ (eval-op vm + expr))
                        (- (eval-op vm - expr))
                        (* (eval-op vm * expr))
                        (/ (eval-op vm / expr))
                        (mod (eval-op vm mod expr))
                        (inc-pc (eval-inc-pc vm))
                        (set-pc (eval-set-pc vm expr))
                        (set-var (eval-set-var vm expr))
                        (set-reg (eval-set-reg vm expr))
                        (reg (reg-from-inst vm expr))
                        ))
        ((register-p expr) (reg-to-num expr))
        ((eq 'pc expr) (eval-get-pc vm))
        (t (error (format nil "unhandled case in eval-mc: ~a" expr)))))

(defun eval-mc-prog (mcvm prog)
  (progn (mapcar (lambda (inst) (eval-mc mcvm inst)) prog)
         mcvm))

;; -----------------------------------------------------------------------------
;; tests


;; test utility.
(defun eval-mc-prog-with (prog checker)
  "prog is an mcvm program, checker is function that takes a vm"
  (let ((vm (new-mcvm)))
    (eval-mc-prog vm prog)
    (apply checker (list vm))))

(eval-mc-prog (new-mcvm)
              `((set-pc 1)
                (set-pc (+ 1 1))
                (set-reg r1 42)
                (set-pc (+ 1 2))))

;; (eval-mc (new-mcvm) '(set-reg r0 43))
;; (eval-mc (new-mcvm) '(set-pc 1))

(eval-mc-prog-with
 '((inc-pc))
 (lambda (vm)
   (expected 4 (mcvm-pc vm))))

(eval-mc-prog-with
 '((set-reg r0 42))
 (lambda (vm)
   (expected 42 (mcvm-get-reg vm 0))))




;; (CMPLT RA RB RC)
;; '((set-pc (+ (get-pc) 4))
;;   (if (< (reg ra) (reg rb))
;;       (set-reg rc 1)
;;       (set-reg rc 0)))
