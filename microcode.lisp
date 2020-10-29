;;;; microcode.lisp
(in-package :regmach4wasm)
(declaim (optimize (debug 3)))

;; -----------------------------------------------------------------------------
;; mcvm

(defstruct mcvm
  (regfile (make-regfile))
  (pc 0)
  (memory (make-ram)))

(defun mcvm-reset (mcvm)
  (setf (mcvm-regfile mcvm) (make-regfile))
  (setf (mcvm-pc mcvm) 0)
  (setf (mcvm-memory mcvm) (make-ram))
  mcvm)

(defun mcvm-fmt-ram (mcvm) (ram-fmt (mcvm-memory mcvm)))

(defun mcvm-ram-size (mcvm)  
  (ram-size (mcvm-memory mcvm)))

(defun new-mcvm () (mcvm-reset (make-mcvm)))

(defun mcvm-ram-set (mcvm addr byte)
  (ram-set (mcvm-memory mcvm) addr byte))

(defun mcvm-ram-get (mcvm addr)
  (ram-get (mcvm-memory mcvm) addr))

(defun mcvm-load-list (mcvm byte-list)
  (ram-load-list (mcvm-memory mcvm) byte-list))

(defun mcvm-fetch-inst (vm)
  ;; grab 4 bytes starting from PC
  (let ((pc (mcvm-pc vm)))    
    (list (mcvm-ram-get vm (+ pc 0))
          (mcvm-ram-get vm (+ pc 1))
          (mcvm-ram-get vm (+ pc 2))
          (mcvm-ram-get vm (+ pc 3)))))

(defun mcvm-set-reg (mcvm reg val)
  (check-type reg number)
  (regfile-set-reg (mcvm-regfile mcvm) reg val))

(defun mcvm-get-reg (mcvm reg)
  (regfile-get-reg (mcvm-regfile mcvm) reg))

;; -----------------------------------------------------------------------------
;; microcode language

;; this simple language doesn't have functions or recursion here is an
;; example of it executing within the context of the microcode
;; evaluator with an implicit instruction present.

;; (CMPLT RA RB RC)
;; '((set-pc (+ (get-pc) 4))
;;   (if (< (reg ra) (reg rb))
;;       (set-reg rc 1)
;;       (set-reg rc 0)))

(defun eval-set-pc (vm env inst)
  (setf (mcvm-pc vm)
        (eval-mc vm env (eval-mc vm env (cadr inst)))))

;; -----------------------------------------------------------------------------
;; instructions
;;
;; all instruction documentation copied from 6.004 β documentation
;;

(defun eval-set-reg (vm env inst)
  (let ((reg (cadr inst))
        (val (caddr inst)))
    (mcvm-set-reg vm reg (eval-mc vm env val))))

(defun instruction-arguments (inst) (cdr inst))

(defun eval-set-var (vm env expr)
  (let ((name (cadr expr))
        (val (caddr expr)))
    (check-type name symbol)
    ;; need a symbol table   
    (env-put env name (eval-mc vm env val))))

(defun eval-op (vm env op inst)
  (apply op (mapcar (lambda (expr) (eval-mc vm env expr))
                    (instruction-arguments inst))))

(defun register-p (reg)
  (if (member reg int-registers) t nil)) ;; return bool

(defun eval-inc-pc (vm env)
  (eval-mc vm env '(set-pc (+ pc 4))))

(defun eval-get-pc (vm) (mcvm-pc vm))

(defun eval-instruction (vm env expr)
  ;; expr is an instruction in this form (ADD r1 r2 r3)
  ;; grab the instruction opcode
  (let* ((opcode (car expr))
         ;; lookup the instruction 
         (inst (instruction-lookup opcode))
         (microcode (instruction-microcode inst))
         (register-names (cdr (instruction-pattern inst)))
         (values (cdr expr)))

    ;; check to see if the instruction is well formed.
    (when (not (eq (length register-names) (length values)))
      (error "instruction pattern mismatch, wrong numbers of instruction values"))
    
    ;; bind the instruction values to register names.
    (mapcar (lambda (pair) (env-put env (car pair) (cadr pair)))
            (zip register-names values))
    
    ;; run the microcode
    (mapcar (lambda (stmt) (eval-mc vm env stmt))
            (bind-vars env microcode))))

(defun eval-get-reg (vm expr)  
  (mcvm-get-reg vm (cadr expr)))

(defun eval-sign-extend (vm env expr)
  ;; (sign-extend abc)
  (let ((x (eval-mc env vm (cadr expr))))
    (check-type x number)
    x))

(defun eval-mc (vm env expr)
  (format t "eval-mc: ~a ~%" expr)
  (cond ((numberp expr) expr) 
        ;;((match-instruction? expr) (eval-instruction vm env expr))
        ((listp expr) (case (car expr)        
                        (+ (eval-op vm env #'+ expr))
                        (- (eval-op vm env #'- expr))
                        (* (eval-op vm env #'* expr))
                        (/ (eval-op vm env #'/ expr))
                        (mod (eval-op vm env #'mod expr))
                        (inc-pc (eval-inc-pc vm env))
                        (set-pc (eval-set-pc vm env expr))
                        (set-var (eval-set-var vm env expr))
                        (set-reg (eval-set-reg vm env expr))
                        (reg (eval-get-reg vm expr))
                        (sign-extend (eval-sign-extend vm env expr))
                        )) 
        ((eq 'pc expr) (eval-get-pc vm))
        ((symbolp expr) (symbol-table-get env expr))
        (t (error (format nil "unhandled case in eval-mc: ~a" expr)))))

(defun eval-mc-prog (vm prog)
  (progn (mapcar (lambda (stmt) (eval-mc vm (make-symbol-table) stmt)) prog)
         vm))

;; -----------------------------------------------------------------------------
;; tests

;; test utility.
(defun eval-mc-prog-with (prog checker)
  "prog is an mcvm program, checker is function that takes a vm"
  (let ((vm (new-mcvm)))
    (eval-mc-prog vm prog)
    (apply checker (list vm))))

(eval-mc-prog-with
 '((set-pc 42))
 (lambda (vm)
   (expected 42 (mcvm-pc vm))))

(eval-mc-prog-with
 '((inc-pc))
 (lambda (vm)
   (expected 4 (mcvm-pc vm))))

(eval-mc-prog-with
 '((set-reg 0 42))
 (lambda (vm)
   (expected 42 (mcvm-get-reg vm 0))))


