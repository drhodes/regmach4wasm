;; a simple systems language for the beta.
(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

;; https://stackoverflow.com/questions/211717
(defun make-keyword (name) (values (intern name "KEYWORD")))
(defun small-integer? (n) (and (>= n -32768) (<= n 32767)))

;; need to maintain scopes in the environment? maybe not.
(defun sys-label (n &optional name) (make-keyword
                           (if name
                               (format nil "L~a_~a" name n)
                               (format nil "L~a" n))))

;; generate labels from a nonce.
(defun sys-gen-label (env &optional name)
  (let ((n (env-get env 'label-nonce)))
    (env-put env 'label-nonce (+ n 1))
    (sys-label n name)))

(defun eval-sys-var-decl (env expr)
  (check-type env environment)
  ;; initialize a variable without value
  ;; TODO handle the case where there is an initial value.
  (let* ((varname (caddr expr))
         (label (sys-gen-label env varname))
         (instructions `((pre (,label '(LONG 0)))))
         (register nil))
    (env-put env varname label)
    (cons register instructions)))

(defun eval-sys-var-expr (env expr)
  (check-type env environment)
  (check-type expr symbol)
  )

(defun eval-sys-constant (env n) (check-type env environment) (check-type n integer)
  (let ((register (sys-next-free-reg env)))
    (if (small-integer? n) ;; if small integer then move n into a ;; register
        (cons register `((cmove ,n ,register)))
        ;; otherwise, use LD instruction
        (let* ((label (sys-gen-label env))          
               (instructions`((ld ,label ,register)
                              (static ,label)
                              (long ,n))))
          (cons register instructions)))))

(defun make-sys-env ()
  (let ((env (make-environment)))
    (env-put env 'label-nonce 0)
    (env-put env 'cur-reg 0)
    env))

(defparameter *register-limit* 26)

(defun sys-get-cur-reg (env) (env-get env 'cur-reg))

(defun sys-inc-cur-reg (env)
  (let ((cur-reg (sys-get-cur-reg env)))
    (if (< cur-reg *register-limit*)
        (env-put env 'cur-reg (+ cur-reg 1))
        (break "ran out of registers, haven't implemented spilling yet."))))

(defun sys-next-free-reg (env)
  (let ((cur-reg (sys-get-cur-reg env)))
    (if (< cur-reg *register-limit*)
        (progn (sys-inc-cur-reg env)
               cur-reg;;(make-symbol (format nil "R~a" cur-reg))
               )
        (break "ran out of registers, haven't implemented spilling yet."))))

(defun eval-sys-symbol (env expr)
  ;; look up the symbol
  (let ((label (env-get env expr))
        (reg (sys-next-free-reg env)))
    (env-put env expr label) ;; keep track of which expr is in which reg. this is just a hunch.
    (cons reg `((LD ,label ,reg)))))

(defun eval-sys-set (env expr)
  ;; find the declaration in the environment.
  ;; (set varname expr)
  (let* ((varname (cadr expr))
         (rhs (caddr expr))
         (reg (sys-next-free-reg env))
         (label (env-get env varname)))
    (if (and label (numberp rhs))
        (cons reg `((CMOVE ,rhs ,reg)
                    (ST ,reg ,label)))
        (let ((r (eval-sys env rhs)))
          (cons (result-reg r)
                (append (result-insts r)
                        `((ST ,(result-reg r) ,label))))))))

(defun eval-sys-add (env expr)
  (when (> (length expr) 3) (break "Can't add move than two values at a time TODO fix this"))
  (let* ((r1 (eval-sys env (cadr expr)))
         (r2 (eval-sys env (caddr expr))))
    (cons (result-reg r1)
          (concat `(,(result-insts r1)
                    ,(result-insts r2)
                    ((ADD ,(result-reg r1) ,(result-reg r2) ,(result-reg r1))))))))

;; this actually works.
;; (bsim-fmt-some-instructions (sys-compile
;;                              '((var int x 0)
;;                                (var int y 0)
;;                                (var int z 0)
                               
;;                                (set x 2)
;;                                (set y 3)
;;                                (set z (+ x y)) 
;;                                )))


;; (defun calling-sequence (args f)
;;   ;; entry sequence
;;   :f
;;   (push lp) (push bp) (move sp bp)
;;   (allocate nlocals)
;;   (push used-regs)
;;   ;; exit sequence
;;   (move bp sp)
;;   (pop bp)
;;   (pop lp)
;;   (jmp lp)
;;   )
;; ;; calling sequence
;; (concat (map (lambda (arg) `(push ,arg)) args))
;; (BR f LP)
;; (deallocate (length args))

;; (defun call (f &rest args)
;;   ;; calling sequence
;;   (concat (map (lambda (arg) `(push ,arg)) args))
;;   (BR f LP)
;;   (deallocate (length args))
;;   )

;; (when (code) (block))
(defun eval-sys-when-stmt (env expr)
  (let* ((conditional (cadr expr))
         (when-block (cddr expr))
         (conditional-result (eval-sys env conditional))
         (statement-result (eval-sys-some env when-block))
         (end-when-label (sys-gen-label env "endwhen"))
         (register (result-reg statement-result))
         (instructions (concat `(,(result-insts conditional-result)
                                 ((BF ,(result-reg conditional-result) ,end-when-label))
                                 ,(result-insts statement-result)
                                 (,end-when-label)))))
    (cons register instructions)
    ))

(bsim-fmt-some-instructions
 (eval-sys-when-stmt (make-sys-env) '(when 1 (+ 1 2));;
                     ;;(when 0 (+ 2 3))
                     ))



;
; LX_0: LONG(0)
;; LY_1: LONG(0)
;; LZ_2: LONG(0)
;; CMOVE (2, 0)
;; ST(0, LX_0)
;; CMOVE(3, 1)
;; ST(1, LY_1)
;; LD(LX_0, 3)
;; LD(LY_1, 4)
;; ADD(3, 4, 3)

;; (test-assemble-beta
;;  (sys-compile '((var int x)
;;                 (var int y)
;;                 (var int z)
;;                 (set x 2)
;;                 (set y 3)
;;                 (set z (+ x y))))
;; )

(defun result-reg (result) (car result))
(defun result-insts (result) (cdr result))

(defun eval-sys-some (env code-block)
  (let ((register nil)
        (instructions (cons nil (apply #'concatenate 'list
                                       (mapcar (lambda (expr)
                                                 (result-insts (eval-sys env expr)))
                                               code-block)))))
    (cons register instructions)))

(defun eval-sys (env expr)
  (print expr)
  (check-type env environment)
  (format t "eval-sys-sys: ~a ~%" expr)
  (cond ((numberp expr) (eval-sys-constant env expr))
        ((symbolp expr) (eval-sys-symbol env expr))        
        ((listp expr) (case (car expr)
                        (var (eval-sys-var-decl env expr))                        
                        (set (eval-sys-set env expr))
                        (when (eval-sys-when-stmt env expr))
                        ;; (func (eval-sys-func env expr))
                        ;; (break (break)) ;; how to add break to an interpreter. 
                        (+ (eval-sys-add env expr))
                        ;; (- (eval-sys-op vm env #'- expr))
                        ;; (* (eval-sys-op vm env #'* expr))
                        ;; (/ (eval-sys-op vm env #'/ expr))
                        ;; (<= (eval-sys-op vm env #'<= expr))
                        ;; (bitwise-or (eval-sys-op vm env #'logior expr))
                        ;; (bitwise-and (eval-sys-op vm env #'logand expr))
                        ;; (mod (eval-sys-op vm env #'mod expr))
                        ;; (inc-pc (eval-sys-inc-pc vm env))
                        ;; (set-pc (eval-sys-set-pc vm env expr))
                        ;; (set-var (eval-sys-set-var vm env expr))
                        ;; (set-reg (eval-sys-set-reg vm env expr))
                        ;; (set-mem (eval-sys-set-mem vm env expr))
                        ;; (mem (eval-sys-get-mem vm env expr))
                        ;; (not (eval-sys-not vm env expr))
                        ;; (reg (eval-sys-get-reg vm expr))
                        ;; (sign-extend (eval-sys-sign-extend vm env expr))
                        ;; (if (eval-sys-if vm env expr))
                        ;; (eq (eval-sys-eq vm env expr))
                        (otherwise (error (format nil "unhandled case in eval-sys: ~a" expr)))))
        ;; ((eq 'nop expr) 0) ;; is nil the right thing to return here?
        ;; ((symbolp expr) (env-get env expr))
        (t (error (format nil "unhandled case in eval-sys-mc: ~a" expr)))))


(eval-sys (make-sys-env) '(var int x))

(defun sys-compile (prog)
  (apply #'concatenate 'list (let ((env (make-sys-env)))
                               (mapcar (lambda (expr) (result-insts (eval-sys env expr))) prog))))


(test-assemble-beta (sys-compile '((var int x)
                                   (set x 4)))
                    (hexs :00000000 :c01f0004 :641f0000))

(test-assemble-beta (sys-compile '((var int asdf)
                                   (set asdf 4)))
                    (hexs :00000000 :c01f0004 :641f0000))


