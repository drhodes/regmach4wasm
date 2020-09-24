(in-package #:regmach4wasm)
(declaim (optimize (debug 3)))

;; $ is assemble at next byte
(defun done (expr) (list 'pass-done expr))
(defun macro-name (expr) (cadr expr))
(defun macro-args (expr) (caddr expr))
(defun macro-body (expr) (cdddr expr))

(defun comment (x) (list 'comment x))
(defun comment? (x) (and (listp x) (eq 'comment (car x))))
(defconstant max-macro-args 100)

(defun put-macro (env sym mac)
  (if (env-contains? env sym)
      ;; get the macro vector index by number of args.
      (let ((vec (env-get env sym))
            (num-args (length (macro-args mac))))
        (setf (elt vec num-args) mac))
      ;; else allocate a vector for the macros.
      (progn
        (env-put env sym (make-array max-macro-args))
        (put-macro env sym mac))))

(defun get-macro (env sym num-args)
  (if (env-contains? env sym)
      (let ((vec (env-get env sym)))
        (elt vec num-args))))

(defun asm-eval-defmacro (env expr)
  (env-put env (macro-name expr) expr)
  (comment 'macrodef))

(defun label? (expr) (keywordp expr))

(defun done? (expr)
  (and (listp expr)
       (not (null expr))
       (eq (car expr) 'done)))

(defun cur-byte-addr (env)
  (env-get env 'cur-byte-addr))

(defun increment-cur-byte (env n)
  (check-type env environment)
  (check-type n number)
  (let ((addr (cur-byte-addr env)))
    (env-put env 'cur-byte-addr (+ addr n))))

(defun asm-eval-number (env expr)
  (check-type expr number)
  (when (< expr -128)
    (format nil "expression results must fit within one byte; interpreted ~
                 as signed integer, ~a is too negative."  expr))
  (when (> expr 255)
    (format nil "expression results must fit within one byte, ~a is too large." expr))
  (increment-cur-byte env 1)
  expr)

(defun asm-eval-label (env expr)
  (env-put env expr (cur-byte-addr env))
  (comment expr))

(defun align? (expr) (and (listp expr)
                          (eq '.align  (car expr))))

(defun asm-eval-align (env expr)
  (if (equal expr '(.align))
      (asm-eval-align env '(.align 4))
      (if (eq 0 (mod (cur-byte-addr env)
                     (asm-eval env (cadr expr))))
          nil
          (progn
            (increment-cur-byte env 1)
            (cons 0 (asm-eval-align env expr))))))



(defun asm-eval-op (env op expr)
  (apply op (mapcar (lambda (x) (asm-eval env x))
                    (cdr expr))))

(defun asm-eval-instruction (env expr)
  ;; if the instruction contains a label
  ;; then try to replace the label  
  (let ((f (lambda (sym)
             (cond
               ;; if the sym is a register, then
               ((register-p sym) sym)
               ;; if the sym is a in the env
               ((env-contains? env sym) (env-get env sym))
               ;; then replace the sym with it's env var else error
               ;; out because eval instruction needs to be done after
               ;; all the labels have been determined.
               ;; (t (error (format nil "unknown argument in instruction: [[~a]], ~a" sym expr)))
               (t sym)))))
    (cons (car expr) (mapcar f (cdr expr)))))

(defun asm-eval-set-var (env expr)
  (let ((sym (cadr expr))
        (val (caddr expr)))
    (env-put env sym val)
    (comment 'setvar)))

(defun asm-eval-left-shift (env expr) 
  (ash (asm-eval env (cadr expr))
       (asm-eval env (caddr expr))))

(defun asm-eval-right-shift (env expr) 
  (ash (asm-eval env (cadr expr))
       (- (asm-eval env (caddr expr)))))

(defun asm-eval-mod (env expr) 
  (mod (asm-eval env (cadr expr))
       (asm-eval env (caddr expr))))

(defun $? (expr) (eq expr '$))


(defun asm-eval (env expr)
  (format t "asm ~a: ~a~%" (cur-byte-addr env) expr)
  (cond
    ((numberp expr) expr)
    (($? expr) (cur-byte-addr env))
    ((symbolp expr) (env-lookup env expr))
    ((listp expr) (case (car expr)
                    (+ (asm-eval-op env #'+ expr))
                    (- (asm-eval-op env #'- expr))
                    (* (asm-eval-op env #'* expr))
                    (/ (asm-eval-op env #'/ expr))
                    (>> (asm-eval-right-shift env expr))
                    (<< (asm-eval-left-shift env expr))                    
                    (% (asm-eval-mod env expr))))
    (t (error (format nil "unhandled expr: ~a" expr)))))


(defun set-cur-byte-addr (env n)
  (env-put env 'cur-byte-addr n))

;; pass1 define the macros
;; pass2 expand the macros
;; pass3 locate the labels
;; pass4 

(defun math-op? (expr) (member (car expr) '(+ - * / %)))

(defun asm-eval-pass1 (env expr)
  (if (listp expr)
      (case (car expr)
        (defmacro  (asm-eval-defmacro env expr))
        (otherwise expr))
      expr))

(defun defined-macro? (env sym)
  (and (symbolp sym)
       (env-contains? env sym)       
       (let ((val (env-get env sym)))
         (eq 'defmacro (car val)))))

;; expand macros
(defun asm-eval-pass2 (env expr)
  (if (and (listp expr)
           (defined-macro? env (car expr)))      
      ;; expand the macro
      (let* ((macrodef (env-get env (car expr)))
             (mac-args (macro-args macrodef))
             (call-args (cdr expr))
             (call-env (env-append (env-elope env)
                                   (zip mac-args call-args))))
        (mapcar (lambda (item) (bind-vars call-env item))
                (macro-body macrodef)))
      ;; pass through the expr
      (list expr)))

(defun repeat-expand-macro (env prog)
 (let ((expanded (apply 'concatenate 'list
                        (loop for expr in prog
                              collect (asm-eval-pass2 env expr)))))
   (if (equal prog expanded)
      expanded
      (repeat-expand-macro env expanded))))

(defun assemble (program)
  (let ((root-env (make-environment)))
    (env-put root-env 'macro-namespace (make-environment))
    (set-cur-byte-addr root-env 0)
    (let* ((result1 (loop for expr in program
                          collect (asm-eval-pass1 root-env expr)))           
           (result2 (repeat-expand-macro root-env result1))
           (result3 (affix-locations root-env result2)))           
      (list root-env result3))))

(defun set? (item)
  (and (listp item)
       (eq (car item) 'set)))

(defun affix-assignment (env item)
  ;; (env-put env item (asm-eval env item))
  (let ((sym (cadr item))
        (val (caddr item)))
    (env-put env sym (asm-eval env val))))

(defun affix-locations (env prog)
  (if (null prog) (list)
      (let ((item (car prog)))
        (cond
          ((label? item) ;; labels are labels and labels are for humans
           (env-put env item (cur-byte-addr env))
           (affix-locations env (cdr prog)))

          ((align? item) ;; .align
           (append (asm-eval-align env item)
                   (affix-locations env (cdr prog))))
          
          ((set? item) ;; assignment 
           (affix-assignment env item)
           (affix-locations env (cdr prog)))

          ((comment? item) ;; comment            
           (affix-locations env (cdr prog)))
          
          ;; otherwise
          (t (let ((result (asm-eval env item)))
               (increment-cur-byte env 1)
               (cons result (affix-locations env (cdr prog)))))))))

;;(test2)




(defun test2 ()
  (assemble
      '((set r0 0)
        (set r1 1)
        (set r2 2)
        (set A 42)
        1 2
        (.align 4)
        $ 5
        (defmacro short (x) (% x #x100) (% (>> x 8) #x100)) 
        (defmacro long (x) (short x) (short (>> x 16)))
        :start
        (+ A A)
        (long 123))))

(defun test-assemble-code (code exp)
  (unless (equal exp (cadr (assemble code)))
    (expected exp (cadr (assemble code)))))


(progn
  (test-assemble-code '($ $ $ $) '(0 1 2 3))
  (test-assemble-code '($ $ $) '(0 1 2))
  (test-assemble-code '(1 (.align 5) 2)
                      '(1 0 0 0 0 2))
  (test-assemble-code '(1 (.align) 2)
                      '(1 0 0 0 2))
  )


(defun test-assemble-beta (code exp)
  ;; "link" in the beta
  (test-assemble-code (append beta.uasm code) exp)
  ;;(append beta.uasm code) 
  )

(defun hex (u32)
  
  (string 'asdf))




(progn
  (test-assemble-beta '((add r1 2 3))
                      (list #x00 #x10 #x61 #x80))
  (test-assemble-beta '(:start (add r1 r2 :start))
                      (list #x00 #x10 #x01 #x80))
  (test-assemble-beta '(0 :start (add r1 r2 :start))
                      (list 0 0 0 0 #x00 #x10 #x21 #x80))
  (test-assemble-beta '(:start (BEQ 1 :loop) :loop)
                      (list #x00 #x00 #xe1 #x73))
  
  
  )




(defun test1 () (assemble beta.uasm))
(test1)




;; (defun assemble (program)
;;  ;; take a list of assembly statements, return a list of bytes,
;;  ;; instructions and stuff...
;;  (let ((root-env (make-environment)))
;;   (env-put root-env 'macro-namespace (make-environment))
;;   ;; different namespace for labels?
;;   (set-cur-byte-addr root-env 0)
;;   (env-put root-env 'pass-number 0)
  
;;   ;; return the assembled list and the root environment.
;;   (list (mapcar (lambda (stmt) (asm-eval root-env stmt))
;;          program)
;;      root-env)))



