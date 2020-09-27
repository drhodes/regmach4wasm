(in-package #:regmach4wasm)
(declaim (optimize (debug 3)))


(defconstant max-macro-args 100)

(defun macro-name (expr) (cadr expr))
(defun macro-args (expr) (caddr expr))
(defun macro-body (expr) (cdddr expr))

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

(defun defined-macro? (env sym)
  (check-type sym symbol)
  (and (symbolp sym)
       (env-contains? env sym)       
       (let ((val (env-get env sym)))
         (eq 'defmacro (car val)))))
