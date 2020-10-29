(in-package #:regmach4wasm)
(declaim (optimize (debug 3)))

(defconstant max-macro-args 100
  "The maximum number of args a macro may have.")

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
        (env-put env sym (make-array max-macro-args :initial-element nil))
        (put-macro env sym mac))))

(defun get-macro (env sym num-args)
  (if (env-contains? env sym)
      (let* ((vec (env-get env sym))
             (macrodef (elt vec num-args)))
        (if macrodef macrodef
            (error (format nil "macro called ~a is defined for ~a arguments" sym num-args))))))

(defun defined-macro? (env sym)
  (check-type sym symbol)
  (and (symbolp sym)
       (env-contains? env sym)))
