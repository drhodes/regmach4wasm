(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

(defstruct environment
  (id 0)
  (parent nil)
  (table (make-symbol-table)))

(defun env-put (env sym val)
  (check-type env environment)
  (symbol-table-put (environment-table env) sym val)
  env)

(defun env-get (env sym)
  (check-type env environment)
  (symbol-table-get (environment-table env) sym))

(defun root-env? (env)
  (check-type env environment)
  (eq 0 (environment-id env)))

(defun env-contains? (env sym)
  (check-type env environment)
  (nth-value 1 (env-get env sym)))

(defun env-elope (env)
  (check-type env environment)
  (make-environment :id (+ 1 (environment-id env))
                    :parent env))

(defun env-append (env pairs)
  (check-type env environment)
  (mapcar (lambda (pair)
            (env-put env (car pair)
                     (cadr pair)))
          pairs)
  env)

(defun env-lookup (env sym)
  (check-type env environment)
  ;; base case
  (if (root-env? env)
      ;; at the global scope:
      (env-get env sym)
      ;; at a local scope:
      (if (env-contains? env sym)
          (env-get env sym)
          ;; else look in and outer scope.
          (env-lookup (environment-parent env) sym))))

(defun bind-vars (env expr)
  (check-type env environment)
  (cond ((listp expr) (mapcar (lambda (x) (bind-vars env x)) expr))
        ((symbolp expr) (let ((replacement (env-get env expr)))
                          (if replacement replacement expr)))
        (t expr)))
