(in-package #:regmach4wasm)

(defun bind-vars (env expr)
  (check-type env environment)
  (cond ((listp expr) (mapcar (lambda (x) (bind-vars env x)) expr))
        ((symbolp expr) (let ((replacement (env-get env expr)))
                          (if replacement replacement expr)))
        (t expr)))

