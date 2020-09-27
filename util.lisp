(in-package #:regmach4wasm)

(defun bind-vars (env expr)
  (check-type env environment)
  (cond ((listp expr) (mapcar (lambda (x) (bind-vars env x)) expr))
        ((symbolp expr) (let ((replacement (env-get env expr)))
                          (if replacement replacement expr)))
        (t expr)))

;; need figure out how bsim handles these differences.
(defun check-number (expr)
  (check-type expr number)
  (when (< expr -128)
    (format nil "expression results must fit within one byte; interpreted ~
                 as signed integer, ~a is too negative."  expr))
  (when (> expr 255)
    (format nil "expression results must fit within one byte, ~a is too large." expr))
  expr)

(defun 32-bit-hex-helper (keystring)
  (mapcar (lambda (num-string) (parse-integer num-string :radix 16))
          (reverse (list 
                    (subseq (string keystring) 0 2)
                    (subseq (string keystring) 2 4)
                    (subseq (string keystring) 4 6)
                    (subseq (string keystring) 6 8)))))

(defun hexs (&rest args)
  (apply 'concatenate 'list (mapcar #'32-bit-hex-helper args)))
