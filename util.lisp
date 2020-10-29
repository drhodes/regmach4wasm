(in-package #:regmach4wasm)

(defun expected (exp got)
  (if (eq exp got)
      'PASS
      (break (format nil "test error: expecting ~a, got: ~a" exp got))))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
      (list)
      (cons (list (car xs) (car ys))
            (zip (cdr xs) (cdr ys)))))

;; need figure out how bsim handles these differences.
(defun check-number (expr)
  (check-type expr number)
  (when (< expr -128)
    (error (format nil "expression results must fit within one byte; interpreted ~
                 as signed integer, ~a is too negative."  expr)))
  (when (> expr 255)
    (error (format nil "expression results must fit within one byte, ~a is too large." expr)))
  expr)

(defun 32-bit-hex-helper (keystring)
  (mapcar (lambda (num-string) (parse-integer num-string :radix 16))
          (reverse (list 
                    (subseq (string keystring) 0 2)
                    (subseq (string keystring) 2 4)
                    (subseq (string keystring) 4 6)
                    (subseq (string keystring) 6 8)))))

(defun pad (xs val n)  
  (let* ((len (length xs))
         (size (- n (mod len n))))
    (if (eq 0 (mod len n)) xs
        (append xs (repeat val size)))))

(defun hexs (&rest args)
  (apply 'concatenate 'list
         (mapcar #'32-bit-hex-helper args)))


(defun repeat (val n)
  (if (eq n 0) (list)
      (cons val (repeat val (- n 1)))))

(defun label->symbol (key)
  (read-from-string (string key)))

(defun take (n xs)
  (if (or (< n 1) (null xs))
      (list)
      (cons (car xs) (take (- n 1) (cdr xs)))))

(defun unimplemented () (error "unimplemented"))
