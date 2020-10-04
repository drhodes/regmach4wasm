(in-package :regmach4wasm)

(defstruct symbol-table (store (make-hash-table)))

(defun symbol-table-put (symtab sym val)
  (check-type symtab symbol-table)
  (setf (gethash sym (symbol-table-store symtab)) val))

(defun symbol-table-get (symtab sym) 
  (check-type symtab symbol-table)
  (gethash sym (symbol-table-store symtab)))

;; (progn
;;   (let ((symtab (make-symbol-table)))
;;     (symbol-table-put symtab 'hello 42)
;;     symtab))
