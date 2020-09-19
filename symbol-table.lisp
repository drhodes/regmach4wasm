(in-package :regmach4wasm)

(defstruct symbol-table (store (make-hash-table)))

(defun symbol-table-put (symtab sym val)
  (check-type symtab symbol-table)
  (check-type sym symbol) 
  (setf (gethash sym (symbol-table-store symtab)) val))

(defun symbol-table-get (symtab sym) 
  (check-type symtab hash-table)
  (check-type sym symbol) 
  (aref (symbol-table-store symtab) reg))

(progn
  (let ((symtab (make-symbol-table)))
    (symbol-table-put symtab 'hello 42)
    symtab))
