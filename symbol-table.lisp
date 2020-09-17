(in-package :regmach4wasm)


(defstruct symbol-table (table (make-hash-table)))

(defun new-symbol-table () (make-symbol-table))


