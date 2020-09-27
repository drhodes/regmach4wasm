;;;; regmach4wasm.asd

(asdf:defsystem "regmach4wasm"
  :description "A register mahcine for WASM"
  :author "Derek Rhodes <physci@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("fiasco")
  
  :components ((:file "package")
               (:file "symbol-table")
               (:file "env")
               (:file "util")
               (:file "macro")
               (:file "regfile")
               (:file "instructions")
               (:file "microcode")
               (:file "beta.lisp") ;; this contains macrodefs for beta.uasm
               (:file "assembler") 
              ))

