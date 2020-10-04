;;;; regmach4wasm.asd

(asdf:defsystem "regmach4wasm"
  :description "A register mahcine for WASM"
  :author "Derek Rhodes <physci@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("fiasco")
  
  :components ((:file "package")
               (:file "util")
               (:file "symbol-table")
               (:file "env")
               (:file "macro")
               (:file "beta") ;; this contains macrodefs for beta.uasm
               (:file "assembler") 
               (:file "instructions")
               (:file "regfile")
               (:file "microcode")
               (:file "emu") 
             ))

