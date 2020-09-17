;;;; regmach4wasm.asd

(asdf:defsystem "regmach4wasm"
  :description "A register mahcine for WASM"
  :author "Derek Rhodes <physci@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "regmach4wasm")
               (:file "symbol-table")
               (:file "assembler")
               (:file "regfile")
               (:file "microcode")

               
               ))

