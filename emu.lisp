(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

(defstruct emu  
  (microcode-vm (make-mcvm))
  (assembler-env nil))

(defun emu-load (program)
  ;; need to get the env back from the assembler which contains
  ;; .directives that the emulator will care about.
  ;;  
  (let* ((emu (make-emu))
         (result (assemble-with-beta program))
         (byte-list (assembly-byte-list result))
         (env (assembly-env result)))    
    (mcvm-load-list (emu-microcode-vm emu) byte-list)

    (setf (emu-assembler-env emu) env)
    emu))


(defun emu-execute-opc (emu byte-list instruction) nil)

(defun emu-execute-op (emu byte-list instruction)
  (let ((env (make-environment))
        (rc-segment (select-bits byte-list 25 41))
        (ra-segment (select-bits byte-list 20 16))
        (rb-segment (select-bits byte-list 15 11)))
    (env-put env 'ra ra-segment)
    (env-put env 'rb rb-segment)
    (env-put env 'rc rc-segment)
    (env-put env 'pc (mcvm-pc (emu-microcode-vm emu)))
    env))

(defun emu-execute-one-instruction (emu byte-list)
  (let* ((opcode (select-bits byte-list 31 26))
         (instruction (get-instruction opcode)))
    (case (instruction-layout instruction)
      (OP (emu-execute-op emu byte-list instruction))
      (OPC (emu-execute-opc emu byte-list instruction)))))

(defun emu-step (emu)
  ;; fetch instruction
  (let* ((vm (emu-microcode-vm emu))
         (next-inst (mcvm-fetch-next-inst vm)))
    ;; execute next instruction
    (emu-execute-one-instruction emu next-inst)))



(progn
  
  (emu-load
   '((ADD 1 2 3)
     (SUB 2 3 4)
     (ADD 4 5 6)))


  ;; end progn
  )


