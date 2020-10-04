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

(defun emu-execute-opc (emu byte-list instruction)
  (let ((env (make-environment))
        (rc-segment (select-bits byte-list 25 21))
        (ra-segment (select-bits byte-list 20 16))
        (lit-segment (select-bits byte-list 15 0)))
    (env-append env `((ra ,ra-segment)
                      (rc ,rc-segment)
                      (literal ,lit-segment)))
    
    (eval-mc-prog (emu-microcode-vm emu)
                  (bind-vars env (instruction-microcode instruction)))))


(defun emu-execute-op (emu byte-list instruction)
  (let ((env (make-environment))
        (rc-segment (select-bits byte-list 25 21))
        (ra-segment (select-bits byte-list 20 16))
        (rb-segment (select-bits byte-list 15 11)))
    (env-append env `((ra ,ra-segment)
                      (rb ,rb-segment)
                      (rc ,rc-segment)))
    
    (eval-mc-prog (emu-microcode-vm emu)
                  (bind-vars env (instruction-microcode instruction)))))

(defun emu-execute-one-instruction (emu byte-list)
  (let* ((opcode (select-bits byte-list 31 26))
         (instruction (get-instruction opcode)))    
    (case (instruction-layout instruction)
      (OP (emu-execute-op emu byte-list instruction))
      (OPC (emu-execute-opc emu byte-list instruction)))))

(defun emu-step (emu)
  ;; fetch instruction
  (let* ((vm (emu-microcode-vm emu))
         (next-inst (mcvm-fetch-inst vm)))
    ;; execute next instruction
    (emu-execute-one-instruction emu next-inst)))

(progn
  (let ((emu (emu-load '((CMOVE 3 r0)
                         (ADD r0 r0 r0)
                         (halt)))))
    (emu-step emu)
    (emu-step emu) 
  ))
