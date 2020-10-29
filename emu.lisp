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
  "execute one OPC instruction given an emulator, a list with four
bytes in indexed from low to high, and an instruction object."
  
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
  (when (equal '(0 0 0 0) byte-list) (break "HALT instruction encountered"))
  
  (let* ((opcode (select-bits byte-list 31 26))
         (instruction (get-instruction opcode)))    
    (case (instruction-layout instruction)
      (OP (emu-execute-op emu byte-list instruction))
      (OPC (emu-execute-opc emu byte-list instruction)))))

(defun emu-step (emu)
  ;; fetch instruction
  ;;(break)
  (let* ((vm (emu-microcode-vm emu))
         (next-inst (mcvm-fetch-inst vm)))
    ;; execute next instruction
    (emu-execute-one-instruction emu next-inst)))

(defun emu-reg (emu reg)
  (mcvm-get-reg (emu-microcode-vm emu) reg))

(defun emu-mem (emu) (mcvm-memory (emu-microcode-vm emu)))

(defun emu-pc (emu) (mcvm-pc (emu-microcode-vm emu)))

;; -----------------------------------------------------------------------------

(defun emu-test-reg (program n-steps register exp)
  (let ((emu (emu-load program)))
    (loop repeat n-steps do (emu-step emu))    
    (let ((result (emu-reg emu register)))
      (unless (eq result exp)
        (expected exp result)))))

(defun emu-fmt-mem (emu)
  "format the array of memory bytes to look like a hexdump with 32 bit
  words in little endian, as bsim does."
  (ram-fmt (emu-mem emu)))

(progn
  (emu-test-reg '((cmove 3 r0)
                  (add r0 r0 r0))
                2 0 6)

  (emu-test-reg '((cmove 1024 r0)
                  (add r0 r0 r0))
                2 0 2048)

  (emu-test-reg '((addc r31 2048 r0)
                  (add r0 r0 r0))
                2 0 4096)
  
  (emu-test-reg '((cmove 3 r0)
                  (add r0 r0 r0)
                  (add r0 r0 r0))
                3 0 12))

(let ((em (emu-load '(:start
                      (add r0 r0 r0)                      
                      (add r0 r0 r0)                      
                      (add r0 r0 r0)                      
                      (beq r0 start r0)
                      (add r0 r0 r0)))))
  (emu-step em)
  (expected 4 (emu-pc em))
  (emu-step em)
  (expected 8 (emu-pc em))
  (emu-step em)
  (expected 12 (emu-pc em))
  (emu-step em)
  (expected 0 (emu-pc em))
  )

(let ((em (emu-load '(:start
                      (add r0 r0 r0)                      
                      (add r0 r0 r0)                      
                      (add r0 r0 r0)                      
                      (add r0 r0 r0)                      
                      (beq r0 start r0)
                      (add r0 r0 r0)))))
  (emu-step em)
  (expected 4 (emu-pc em))
  (emu-step em)
  (expected 8 (emu-pc em))
  (emu-step em)
  (expected 12 (emu-pc em))
  (emu-step em)
  (expected 16 (emu-pc em))
  (emu-step em)
  (expected 0 (emu-pc em))
  )


(setf test-emu (emu-load '(:start
                           (addc r0 start r0)
                           (add r0 r0 r0))))

(emu-step test-emu)


 
  
