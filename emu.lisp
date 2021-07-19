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
      (OPC (emu-execute-opc emu byte-list instruction))
      (OP-JMP (emu-execute-opc emu byte-list instruction))
      (otherwise (error "unknown opcode layout encountered")))))

(defun emu-step (emu)
  ;; fetch instruction
  ;; (break)
  (let* ((vm (emu-microcode-vm emu))
         (next-inst (mcvm-fetch-inst vm)))
    ;; execute next instruction
    (emu-execute-one-instruction emu next-inst)))

(defun emu-reg (emu reg)
  ;; (check-type reg integer)
  ;; (assert (>= reg 0))
  ;; (assert (<= reg 31))
  (if (eq reg 31) 0
      (mcvm-get-reg (emu-microcode-vm emu) reg)))

(defun emu-mem (emu)
  (mcvm-memory (emu-microcode-vm emu)))

(defun emu-pc (emu)
  (mcvm-pc (emu-microcode-vm emu)))

;; -----------------------------------------------------------------------------

(defun emu-test-reg (program n-steps register exp)
  (let ((emu (emu-load program)))
    (loop repeat n-steps do     
      (emu-step emu))    
    (let ((result (emu-reg emu register)))
      (unless (eq result exp)
        (expected exp result)))
    emu))

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
                3 0 12)
  
  (emu-test-reg '((cmove 0 r0)
                  (cmove 1234 r1)
                  (or r0 r1 r0))
                3 0 1234)
  
  (emu-test-reg '((cmove 0 r0)
                  (cmove 1234 r1)
                  (and r0 r1 r0))
                3 0 0)
  
  (emu-test-reg '((cmove 11 r0)
                  (cmove 11 r1)
                  (and r0 r1 r0))
                3 0 11)

  ;;(emu-test-reg '() 0 31 0)
  
  ;; when is 
  )

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

(emu-test-reg '((cmove 1 r0)
                (or r0 r0 r0)
                (or r0 r0 r0))
              3 0 1)

(emu-test-reg '((cmove 0 r0)
                (cmove 1234 r1)
                (or r0 r1 r0))
              3 0 1234)

(emu-test-reg '((cmove 0 r0)
                (cmove 1234 r1)
                (and r0 r1 r0))
              3 0 0)

(emu-test-reg
 ;; count down from 10 in R0, count up to 10 in R2
 '((cmove 10 r0)
   (cmove 0 r2)
   
   :loop 
   (addc r0 -1 r0)
   (addc r2 1 r2)
   (bt r0 :loop)                        ; if r0 > 0 goto loop.
   
   :done
   ;; after 32 cycles register 2 should contain 10
   )
 32 2 10)

(emu-test-reg
 '((cmove 42 r0)                        ; put 42 into r0
   (st r0 4 r31)                        ; mem[4] = reg[r0]
   (ld r31 4 r0)                        ; r0 <- 42;
   )
 3 0 42)

(emu-test-reg
 '((br :A)
   (cmove 7 r0)
   (cmove 7 r0)
   :A
   (cmove 0 r0)
   (cmove 0 r0)
   (cmove :A r0)
   )
 4 0 12)

(emu-test-reg
 '((br :A)   
   (OR r0 r0 r0)
   (OR r0 r0 r0)
   :A
   (ADDC r31 1 r0)
   (ADD r0 r0 r0)
   (ADD r0 r0 r0)
   (ADD r0 r0 r0)
   )
 5 0 8) 

(emu-test-reg
 ;; this assembles correctly.
 '((br :a)
   (cmove 7 r0)
   :a
   (ADD r31 r31 r0)
   )
 2 0 0)

(emu-test-reg
 '((push r0)
   (ADD r0 r0 r0))
 1 0 0)

(defparameter bubble-sort-prog
  '((BR STEP1)  ;; start execution with Step 1

    ;; the array to be sorted
    :A
    (LONG 10) (LONG 56) (LONG 27) (LONG 69) (LONG 73) (LONG 99)
    (LONG 44) (LONG 36) (LONG 10) (LONG 72) (LONG 71) (LONG 1)

    (set ALEN (/ (- $ A) 4))  ;; determine number of elements in A
    
    ;; Please enter your code for each of the steps below...
    (set swapped r1)
    (set i r2)
    (set cur r3)
    (set prev r4)
    (set tmp r5)
    (set idx r6)

    :STEP1
    (CMOVE 0 swapped)

    :STEP2
    (CMOVE 0 i)

    :STEP3
    (ADDC i 1 i)                        ; increment array index
    (CMPLTC i 12 tmp)                   ; 
    (BF tmp STEP5)
    
    :STEP4
    (MULC i 4 idx)
    (LD idx (- A 4) prev)
    (LD idx A cur) 
    (CMPLE prev cur tmp)  ;; if A[i-1] <= A[i] then tmp=1 else tmp=0
    (BT tmp STEP3)        ;; if tmp == 1 then goto STEP3
    
    (ST prev A idx) 	  ;; swap A[i-1] and A[i] 
    (ST cur (- A 4) idx) 
    
    (CMOVE 1 swapped)     ;; set swapped to 1
    (BR STEP3)
    
    :STEP5

    (BT swapped STEP1) 

    :done
    (HALT)))


(emu-test-reg bubble-sort-prog 0 0 0)
(emu-test-reg bubble-sort-prog 0 0 0)
(emu-test-reg bubble-sort-prog 2 2 0)
(emu-test-reg bubble-sort-prog 3 2 0)
(emu-test-reg bubble-sort-prog 4 2 1)
(emu-test-reg bubble-sort-prog 4 5 0)
(emu-test-reg bubble-sort-prog 5 5 1)
(emu-test-reg bubble-sort-prog 7 6 4)
(emu-test-reg bubble-sort-prog 8 4 10)

(defun test-bubble-sort-at-cycle (n regmap &optional emu)
  (if (null regmap) (list 'pass emu)
      (let* ((reg (caar regmap))
             (val (cadar regmap))
             (emu (emu-test-reg bubble-sort-prog n reg val)))
        (test-bubble-sort-at-cycle n (cdr regmap) emu))))

(defun test-pc-at-cycle (program n-steps exp)
  (let ((emu (emu-load program)))
    (loop repeat n-steps do     
      (emu-step emu))    
    (let ((result (emu-pc emu)))
      (unless (eq result exp)
        (expected exp result)))))

(defun hexsymbol-to-int (s)
  (parse-integer (format nil "~a" s) :radix 16))

(defun test-pc-at-cycles (program pc-list &optional cycle)
  "pc-list is a list of expected program counter values which signify
what the PC should be as encountered, as the program runs one cycle as
a time."
  (cond
    ((null pc-list) 'pass)
    ((null cycle) (test-pc-at-cycles program pc-list 1))
    (t (progn (test-pc-at-cycle program cycle (car pc-list))
              (test-pc-at-cycles program (cdr pc-list) (+ 1 cycle))))))


(test-pc-at-cycles bubble-sort-prog
                   (mapcar 'hexsymbol-to-int
                           '(34 38
                             3c 40 44 48 4c 50 54 58
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58
                             3c 40 44 48 4c 50 54 58
                             3c 40 44 48 4c 50 54 58
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 48 4c 50 54 58 5c 60 64 68
                             3c 40 44 6c
                             )
                           ))

(test-bubble-sort-at-cycle 86 '((0 0) (1 1) (2 9) (3 #xa) (4 #x63) (5 1) (6 #x20) (7 0)))
(test-bubble-sort-at-cycle 87 '((0 0) (1 1) (2 9) (3 #xa) (4 #x63) (5 1) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 88 '((0 0) (1 1) (2 9) (3 #xa) (4 #x63) (5 1) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 89 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 1) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 90 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 91 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 92 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 93 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 94 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 95 '((0 0) (1 1) (2 9) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 96 '((0 0) (1 1) (2 #xa) (3 #x48) (4 #x63) (5 0) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 97 '((0 0) (1 1) (2 #xa) (3 #x48) (4 #x63) (5 1) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 98 '((0 0) (1 1) (2 #xa) (3 #x48) (4 #x63) (5 1) (6 #x24) (7 0)))
(test-bubble-sort-at-cycle 99 '((0 0) (1 1) (2 #xa) (3 #x48) (4 #x63) (5 1) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 100 '((0 0) (1 1) (2 #xa) (3 #x48) (4 #x63) (5 1) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 101 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 1) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 102 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 103 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 104 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 105 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 106 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 107 '((0 0) (1 1) (2 #xa) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 108 '((0 0) (1 1) (2 #xb) (3 #x47) (4 #x63) (5 0) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 109 '((0 0) (1 1) (2 #xb) (3 #x47) (4 #x63) (5 1) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 110 '((0 0) (1 1) (2 #xb) (3 #x47) (4 #x63) (5 1) (6 #x28) (7 0)))
(test-bubble-sort-at-cycle 111 '((0 0) (1 1) (2 #xb) (3 #x47) (4 #x63) (5 1) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 112 '((0 0) (1 1) (2 #xb) (3 #x47) (4 #x63) (5 1) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 113 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 1) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 114 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 115 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 116 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 117 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 118 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 119 '((0 0) (1 1) (2 #xb) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 120 '((0 0) (1 1) (2 #xc) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))
(test-bubble-sort-at-cycle 121 '((0 0) (1 1) (2 #xc) (3 #x1) (4 #x63) (5 0) (6 #x2c) (7 0)))

(defun memcheck-at-cycle (prog n exp) '())

;; multiplication

(emu-test-reg '(;(option clk)
                (cmove 2 r0)
                (mul r0 r0 r0))
              2 0 4)

;; factorial works!
(emu-test-reg
 '((cmove stack sp)
   (br start)
   
   ;; ------------------------------------------------------------------
   :fact                                ; factorial
   
   ;; prologue
   (push lp)                            ; save linkage
   (push bp)                            ; save base
   (move sp bp)                         ; update 
   (push r1)                            ; remember caller's r1
   (ld bp -12 r1)                       ; r1 <- n

   ;; body
   (cmplec r1 0 r0)                     ; r0 <- n <= 0 ?
   (bt r0 else)                         ; if n<=0 goto else
   
   (subc r1 1 r1)                       ; otherwise, r -= 1
   (push r1)                            ; prepare arg to fact
   (br fact lp)                         ; fact(n-1)
   
   ;; this is where the recursion starts unwinding
   (deallocate 1)                       ; remove r1
   (ld bp -12 r1)                       ; r1 <- fact(n-1)
   (mul r1 r0 r0)                       ; r0 <- n * fact(n-1)
   (br rtn)                             ; goto rtn

   :else                                ; base case
   (cmove 1 r0)                         ; return 1

   :rtn                                 ; epilogue
   (pop r1)                             ; restore caller's r1
   (move bp sp)                         ; restore caller's stack
   (pop bp)                             ; restore caller's base pointer
   (pop lp)                             ; 
   (jmp lp)

   ;; ------------------------------------------------------------------
   :start
   (cmove 4 r0)                         ; prepare fact(4)
   (push r0)                            ; push 4 onto arg stack
   (call fact)                          ; go!
   (halt)

   :stack
   (reserve 100))                       ; 
 129 0 24)



