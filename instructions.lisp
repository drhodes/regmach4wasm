(in-package :regmach4wasm)

;; -----------------------------------------------------------------------------
;; INSTRUCTION SET
;;
;; all instruction documentation copied from 6.004 β documentation
;; found at this url:
;; 
;; https://d37djvu3ytnwxt.cloudfront.net/assets/courseware
;;        /v1/a31badb240163a3d3e3f1573af782c79
;;        /asset-v1:MITx+6.004.2x_2+3T2016+type@asset+block
;;        /pdfs_course_beta.pdf

;; https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/a31badb240163a3d3e3f1573af782c79/asset-v1:MITx+6.004.2x_2+3T2016+type@asset+block/pdfs_course_beta.pdf

(defstruct instruction pattern layout opcode description microcode)

(defvar *instructions* (make-hash-table))
(setf *instructions* (make-hash-table))

(defun put-instruction (opcode instruction)
  (setf (gethash opcode *instructions*) instruction))

(defun get-instruction (opcode)
  (gethash opcode *instructions*))

(defun add-instruction (pattern layout opcode description microcode)
  "add-instruction does some stuff TODO"
  (put-instruction opcode
                   (make-instruction :pattern pattern
                                     :layout layout
                                     :opcode opcode
                                     :description description
                                     :microcode microcode)))

(add-instruction '(ADD RA RB RC) 'OP #b100000                 
                 "The contents of register Ra are added to the
                  contents of register Rb and the 32-bit sum is
                  written to Rc. This instruction computes no carry or
                  overflow information. If desired then this can be
                  computed through explicit compare instructions."

                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (reg rb)))))

(add-instruction '(ADDC RA literal RC) 'OPC #b110000
                 "The contents of register Ra are added to literal and
                  the 32-bit sum is written to Rc. This instruction
                  computes no carry or overflow information. If
                  desired then this can be computed through explicit
                  compare instructions."

                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (sign-extend literal)))))

(add-instruction '(AND RA RB RC) 'OP #b101000
                 "This performs the bitwise boolean AND function
                  between the contents of register Ra and the contents
                  of register Rb.  The result is written to register
                  Rc."
                 
                 '((inc-pc)
                   (set-reg rc (bitwise-and (reg ra) (reg rb)))))

(add-instruction '(ANDC RA literal RC) 'OPC #b101000
                 "This performs the bitwise boolean AND function
                  between the contents of register Ra and literal. The
                  result is written to register Rc."

                 '((inc-pc)
                   (set-reg rc (bitwise-and (reg ra) (sign-extend rb)))))

(add-instruction '(BEQ RA literal RC) 'OPC #b011100
                 ;; BEQ(Ra,label,Rc)
                 "The PC of the instruction following the BEQ
                  instruction (the updated PC) is written to register
                  Rc. If the contents of register Ra are zero then the
                  PC is loaded with the target address otherwise
                  execution continues with the next sequential
                  instruction. The displacement literal is treated as
                  a signed word offset. This means it is multiplied by
                  4 to convert it to a byte offset then sign extended
                  to 32 bits and added to the updated PC to form the
                  target address."
                 
                 '((inc-pc)                   
                   (set-var effective-address (+ pc (* 4 (sign-extend literal))))
                   (set-var temp (reg ra)) ;; temp is needed here
                                           ;; because ra will get
                                           ;; clobbered if ra=rc
                   (set-reg rc pc)
                   (if (eq temp 0)
                       (set-pc effective-address)
                       nop))
                 )

(add-instruction '(BNE RA literal RC) 'OPC #b011101
                 "The PC of the instruction following the BNE
                  instruction (the updated PC) is written to register
                  RC. Ift eh contents of register RA are non-zero then
                  the PC is loaded with the target address - otherwise
                  execution continues with the next sequentuial
                  instruction.  The displacement literal is treated as
                  a signed word offet. This mean it is multiplied by 4
                  to convert it to a byte offset then sign extended to
                  32 bits and added to the updated PC to form the
                  target address"
                 
                 '((inc-pc)
                   (set-var effective-address (+ pc (* 4 (sign-extend literal))))
                   (set-var temp (reg ra))
                   (set-reg rc pc)
                   (if (not (eq temp 0))
                       (set-pc effective-address)
                       nop)))

(add-instruction '(CMPEQ RA RB RC) 'OP #b100100 
                 "If the contents of register RA are equal to the
                  contents of the register RB then the value 1 is
                  written to register RC else 0 is wrttien to RC"
                 
                 '((inc-pc)
                   (if (= (reg ra) (reg rb))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(CMPEQC RA literal RC) 'OPC #b110100
                 "If the contents of register RA are equal to literal
                  then the value 1 is written to register RC else 0 is
                  wrttien to RC"

                 '((inc-pc)
                   (if (= (reg ra) (sign-extend literal))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(CMPLE RA RB RC) 'OP #b100110
                 "If the contents of register RA are less than or
                  equal to the contents of register RB then 1 is
                  written to register RC - otherwise zero is written
                  to RC"

                 '((inc-pc)
                   (if (<= (reg ra) (reg rb))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(CMPLEC RA literal RC) 'OPC #b110110
                 "If the contents of register RA are less than or
                  equal to literal - the value one is written to
                  register RC then otherwise 0 is written to RC"
                 
                  '((inc-pc)
                   (if (<= (reg ra) (sign-extend literal))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(CMPLT RA RB RC) 'OP #b100101
                 "If the contents of register RA are less than the
                  contents of register RB then the value one is
                  written to register RC otherwise zero is written to
                  RC"
                 
                 '((inc-pc)
                   (if (<= (reg ra) (reg rb))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(CMPLTC RA literal RC) 'OPC #b110101
                 "If the contents of register RA are less than literal
                  then the value one is written to register RC
                  otherwise zero is written to RC"

                 '((inc-pc)
                   (if (<= (reg ra) (sign-extend literal))
                       (set-reg rc 1)
                       (set-reg rc 0))))

(add-instruction '(DIV RA RB RC) 'OP #b100011
                 "The contents of register R a are divided by the
                  contents of register Rb and the low-order 32 bits of
                  the quotient are written to Rc."
                 '((inc-pc)
                   (set-reg rc (/ (reg ra) (reg rb)))))

(add-instruction '(DIVC RA literal RC) 'OPC #b110011
                 "The contents of register Ra are divided by literal
                  and the low-order 32 bits of the quotient is written
                  to RC."
                 '((inc-pc)
                   (set-reg rc (/ (reg ra) (sign-extend literal)))))

(add-instruction '(JMP RA RC) 'OP-JMP #b011011
                 "The PC of the instruction following the JMP
                  instruction (the updated PC) is written to register
                  Rc, then the PC is loaded with the contents of
                  register Ra. The low two bits of Ra are masked to
                  ensure that the target address is aligned on a
                  4-byte boundary. Ra and Rc may specify the same
                  register; the target calculation using the old value
                  is done before the assignment of the new value. The
                  unused literal field should be filled with
                  zeroes. Note that JMP can clear the supervisor
                  bit (bit 31 of the PC) but not set it – see section
                  6.3 for details"

                 '((inc-pc)
                   (set-var effective-address (and (reg ra) #xFFFFFFFC))
                   (set-reg rc pc)
                   (set-pc effective-address)))

(add-instruction '(LD RA literal RC) 'OP-LD #b011000
                 "The effective address EA is computed by adding the
                  contents of register Ra to the signextended 16-bit
                  displacement literal. The location in memory
                  specified by EA is read into register Rc."

                 '((inc-pc)
                   (set-var effective-address (+ (reg ra) (sign-extend literal)))
                   (set-reg rc (get-mem effective-address))))

(add-instruction '(LDR label RC) 'OP-LDR #b011111
                 "The effective address EA is computed by multiplying
                  the sign-extended literal by 4 (to convert it to a
                  byte offset) and adding it to the updated PC. The
                  location in memory specified by EA is read into
                  register Rc. The Ra field is ignored and should be
                  11111 (R31). The supervisor bit (bit 31 of the PC)
                  is ignored (i.e., treated as zero) when computing
                  EA."
                 
                 '((inc-pc)
                   (set-var effective-address (+ pc (* 4 (sign-extend literal))))
                   (set-reg rc (mem-fetch effective-address))))

(add-instruction '(MUL RA RB RC) 'OP #b100010
                 "The contents of register Ra are multiplied by the
                  contents of register Rb and the low-order 32 bits of
                  the product are written to Rc."

                 '((inc-pc)
                   (set-reg rc (* (reg ra) (reg rb)))))


(add-instruction '(MULC RA literal RC) 'OPC #b110010
                 "The contents of register Ra are multiplied by
                  literal and the low-order 32 bits of the product are
                  written to Rc."
                 
                 '((inc-pc)
                   (set-reg rc (* (reg ra) (sign-extend literal)))))


(add-instruction '(OR RA RB RC) 'OP #b101001
                 "This performs the bitwise boolean OR function
                  between the contents of register Ra and the contents
                  of register Rb. The result is written to register
                  Rc."
                 
                 '((inc-pc)
                   (set-reg rc (bitwise-or (reg ra) (reg rb)))))

(add-instruction '(ORC RA literal RC) 'OP #b111001
                 "This performs the bitwise boolean OR function
                  between the contents of register Ra and literal. The
                  result is written to register Rc."
                 
                 '((inc-pc)
                   (set-reg rc (bitwise-or (reg ra) (sign-extent literal)))))

(defun select-bits (byte-list hi-bit lo-bit)  
  (destructuring-bind (b0 b1 b2 b3) byte-list
    (let ((temp #x00000000))
      (setf temp (logior temp (ash b0 0)))
      (setf temp (logior temp (ash b1 8)))
      (setf temp (logior temp (ash b2 16)))
      (setf temp (logior temp (ash b3 24)))

      ;; left shift temp to hi-bit
      (setf temp (ash temp (- 31 hi-bit)))
      ;; mask off the bits higher than 32.
      (setf temp (logand #xFFFFFFFF temp))
      ;; right shift temp back to where it was.
      (setf temp (ash temp (- (- 31 hi-bit))))
      ;; right shift temp to lo-bit.
      (setf temp (ash temp (- lo-bit)))
      ;; why not combine the last two steps? It's late and my brain
      ;; hurts.
      (logand #xFFFFFFFF temp))))

;;(select-bits  '(254 255 0 112) 15 0)

(defun sign-extend-16 (n)
  "Sign-extend means something different for an emulator because
   arithmetic routines are being carried out on the host hardware. So,
   we just need a negative number if the 16 bit version is negative.
   more thought needs to go into whether or not"
  (let* ((mask1 (logand #xFFFF n))) ;; mask off bits > 16.
    ;; if bit 15 is 1
    (if (= 1 (ash mask1 -15))
        ;; then this is a negative number.
        (lognot (- 65535 mask1))
        ;;(- n)
        ;; else this is a positive number.
        n)))

