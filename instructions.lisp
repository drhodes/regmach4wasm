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

;; (defun instruction-lookup (opcode)
;;   (let ((mc-prog (symbol-table-get *instructions* opcode)))
;;     (if mc-prog mc-prog
;;         (error (format nil "Failed to locate opcode in *instruction* table: ~a" opcode)))))

(add-instruction '(ADD RA RB RC)
                 'OP
                 #b100000
                 '(The contents of register Ra are added to the contents of
                   register Rb and the 32-bit sum is written to Rc. This
                   instruction computes no carry or overflow information. If
                   desired then this can be computed through explicit compare
                   instructions.)
                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (reg rb)))))

(add-instruction '(ADDC RA literal RC)
                 'OPC
                 #b110000
                 '(The contents of register Ra are added to literal and the 32-bit
                   sum is written to Rc. This instruction computes no carry or overflow
                   information. If desired then this can be computed through explicit
                   compare instructions.)                 
                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (sign-extend lit)))))

(add-instruction '(AND RA RB RC)
                 'OP
                 #b101000
                 '(This performs the bitwise boolean AND function between the
                   contents of register Ra and the contents of register Rb.
                   The result is written to register Rc.)                 
                 '((inc-pc)
                   (set-reg rc (bit-and (reg ra) (reg rb)))))

(add-instruction '(ANDC RA literal RC)
                 'OPC
                 #b101000
                 '(This performs the bitwise boolean AND function between the
                   contents of register Ra and literal. The result is written
                   to register Rc.)                 
                 '((inc-pc)
                   (set-reg rc (bit-and (reg ra) (sign-extend rb)))))

(add-instruction '(BEQ RA literal RC)
                 'OPC
                 #b011100                 
                 '(The PC of the instruction following the BEQ instruction (the updated PC)
                   is written to register Rc. If the contents of register Ra are zero then the
                   PC is loaded with the target address otherwise execution continues
                   with the next sequential instruction. The displacement literal is treated
                   as a signed word offset. This means it is multiplied by 4 to convert it to
                   a byte offset then sign extended to 32 bits and added to the updated PC to form
                   the target address.)
                 
                 '((set-var diff (- (/ (- (offset label) (offset current-instruction)) 4) 1))
                   (inc-pc)
                   (set-var effective-address (+ pc (* 4 (sign-extend diff))))
                   (set-var temp (reg ra))
                   (set-reg rc pc)
                   (if (eq temp 0)
                       (set pc ea))))


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

;; (select '(#b00000110 #b00000000 #b00000000 #b00000000 ) 3 1) ;; 



;; CMPEQ
;; CMPLE
;; CMPLT
;; DIV
;; MUL
;; OR
;; SHL
;; SHR
;; SRA
;; SUB
;; XOR
;; XNOR

;; OPC
;; ADDC
;; ANDC
;; CMPEQC
;; CMPLEC
;; CMPLTC
;; DIVC
;; MULC
;; ORC
;; SHLC
;; SHRC
;; SRAC
;; SUBC
;; XORC
;; XNORC 


(defun match-instruction? (xs)
  (and (listp xs)
       (> (length xs) 0)
       (symbolp (car xs))
       (not (eq nil (symbol-table-get *instructions* (car xs))))))
