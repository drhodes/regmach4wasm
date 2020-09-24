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

(defstruct instruction pattern layout opcode description microcode)
(defvar *instructions* (make-symbol-table))

(defun add-instruction (pattern layout opcode description microcode)
  "add-instruction does some stuff TODO"
  (symbol-table-put *instructions*
                    (car pattern)
                    (make-instruction :pattern pattern
                                      :layout layout
                                      :opcode opcode
                                      :description description
                                      :microcode microcode)))

(add-instruction '(ADD RA RB RC)
                 'OP
                 #*100000
                 '(The contents of register Ra are added to the contents of
                   register Rb and the 32-bit sum is written to Rc. This
                   instruction computes no carry or overflow information. If
                   desired then this can be computed through explicit compare
                   instructions.)                 
                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (reg rb)))))

(add-instruction '(ADDC RA literal RC)
                 'OPC
                 #*110000
                 '(The contents of register Ra are added to literal and the 32-bit
                   sum is written to Rc. This instruction computes no carry or overflow
                   information. If desired then this can be computed through explicit
                   compare instructions.)                 
                 '((inc-pc)
                   (set-reg rc (+ (reg ra) (sign-extend lit)))))

(add-instruction '(AND RA RB RC)
                 'OP
                 #*101000
                 '(This performs the bitwise boolean AND function between the
                   contents of register Ra and the contents of register Rb.
                   The result is written to register Rc.)                 
                 '((inc-pc)
                   (set-reg rc (bit-and (reg ra) (reg rb)))))

(add-instruction '(ANDC RA literal RC)
                 'OP
                 #*101000
                 '(This performs the bitwise boolean AND function between the
                   contents of register Ra and literal. The result is written
                   to register Rc.)                 
                 '((inc-pc)
                   (set-reg rc (bit-and (reg ra) (sign-extend rb)))))

(add-instruction '(BEQ RA label RC)
                 'OP
                 #*011100                 
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
                   (if (eq temp 0) (set pc ea))))


;;(instruction-lookup 'ADD)

(defun instruction-lookup (opcode)
  (let ((mc-prog (symbol-table-get *instructions* opcode)))
    (if mc-prog mc-prog
        (error (format nil "Failed to locate opcode in *instruction* table: ~a" opcode)))))

(defun instruction? (xs)
  (and (listp xs)
       (> (length xs) 0)
       (symbolp (car xs))
       (not (eq nil (symbol-table-get *instructions* (car xs))))))

(instruction? '(ADD r1 r2 r3))




;; '(mapcar #'add-instruction
;;         '(
;;           ;; ------------------------------------------------------------------
          
          
;;           ;; ------------------------------------------------------------------
;;           ((AND RA RB RC) '101000

;;            (This performs the bitwise boolean AND function between the
;;             contents of register Ra and the contents of register Rb.
;;             The result is written to register Rc.)
           
;;            ((inc-pc)
;;             (set-reg rc (bitwise-and (reg ra) (reg rb)))))
          
;;           ;; ------------------------------------------------------------------
;;           ((SUB RA RB RC) '100001

;;            (The contents of register Rb are subtracted from the contents
;;             of register Ra and the 32-bit difference is written to Rc.
;;             This instruction computes no borrow or overflow information.
;;             If desired then this can be computed through explicit compare
;;             instructions.)
           
;;            ((inc-pc)
;;             (set-reg rc (+ (reg ra) (reg rb)))))

;;           ;; ------------------------------------------------------------------
          
;;           ;; end of op instructions
;;           ))

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
