(in-package :regmach4wasm)

;; -----------------------------------------------------------------------------
;; instructions
;;
;; all instruction documentation copied from 6.004 β documentation
;;

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

(instruction-lookup 'ADD)


(defun instruction-lookup (opcode)
  (let ((mc-prog (symbol-table-get *instructions* opcode)))
    (if mc-prog mc-prog
        (error (format nil "Failed to locate opcode in *instruction* table: ~a" opcode)))))

(defun reg-from-inst (mcvm inst)
  "Remember there are two types of instructions OP and OPC."
  (cond
    ((jmp? inst) (jmp-ra inst))
    
    )
  ;; (ADD ra rb rc)
  ;; (ADDC ra lit rc)
  )

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
