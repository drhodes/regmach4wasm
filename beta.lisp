(in-package #:regmach4wasm)

;; 
;; this file is a port of  ported from beta.uasm to Lisp.
;; found in BSIM located http://computationstructures.org/exercises/sandboxes/bsim.html
;; TODO: track this file down on github.
;;

(defparameter beta.uasm
  '(
    (set r0 0)
    (set r1 1)
    (set r2 2)
    (set r3 3)
    (set r4 4)
    (set r5 5)
    (set r6 6)
    (set r7 7)
    (set r8 8)
    (set r9 9)
    (set r10 10)
    (set r11 11)
    (set r12 12)
    (set r13 13)
    (set r14 14)
    (set r15 15)
    (set r16 16)
    (set r17 17)
    (set r18 18)
    (set r19 19)
    (set r20 20)
    (set r21 21)
    (set r22 22)
    (set r23 23)
    (set r24 24)
    (set r25 25)
    (set r26 26)
    (set r27 27)
    (set r28 28)
    (set r29 29)
    (set r30 30)
    (set r31 31)

    (set bp 27) ;; frame pointer (points to base of frame)
    (set lp 28) ;; linkage register (holds return adr)
    (set sp 29) ;; stack pointer (points to 1st free locn)
    (set xp 30) ;; interrupt return pointer (lp for interrupts)

    (defmacro short (x) (% x #x100) (% (>> x 8) #x100))
    (defmacro long (x) (short x) (short (>> x 16)))
    (defmacro word (x) (short x) (short (>> x 16)))

    (defmacro storage (num-words) (set $ (+ $ (* 4 num-words))))
    
    (defmacro betaop (OP RA RB RC)     
      (.align 4)
      (WORD (+ (<< OP 26)
               (<< (% RC #x20) 21)
               (<< (% RA #x20) 16)
               (<< (% RB #x20) 11))))

    (defmacro betaopc (OP RA CC RC)
      (.align 4)
      (WORD (+ (<< OP 26)
               (<< (% RC #x20) 21)
               (<< (% RA #x20) 16)
               (% CC #x10000))))

    (defmacro BETABR (OP RA RC LABEL) (betaopc OP RA (- (>> (- LABEL $) 2) 1) RC))
    
    (defmacro ADD (RA RB RC)     (betaop #x20 ra rb rc))
    (defmacro ADDC (RA C RC)     (betaopc #x30 RA C RC))

    (defmacro AND (RA RB RC)     (betaop #x28 RA RB RC))
    (defmacro ANDC (RA C RC)     (betaopc #x38 RA C RC))
    (defmacro MUL (RA RB RC)     (betaop #x22 RA RB RC))
    (defmacro MULC (RA C RC)     (betaopc #x32 RA C RC))
    (defmacro DIV (RA RB RC)     (betaop #x23 RA RB RC))
    (defmacro DIVC (RA C RC)     (betaopc #x33 RA C RC))
    (defmacro OR ( RA RB RC)     (betaop  #x29 RA RB RC))
    (defmacro ORC (RA  C RC)     (betaopc #x39 RA C RC))
    (defmacro SHL (RA RB RC)     (betaop  #x2C RA RB RC))
    (defmacro SHLC (RA C RC)     (betaopc #x3C RA C RC))
    (defmacro SHR (RA RB RC)     (betaop  #x2D RA RB RC))
    (defmacro SHRC (RA C RC)     (betaopc #x3D RA C RC))
    (defmacro SRA (RA RB RC)     (betaop  #x2E RA RB RC))
    (defmacro SRAC (RA C RC)     (betaopc #x3E RA C RC))
    (defmacro SUB (RA RB RC)     (betaop  #x21 RA RB RC))
    (defmacro SUBC (RA C RC)     (betaopc #x31 RA C RC))
    (defmacro XOR (RA RB RC)     (betaop  #x2A RA RB RC))
    (defmacro XORC (RA C RC)     (betaopc #x3A RA C RC))
    (defmacro XNOR (RA RB RC)    (betaop  #x2B RA RB RC))
    (defmacro XNORC (RA C RC)    (betaopc #x3B RA C RC))

    (defmacro CMPEQ (RA RB RC)    (betaop #x24 RA RB RC))
    (defmacro CMPEQC (RA C RC)    (betaopc #x34 RA C RC))
    (defmacro CMPLE (RA RB RC)    (betaop #x26 RA RB RC))
    (defmacro CMPLEC (RA C RC)    (betaopc #x36 RA C RC))
    (defmacro CMPLT (RA RB RC)    (betaop #x25 RA RB RC))
    (defmacro CMPLTC (RA C RC)    (betaopc #x35 RA C RC))
    
    ;;(defmacro BETABR (OP RA RC LABEL) (betaopc OP RA (- (>> (- LABEL $) 2) 1) RC))

    ;; take this one out for now since the handling of macros with
    ;; different numbered args is questionable!
    
    (defmacro BEQ (RA LABEL RC)       (BETABR #x1C RA RC LABEL))
    (defmacro BEQ (RA LABEL)          (BETABR #x1C RA r31 LABEL))
    (defmacro BF (RA LABEL RC)        (BEQ RA LABEL RC))
    (defmacro BF (RA LABEL)           (BEQ RA LABEL))
    (defmacro BNE (RA LABEL RC)       (BETABR #x1D RA RC LABEL))
    (defmacro BNE (RA LABEL)          (BETABR #x1D RA r31 LABEL))
    (defmacro BT (RA LABEL RC)        (BNE RA LABEL RC))
    (defmacro BT (RA LABEL)           (BNE RA LABEL))
    (defmacro BR (LABEL RC)           (BEQ r31 LABEL RC))
    (defmacro BR (LABEL)              (BR LABEL r31))
    (defmacro JMP (RA RC)             (betaopc #x1B RA 0 RC))
    (defmacro JMP (RA)                (betaopc #x1B RA 0 r31))

    (defmacro LD (RA CC RC)            (betaopc #x18 RA CC RC))
    (defmacro LD (CC RC)               (betaopc #x18 R31 CC RC))
    (defmacro ST (RC CC RA)            (betaopc #x19 RA CC RC))
    (defmacro ST (RC CC)               (betaopc #x19 R31 CC RC))
    (defmacro LDR (CC RC)              (BETABR #x1F R31 RC CC))

    (defmacro MOVE (RA RC)       (ADD RA R31 RC))
    (defmacro CMOVE (CC RC)      (ADDC R31 CC RC))

    (defmacro PUSH (RA)     (ADDC SP 4 SP) (ST RA -4 SP))
    (defmacro POP (RA)      (LD SP -4 RA)  ( ADDC SP -4 SP))
    (defmacro CALL (label)  (BR label LP))
    ;;

    (defmacro RTN () (JMP LP))
    (defmacro XRTN () (JMP XP))

    (DEFMACRO GETFRAME (OFFSET REG) (LD BP  OFFSET  REG))
    (DEFMACRO PUTFRAME (REG  OFFSET) (ST  REG  OFFSET  BP))
    (DEFMACRO CALL (S N) BR(S LP) (SUBC SP  (* 4 N)  SP))
    
    (DEFMACRO ALLOCATE (N) (ADDC SP (* N 4) SP))
    (DEFMACRO DEALLOCATE (N) (SUBC SP (* N 4) SP))

    (DEFMACRO PRIV_OP (FNCODE) (BETAOPC 0 0 FNCODE 0))
    (defmacro HALT () (PRIV_OP 0))
    (defmacro RDCHAR () (PRIV_OP 1))
    (defmacro WRCHAR () (PRIV_OP 2))
    (defmacro CYCLE ()  (PRIV_OP 3))
    (defmacro TIME ()   (PRIV_OP 4))
    (defmacro CLICK ()  (PRIV_OP 5))
    (defmacro RANDOM () (PRIV_OP 6))
    (defmacro SEED ()   (PRIV_OP 7))
    (defmacro SERVER () (PRIV_OP 8))



    ;; SVC calls; used for OS extensions

    (defmacro SVC (code)      (betaopc #x01 0 code 0))

    ;; Trap and interrupt vectors
    (set VEC-RESET    0)             ;; Reset (powerup)
    (set VEC-II       4)             ;; Illegal instruction (also SVC call)
    (set VEC-SEGFAULT 8)             ;; Segmentation fault
    (set VEC-CLK      12)            ;; Clock interrupt
    (set VEC-KBD      16)            ;; Keyboard interrupt
    (set VEC-MOUSE    20)            ;; Mouse interrupt

    ;; constant for the supervisor bit in the PC
    (set PC-SUPERVISOR #x80000000)    ;; the bit itself
    (set PC-MASK       #x7fffffff)    ;; a mask for the rest of the PC

    ;; kernel-mode addresses for segmentation base and bounds
    (set SEG_BASE -4)    ;; base register
    (set SEG_BOUNDS -8)  ;; bounds register

    (defmacro save-all-regs (where) (save-all-regs where r31))

    (defmacro save-all-regs (where base-reg)
      (ST r0 where base-reg)

      (ST r1 (+ where 4) base-reg)
      (ST r2 (+ where 8) base-reg)
      (ST r3 (+ where 12) base-reg)
      (ST r4 (+ where 16) base-reg)
      (ST r5 (+ where 20) base-reg)
      (ST r6 (+ where 24) base-reg)
      (ST r7 (+ where 28) base-reg)
      (ST r8 (+ where 32) base-reg)
      (ST r9 (+ where 36) base-reg)
      (ST r10 (+ where 40) base-reg)
      (ST r11 (+ where 44) base-reg)      
      (ST r12 (+ where 48) base-reg)            
      (ST r13 (+ where 52) base-reg)                  
      (ST r14 (+ where 56) base-reg)
      (ST r15 (+ where 60) base-reg)
      (ST r16 (+ where 64) base-reg)
      (ST r17 (+ where 68) base-reg)
      (ST r18 (+ where 72) base-reg)
      (ST r19 (+ where 76) base-reg)
      (ST r20 (+ where 80) base-reg)
      (ST r21 (+ where 84) base-reg)
      (ST r22 (+ where 88) base-reg)
      (ST r23 (+ where 92) base-reg)
      (ST r24 (+ where 96) base-reg)
      (ST r25 (+ where 100) base-reg)
      (ST r26 (+ where 104) base-reg)
      (ST r27 (+ where 108) base-reg)      
      (ST r28 (+ where 112) base-reg)      
      (ST r29 (+ where 116) base-reg)      
      (ST r30 (+ where 120) base-reg)      
      (ST base-reg (+ where 124) base-reg))

    
    (defmacro restore-all-regs (where) (restore-all-regs where r31))
    
    (defmacro restore-all-regs (where base-reg)
      (LD base-reg WHERE r0)
      (LD base-reg (+ WHERE 4) r1)
      (LD base-reg (+ WHERE 8) r2)
      (LD base-reg (+ WHERE 12) r3)
      (LD base-reg (+ WHERE 16) r4)
      (LD base-reg (+ WHERE 20) r5)
      (LD base-reg (+ WHERE 24) r6)
      (LD base-reg (+ WHERE 28) r7)
      (LD base-reg (+ WHERE 32) r8)
      (LD base-reg (+ WHERE 36) r9)
      (LD base-reg (+ WHERE 40) r10)
      (LD base-reg (+ WHERE 44) r11)
      (LD base-reg (+ WHERE 48) r12)
      (LD base-reg (+ WHERE 52) r13)
      (LD base-reg (+ WHERE 56) r14)
      (LD base-reg (+ WHERE 60) r15)
      (LD base-reg (+ WHERE 64) r16)
      (LD base-reg (+ WHERE 68) r17)
      (LD base-reg (+ WHERE 72) r18)
      (LD base-reg (+ WHERE 76) r19)
      (LD base-reg (+ WHERE 80) r20)
      (LD base-reg (+ WHERE 84) r21)
      (LD base-reg (+ WHERE 88) r22)
      (LD base-reg (+ WHERE 92) r23)
      (LD base-reg (+ WHERE 96) r24)
      (LD base-reg (+ WHERE 100) r25)
      (LD base-reg (+ WHERE 104) r26)
      (LD base-reg (+ WHERE 108) r27)
      (LD base-reg (+ WHERE 112) r28)
      (LD base-reg (+ WHERE 116) r29)
      (LD base-reg (+ WHERE 120) r30))


    ;; Macro to extract and right-adjust a bit field from RA, and
    ;; leave it in RB.  The bit field M:N, where M >= N.
    (defmacro extract-field (RA M N RB)
      ;; Shift left, to mask out high bits
      (SHLC RA (- 31 M) RB)         
      ;; Shift right, to mask out low bits.
      (SHRC RB (- 31 (- M N)) RB))

    
    (DEFMACRO RESERVE (N)  (set $ (+ $ (* N 4))))))



