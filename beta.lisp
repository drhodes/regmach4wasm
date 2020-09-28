(in-package #:regmach4wasm)

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
    
    (defmacro ADD (RA RB RC) (betaop #x20 ra rb rc))
    (defmacro ADDC (RA C RC)   (betaopc #x30 RA C RC))

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
    (defmacro BETABR (OP RA RC LABEL) (betaopc OP RA (- (>> (- LABEL $) 2) 1) RC))

    ;; take this one out for now since the handling of macros with
    ;; different numbered args is questionable!
    
    ;;(defmacro BEQ (RA LABEL RC)       (BETABR #x1C RA RC LABEL))
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
    (set VEC_RESET    0)             ;; Reset (powerup)
    (set VEC_II       4)             ;; Illegal instruction (also SVC call)
    (set VEC_SEGFAULT 8)             ;; Segmentation fault
    (set VEC_CLK      12)            ;; Clock interrupt
    (set VEC_KBD      16)            ;; Keyboard interrupt
    (set VEC_MOUSE    20)            ;; Mouse interrupt

    ;; constant for the supervisor bit in the PC
    (set PC_SUPERVISOR 0x80000000)    ;; the bit itself
    (set PC_MASK       0x7fffffff)    ;; a mask for the rest of the PC

    ;; kernel-mode addresses for segmentation base and bounds
    (set SEG_BASE -4)    ;; base register
    (set SEG_BOUNDS -8)  ;; bounds register



    
    (DEFMACRO RESERVE (N)  (set $ (+ $ (* N 4))))
    ))



