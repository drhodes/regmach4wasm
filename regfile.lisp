(in-package :regmach4wasm)

;; -----------------------------------------------------------------------------
;; regfile

(defstruct regfile (regs-int-32 (make-array 32)))

(defun regfile-set-reg (rf reg val)
  ;; reg has to be a number from 0 to 31
  ;; val has to be a 32 bit value.
  (check-type reg number)
  (check-type val number)
  (when (and (< reg 0) (> reg 31))
    (error "regfile-set-reg got a bad register"))
  
  (setf (aref (regfile-regs-int-32 rf) reg) val))

(defun regfile-get-reg (rf reg)
  (check-type reg number)
  (when (and (< reg 0) (> reg 31))
    (error "regfile-get got a bad register"))
  (aref (regfile-regs-int-32 rf) reg))

(defun expected (exp got)
  (if (eq exp got)
      'PASS
      (error (format nil "test error: expecting ~a, got: ~a" exp got))))

(defun test-with-regfile (test-func)
  (apply test-func (list (make-regfile))))

(test-with-regfile
 (lambda (rf)
   (expected 0 (regfile-get-reg rf 1))))

(test-with-regfile
 (lambda (rf)
   (regfile-set-reg rf 0 1)
   (expected (regfile-get-reg rf 0) 1)))


(defvar int-registers '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9
                        r10 r11 r12 r13 r14 r15 r16 r17 r18 r19
                        r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31))

(defun reg-to-num (reg)
  "Register to number, converts symbol of format r15 to 15"
  (check-type reg symbol)
  (case reg
    (r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7) (r8 8) (r9 9)
    (r10 10) (r11 11) (r12 12) (r13 13) (r14 14) (r15 15) (r16 16) (r17 17) (r18 18) (r19 19)
    (r20 20) (r21 21) (r22 22) (r23 23) (r24 24) (r25 25) (r26 26) (r27 27) (r28 28) (r29 29)
    (r30 30) (r31 31)
    (otherwise (error (format nil "unknown register: ~a, expecting r0 - r31" reg)))))
