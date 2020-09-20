(in-package #:regmach4wasm)

'((.reset)
  :label

  (set $ (+ $ (* 4 NWORDS)))  
  (add f0 f1 f2)
  (add r1 r2 r3)
  (mv f0 r4 )
  )

'(1 2 3)


;; $ is current address.



'(
  ;; the assembler boils all the expression down to bytes.
  :label1
  
  ;; end program
  )

(defun test-with-assembler (program predicate)
  )

(defun asm-eval-number (expr)
  (check-type expr number)
  (when (< expr -128)
    (format nil "expression results must fit within one byte; interpreted ~
                 as signed integer, ~a is too negative."  expr))
  (when (> expr 255)
    (format nil "expression results must fit within one byte, ~a is too large." expr))
  expr)




;; (defun assemble (mcvm expr)
;;   ;; ((set-pc (+ pc 4))
;;   ;;  (set-reg rc (& ra rb)))
;;   (cond ((numberp expr) expr)
;;         ;; ((label? expr) (eval-label expr))
;;         ;; this needs to go into the assembler.
;;         ;; FANTASTIC. untagged labels encountered on the first pass
;;         ;; should be replaced with (label-pass-2 :keyword)
;;         ;; or maybe (pass2 :keyword)
;;         ;; symbol table         
;;         ((listp expr) (case (car expr)        
;;                         (+ (eval-op mcvm + expr))
;;                         (- (eval-op mcvm - expr))
;;                         (* (eval-op mcvm * expr))
;;                         (/ (eval-op mcvm / expr))
;;                         (set-pc (eval-set-pc mcvm expr))
;;                         (set-var (eval-set-var mcvm expr))
;;                         (set-reg (eval-set-reg mcvm expr))))
;;         ((register-p expr) (reg-to-num expr))        
;;         (t (error (format nil "unhandled case in eval-mc: ~a" expr)))))
