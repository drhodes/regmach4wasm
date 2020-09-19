(in-package #:regmach4wasm)

'(
  (.reset)
  :label
  (heap-32 :heap1) ;; uses 
  (add f0 f1 f2)
  (add r1 r2 r3)
  (mv f0 r4 )
  )



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
