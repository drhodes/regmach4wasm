(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

(defun comment (x) (list 'comment x))
(defun comment? (x) (and (listp x) (eq 'comment (car x))))
(defun label? (expr) (keywordp expr))
(defun align? (expr) (and (listp expr)
                          (eq '.align  (car expr))))

(defun asm-eval-align (env expr)
  (if (equal expr '(.align))
      ;; if using short hand without args, the assume 4 bytes.
      (asm-eval-align env '(.align 4))
      ;; else
      (if (eq 0 (mod (cur-byte-addr env)
                     (asm-eval env (cadr expr))))
          nil
          (progn
            (increment-cur-byte env 1)
            (cons 0 (asm-eval-align env expr))))))

(defun asm-eval-left-shift (env expr addr) 
  (ash (asm-eval-expr env (cadr expr) addr)
       (asm-eval-expr env (caddr expr) addr)))

;; https://rosettacode.org/wiki/Bitwise_operations#Common_Lisp
(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defun asm-eval-right-shift (env expr addr) 
  (shr (asm-eval-expr env (cadr expr) addr)
       32
       (asm-eval-expr env (caddr expr) addr)))

;; (defun asm-eval-right-shift (env expr addr) 
;;   (ash (asm-eval-expr env (cadr expr) addr)
;;        (- (asm-eval-expr env (caddr expr) addr))))

(defun asm-eval-mod (env expr addr) 
  (mod (asm-eval-expr env (cadr expr) addr)
       (asm-eval-expr env (caddr expr) addr)))

(defun $? (expr) (eq expr '$))

;; pass: define the macros
;; pass: establish pause-unpause-$ delimeters for toplevel macro calls.
;; pass: expand the macros
;; pass: locate the labels
;; pass: 

(defun asm-eval-pass1 (env expr)
  (if (listp expr)
      (case (car expr)
        (defmacro (progn
                    (put-macro env (macro-name expr) expr)
                    (comment 'defmacro)))
        (otherwise expr))
      expr))

;; ---------------------------------
;; Expand one macro.
(defun asm-eval-pass2 (env expr)
  (if (and (listp expr)
           (defined-macro? env (car expr)))      
      ;; expand the macro
      (let* ((macrodef (get-macro env (car expr) (length (cdr expr))))
             (mac-args (macro-args macrodef))
             (call-args (cdr expr))
             (call-env (env-append (env-elope env)
                                   (zip mac-args call-args))))
        (mapcar (lambda (item) (bind-vars call-env item))
                (macro-body macrodef)))
      ;; pass through the expr
      (list expr)))

(defun repeat-expand-macro (env prog)
  (let ((expanded (apply 'concatenate 'list
                         (loop for expr in prog
                               collect (asm-eval-pass2 env expr)))))
    (if (equal prog expanded)
        expanded
        (repeat-expand-macro env expanded))))

(defun set? (item)
  (and (listp item)
       (eq (car item) 'set)))

(defun wrap-one-macro (item)
  (list 'pause-incrementing-$ item 'unpause-incrementing-$))

(defun wrap-macro-calls (env prog)
  (if (null prog) (list)
      (let ((item (car prog))
            (rest (cdr prog)))        
        (cond ((and (listp item)
                    (defined-macro? env (car item))) 
               (append (wrap-one-macro item)
                       (wrap-macro-calls env rest)))
              (t (cons item (wrap-macro-calls env rest)))))))

(defun affix-assignment (env item)
  (let ((sym (cadr item))
        (val (caddr item)))
    (if (eq sym '$)
        (set-cur-byte-addr env val)
        (env-put env sym (asm-eval-expr env val (cur-byte-addr env))))))

(defun match-set-current-byte? (item)
  (and (listp item)
       (> (length item) 2) 
       (equal 'set (car item))
       (equal '$ (cadr item))
       ))

;; this is a pass.

(defun pass-affix-locations (env prog)
  (if (null prog) (list)
      (let ((item (car prog)))
        (cond
          ((label? item) ;; track where the labels are
           (env-put env (label->symbol item) (cur-byte-addr env))
           (pass-affix-locations env (cdr prog)))
          
          ((align? item) ;; .align
           (append (asm-eval-align env item)
                   (pass-affix-locations env (cdr prog))))

          ;; this clause needs to come before (set? item)
          ((match-set-current-byte? item)
           (let* ((val (asm-eval-expr env (caddr item) (cur-byte-addr env)))
                  (diff (- val (cur-byte-addr env))))
             (set-cur-byte-addr env val)
             (append (repeat 0 diff)
                     (pass-affix-locations env (cdr prog)))))
          
          ((set? item) ;; assignment           
           (affix-assignment env item)
           (pass-affix-locations env (cdr prog)))

          ((comment? item) ;; comment            
           (pass-affix-locations env (cdr prog)))

          ((or (pause? item) (unpause? item))
           ;; keep the pause statements without incrementing 
           (cons item (pass-affix-locations env (cdr prog))))
          
          ;; otherwise
          (t (increment-cur-byte env 1)
             (cons item (pass-affix-locations env (cdr prog))))))))

(defun assemble (program)
  (let ((env (make-environment)))
    (env-put env 'macro-namespace (make-environment))
    (set-cur-byte-addr env 0)
    (let* ((result1 (loop for expr in program
                          collect (asm-eval-pass1 env expr)))
           ;; fix design-mistake-1 (see docs)
           ;; this is ugly.
           (result15 (wrap-macro-calls env result1))
           (result2 (repeat-expand-macro env result15))
           (result3 (affix-locations env result2))
           (result4 (progn
                      (set-cur-byte-addr env 0)
                      (replace-symbols env result3)))           
           (result5 (progn (set-cur-byte-addr env 0)
                           (asm-eval-prog env result4))))
      result5)))

(defun increment-cur-byte (env n)
  (env-put env 'cur-byte-addr
           (+ n (env-get env 'cur-byte-addr))))

(defun asm-eval-prog (env prog)  
  (if (null prog) (list)
      (let ((item (car prog))
            (rest (cdr prog))
            (cur-addr (cur-byte-addr env)))
        (cond ((pause? item) (progn (pause$ env)
                                    (asm-eval-prog env rest)))
              ((unpause? item) (progn (unpause$ env)
                                      (asm-eval-prog env rest)))              
              (t (progn
                   (increment-cur-byte env 1)
                   (cons (asm-eval-expr env item cur-addr)
                         (asm-eval-prog env rest))))))))

(defun pause$ (env)
  (env-put env 'paused-byte-addr (cur-byte-addr env))
  (env-put env 'cur-byte-addr-paused t))

(defun unpause$ (env)
  (env-put env 'cur-byte-addr-paused nil))

(defun pause? (item) (eq 'pause-incrementing-$ item))
(defun unpause? (item) (eq 'unpause-incrementing-$ item))

(defun set-cur-byte-addr (env n)
  (env-put env 'cur-byte-addr n))

(defun cur-byte-addr (env)
  (if (env-get env 'cur-byte-addr-paused)
      (env-get env 'paused-byte-addr)
      (env-get env 'cur-byte-addr)))

(defun asm-eval-expr (env expr addr)
  (check-type env environment)
  (check-type addr number)
  (cond (($? expr) addr)
        ((numberp expr) expr)
        ((label? expr) (env-get env (label->symbol expr)))
        ((symbolp expr) (env-get env expr))
        ((listp expr) (case (car expr)
                        (+ (asm-eval-op env #'+ expr addr))
                        (- (asm-eval-op env #'- expr addr))
                        (* (asm-eval-op env #'* expr addr))
                        (/ (asm-eval-op env #'/ expr addr))
                        (>> (asm-eval-right-shift env expr addr))
                        (<< (asm-eval-left-shift env expr addr))                    
                        (% (asm-eval-mod env expr addr))))
        (t (error (format nil "unhandled case in asm-eval-expr: ~a" expr)))))

(defun asm-eval-op (env op expr addr)
  (apply op (mapcar (lambda (x) (asm-eval-expr env x addr))
                    (cdr expr))))

'(progn
  ;; failing 
  (test-assemble-beta '($ $ $ $ (betabr 0 0 0 4)) (hexs :03020100 :0000ffff))
  (test-assemble-beta '($ $ (betabr 0 0 0 4) $ $) (hexs :00000100 :0000ffff :00000908))
  (test-assemble-beta '($ $ (beq 0 0 0) $ $ 0 0) (hexs :00000100 :7000fffe :00000908))
  (test-assemble-beta '((BR :step1)
                        :A
                        (LONG 10) (LONG 56) (LONG 27) (LONG 69) (LONG 73) (LONG 99)
                        (LONG 44) (LONG 36) (LONG 10) (LONG 72) (LONG 71) (LONG 1)
                        
                        (set ALEN (/ (- $ A) 4))
                        
                        :step1)
   (hexs :73ff0000))
  )





(progn
  
  (test-assemble-beta '($ $ (add 0 0 0) $ $ 0 0) (hexs :00000100 :80000000 :00000908))
  (test-assemble-beta '((reserve 2) $ $ $ $) (hexs :00000000 :00000000 :0b0a0908))
  (test-assemble-code '(:a a) '(0))
  
  (let ((env (make-environment)))
    (set-cur-byte-addr env 0)
    (pause$ env)
    (set-cur-byte-addr env 4)
    (unpause$ env)
    (cur-byte-addr env))

  (let ((env (make-environment)))
    (env-put env 'ASDF 42)
    (env-get env (read-from-string (string :asdf))))

  (test-assemble-beta '((long 123456)) (hexs :0001e240))
  (test-assemble-beta '((long 123456) $ $ $ $) (hexs :0001e240 :07060504))


  
  (test-assemble-beta '((set x 2) (set y 3) (+ x y)) '(5))
  
  (test-assemble-beta '((ADD 1 2 3) (SUB 2 3 4) (ADD 4 5 6))
                      (hexs :80611000 :84821800 :80c42800))
  
  (test-assemble-beta '((defmacro wrap (x) x x) (wrap 1) $ $) (hexs :03020101))
  (test-assemble-beta '((ADD 0 0 0) $) (list 0 0 0 128 4))
  (test-assemble-beta '((ADD $ $ $) (ADD $ $ $)) (list 0 0 0 #x80 0 #x20 #x84 #x80)) 
  (test-assemble-beta '($ $ $ $) (hexs :03020100))
  
  (test-assemble-beta '((ADD $ $ $)) '(0 0 0 #x80))
  (test-assemble-beta '((betabr 1 2 3 4)) (list #x00 #x00 #x62 #x04)) 
  (test-assemble-beta '((betabr 0 0 0 4)) '(0 0 0 0))
  (test-assemble-beta '((BEQ 1 :loop) :loop) (list #x00 #x00 #xe1 #x73))
  (test-assemble-beta '((>> 1 0) (>> 1 1) (>> 1 2) (>> 1 3)
                        (<< 1 0) (<< 1 1) (<< 1 2) (<< 1 3))
                      '(1 0 0 0 1 2 4 8))
  
  (test-assemble-beta '(0 0 0 $) (list 0 0 0 3))
  
  (test-assemble-beta '((defmacro wrap (x) x) (wrap 1) $) '(1 1))
  
  (test-assemble-beta '((defmacro wrap (x) x x) $) '(0))
  (test-assemble-beta '((set $ 5) $) '(0 0 0 0 0 5))
  (test-assemble-beta '((+ 1 1) $) '(2 1))
  

  (test-assemble-code '(1 (.align 5) 2) '(1 0 0 0 0 2))
  (test-assemble-code '(1 (.align) 2) '(1 0 0 0 2))
  (test-assemble-beta '((set $ (+ $ 8)) $) '(0 0 0 0 0 0 0 0 8))

  (test-assemble-beta '($ 1 2 $) '(0 1 2 3))
  
  (test-assemble-beta '((add r1 2 3)) (list #x00 #x10 #x61 #x80))
  (test-assemble-beta '(:start (add r1 r2 :start)) (list #x00 #x10 #x01 #x80))
  (test-assemble-beta '(0 :start (add r1 r2 :start)) (list 0 0 0 0 #x00 #x10 #x21 #x80))
  (test-assemble-beta '((BEQ 1 2)) (list #xff #xff #xe1 #x73))
  



  (test-assemble-beta '((+ $ $)) '(0)) 
  (test-assemble-beta '((set $ 10) (+ $ $)) '(0 0 0 0 0 0 0 0 0 0 20))
  
  (test-assemble-beta '((betabr 0 0 0 0)) '(#xff #xff #x00 #x00))
  (test-assemble-beta '((betabr 0 0 4 0)) '(#xFF #xFF #x80 #x00))
  (test-assemble-beta '((betabr 0 4 0 0)) '(#xFF #xFF #x04 #x00))
  (test-assemble-beta '((betabr 4 0 0 0)) '(#xFF #xFF #x00 #x10))
  (test-assemble-beta '((% (+ (<< 0 26) (<< (% 0 32) 21)
                            (<< (% 0 32) 16)
                            (% (- (>> (- 4 $) 2) 1) 65536))
                         256))
                      '(0))
  
  ;; The problem is that the macro is being expanded into 4
  ;; bytes. Each byte must use the same cur-byte-loc ($), because
  ;; those are the semantics. 
  
  (test-assemble-beta '((% (+ (<< 0 26) (<< (% 0 32) 21)
                            (<< (% 0 32) 16)
                            (% (- (>> (- 4 0) 2) 1) 65536))
                         256))
                      '(0))

  (test-assemble-beta '((% (>> (+ (<< 0 26) (<< (% 0 32) 21)
                                (<< (% 0 32) 16) (% (- (>> (- 4 $) 2) 1) 65536))
                            8)
                         256))
                      '(0))
  
  (test-assemble-beta '((% (>> (+ (<< 0 26) (<< (% 0 32) 21)
                                (<< (% 0 32) 16)
                                (% (- (>> (- 4 $) 2) 1) 65536))
                            16)
                         256))
                      '(0))
  
  (test-assemble-beta '((% (>> (>> (+ (<< 0 26) (<< (% 0 32) 21) (<< (% 0 32) 16)
                                    (% (- (>> (- 4 $) 2) 1) 65536))
                                16)
                            8)
                         256))
                      '(0))
  

  (test-assemble-beta '((betaopc 0 0 0 0)) '(#x00 #x00 #x00 #x00))
  (test-assemble-beta '((betaopc 0 0 0 4)) '(#x00 #x00 #x80 #x00))
  (test-assemble-beta '((betaopc 0 0 4 0)) '(#x04 #x00 #x00 #x00))
  (test-assemble-beta '((betaopc 0 4 0 0)) '(#x00 #x00 #x04 #x00))
  (test-assemble-beta '((betaopc 4 0 0 0)) '(#x00 #x00 #x00 #x10))

  (test-assemble-beta '((betaopc 4 4 4 4)) '(#x04 #x00 #x84 #x10))
  (test-assemble-beta '((betaopc 14 14 14 14)) '(#x0E #x00 #xCE #x39))

  ;;(defmacro BETABR (OP RA RC LABEL) (betaopc OP RA (- (>> (- LABEL $) 2) 1) RC))
  ;; OP=0 RA=0 RC=0 LABEL=4
  (test-assemble-beta '((- (>> (- 4 $) 2) 1)) '(0))
  (test-assemble-beta '((- (>> (- 0 $) 2) 1)) '(-1))
  ;;(test-assemble-beta '((betaopc 1 2 3 4)) '(#x03 #x00 #x82 #x04))

  
  )















