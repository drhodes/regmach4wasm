(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

(defstruct assembly env byte-list)

(defun comment (x) (list 'comment x))
(defun comment? (x) (and (listp x) (eq 'comment (car x))))
(defun label? (expr) (keywordp expr))
(defun align? (expr) (and (listp expr) (eq '.align  (car expr))))
(defun text? (expr) (and (listp expr) (eq '.text  (car expr))))

(defun asm-eval-align (env expr)
  (if (equal expr '(.align))
      ;; if using short hand without args, the assume 4 bytes.
      (asm-eval-align env '(.align 4))
      ;; else
      (if (eq 0 (mod (cur-byte-addr env)
                     (asm-eval-expr env
                                    (cadr expr)
                                    (cur-byte-addr env))))
          nil
          (progn
            (increment-cur-byte env 1)
            (cons 0 (asm-eval-align env expr))))))

(defun asm-eval-text (env expr)
  (let ((chars (concatenate 'list (cadr expr))))
    (increment-cur-byte env (length chars))
    (mapcar #'char-code chars)))

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

          ((text? item) ;; .text
           (append (asm-eval-text env item)
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
           (result3 (pass-affix-locations env result2))
           (result4 (progn (set-cur-byte-addr env 0) (asm-eval-prog env result3)))
           (result5 (mapcar #'fix-neg-byte result4))
           )      
      (make-assembly :env env :byte-list result5))))

(defun fix-neg-byte (n)
  ;; adhere to 6.004x 
  ;; https://github.com/6004x/6.004_tools/blob/c4c999250002e2a595402d4a7abe302c4e91fdd5/bsim/assemble.js#L347
  ;; "numbers must be unsigned, so if they're less than zero we force them to be the unsigned"
  (check-number n)
  (if (< n 0)
      (+ #xFF n 1)
      n))

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

(defun assemble-with-beta (code)
  (assemble (append beta.uasm code)))

(defun test-assemble-code (code exp)
  (unless (equal exp (pad (assembly-byte-list (assemble code)) 0 4))
    (expected exp (pad (assembly-byte-list (assemble code)) 0 4))))

(defun test-assemble-beta (code exp)
  ;; "link" in the beta
  (test-assemble-code (append beta.uasm code) exp))

(progn
  ;; passing
  (test-assemble-beta '($ $ $
                        :start
                        (add r0 r0 r0)
                        (beq r0 start r0)
                        (add r0 r0 r0)) (hexs :00020100 :80000000 :7000fffd :80000000))

  (test-assemble-beta '((.text "abcd")) (hexs :64636261))
  
  (test-assemble-beta '((restore-all-regs 0))
                      (hexs :601f0000 :603f0004 :605f0008 :607f000c
                            :609f0010 :60bf0014 :60df0018 :60ff001c
                            :611f0020 :613f0024 :615f0028 :617f002c
                            :619f0030 :61bf0034 :61df0038 :61ff003c
                            :621f0040 :623f0044 :625f0048 :627f004c
                            :629f0050 :62bf0054 :62df0058 :62ff005c
                            :631f0060 :633f0064 :635f0068 :637f006c
                            :639f0070 :63bf0074 :63df0078))
  
  (test-assemble-beta '((cmove 25 r0) (add r0 r0 r0)) (hexs :c01f0019 :80000000))
  
  (test-assemble-beta '($ $ (betabr $ $ $ $) $ $ 0 0) (hexs :00000100 :0842ffff :00000908))
  (test-assemble-beta '($ $ (betabr 0 0 0 0) $ $ 0 0) (hexs :00000100 :0000fffe :00000908))
  (test-assemble-beta '($ $ $ $ (betabr 0 0 0 0)) (hexs :03020100 :0000fffe))

  (test-assemble-beta '((WORD -18) (WORD 5) (WORD -28) (WORD 6) (WORD -32) (WORD -16) (WORD -13)
                        (WORD -79) (WORD -55) (WORD -36) (WORD -49) (WORD 22) (WORD -28) (WORD 2)
                        (WORD -33) (WORD 10) (WORD 4) (WORD 23) (WORD 10) (WORD -55))                      
                      (hexs 
                       :ffffffee :00000005 :ffffffe4 :00000006 :ffffffe0 :fffffff0 :fffffff3
                       :ffffffb1 :ffffffc9 :ffffffdc :ffffffcf :00000016 :ffffffe4 :00000002
                       :ffffffdf :0000000a :00000004 :00000017 :0000000a :ffffffc9))
  
  (test-assemble-beta '($ $ (betabr 0 0 0 4) 0 0) (hexs :00000100 :0000ffff :00000000))
  (test-assemble-beta '($ $ (betabr 0 0 0 4) $ $) (hexs :00000100 :0000ffff :00000908))
  (test-assemble-beta '($ $ (beq 0 0 0) $ $ 0 0) (hexs :00000100 :7000fffe :00000908))
  (test-assemble-beta '($ $ $ $ (beq 0 0 0) $ $) (hexs :03020100 :7000fffe :00000908))
  (test-assemble-beta '($ $ $ $ (betabr 0 0 0 4)) (hexs :03020100 :0000ffff))
  
  (test-assemble-beta '((short 12345)) (hexs :00003039))
  (test-assemble-beta '((word 0)) (hexs :00000000)) 
  (test-assemble-beta '((word -12345678)) (hexs :ff439eb2))
  (test-assemble-beta '((word 12345678)) (hexs :00bc614e))

  (test-assemble-beta '(0 0 (betabr 0 0 0 4) $ $) (hexs :00000000 :0000ffff :00000908))
  (test-assemble-beta '(0 0 0 0 (betabr 0 0 0 4)) (hexs :00000000 :0000ffff))
  (test-assemble-code '($ $ $ $ (- 0 1)) (hexs :03020100 :000000ff))
  (test-assemble-code '(-1) (hexs :000000ff))
  (test-assemble-code '(-2) (hexs :000000fe))
  (test-assemble-code '((- 1 2)) (hexs :000000ff))
  
  (test-assemble-beta '(-1 -1) (hexs :0000ffff))
  (test-assemble-beta '((- (>> (- step $) 2) 1) :step ) (hexs :000000ff))
  
  (test-assemble-beta '((- (>> (- 0 $) 2) 1)) (hexs :000000ff))
  (test-assemble-beta '((BR step1) $ :step1) (hexs :73ff0000 :00000004))
  (test-assemble-beta '((BR step1) $ :step1) (hexs :73ff0000 :00000004))

  (test-assemble-beta '((>> 100 2)) (hexs :00000019))
  (test-assemble-beta '((>> 100 3)) (hexs :0000000c))
  (test-assemble-beta '((<< 1 1)) (hexs :00000002))
  (test-assemble-beta '((<< 1 2)) (hexs :00000004))
  (test-assemble-beta '((<< 1 3)) (hexs :00000008))
  (test-assemble-beta '((<< 1 4)) (hexs :00000010))

  (test-assemble-beta '((betaopc 0 0 0 0)) (hexs :00000000))
  (test-assemble-beta '((betaopc 1 0 0 0)) (hexs :04000000))
  (test-assemble-beta '((betaopc 0 1 0 0)) (hexs :00010000))
  (test-assemble-beta '((betaopc 0 0 1 0)) (hexs :00000001))
  (test-assemble-beta '((betaopc 0 0 0 1)) (hexs :00200000))
  (test-assemble-beta '((betaopc 0 0 1 1)) (hexs :00200001))
  (test-assemble-beta '((betaopc 0 1 0 1)) (hexs :00210000))
  (test-assemble-beta '((betaopc 0 1 1 0)) (hexs :00010001))
  (test-assemble-beta '((betaopc 0 1 1 1)) (hexs :00210001))
  (test-assemble-beta '((betaopc 1 0 0 1)) (hexs :04200000))
  (test-assemble-beta '((betaopc 1 0 1 0)) (hexs :04000001))
  (test-assemble-beta '((betaopc 1 0 1 1)) (hexs :04200001))
  (test-assemble-beta '((betaopc 1 1 0 0)) (hexs :04010000))
  (test-assemble-beta '((betaopc 1 1 0 1)) (hexs :04210000))
  (test-assemble-beta '((betaopc 1 1 1 0)) (hexs :04010001))
  (test-assemble-beta '((betaopc 1 1 1 1)) (hexs :04210001))
  
  (test-assemble-beta '((betaopc 3 7 2 11)) (hexs :0d670002))
  (test-assemble-beta '((betaopc 3 7 2 11) (betaopc 1 2 3 4)) (hexs :0d670002 :04820003))

  (test-assemble-beta '((BR :step1)
                        (set $ 52)
                        :step1) (hexs :73ff000c :00000000 :00000000 :00000000
                        :00000000 :00000000 :00000000 :00000000
                        :00000000 :00000000 :00000000 :00000000
                        :00000000))
  
  (test-assemble-beta '($ $ (add 0 0 0) $ $ 0 0) (hexs :00000100 :80000000 :00000908))
  (test-assemble-beta '((reserve 2) $ $ $ $) (hexs :00000000 :00000000 :0b0a0908))
  (test-assemble-code '(:a a) '(0 0 0 0))
  
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
  (test-assemble-beta '((set x 2) (set y 3) (+ x y)) '(5 0 0 0))
  (test-assemble-beta '((ADD 1 2 3) (SUB 2 3 4) (ADD 4 5 6))
                      (hexs :80611000 :84821800 :80c42800))
  (test-assemble-beta '((defmacro wrap (x) x x) (wrap 1) $ $) (hexs :03020101))
  (test-assemble-beta '((ADD 0 0 0) $) (list 0 0 0 128 4 0 0 0))
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
  (test-assemble-beta '((defmacro wrap (x) x) (wrap 1) $) '(1 1 0 0))
  (test-assemble-beta '((defmacro wrap (x) x x) $) '(0 0 0 0))
  (test-assemble-beta '((set $ 5) $) '(0 0 0 0 0 5 0 0))
  (test-assemble-beta '((+ 1 1) $) '(2 1 0 0))
  (test-assemble-code '(1 (.align 5) 2) '(1 0 0 0 0 2 0 0))
  (test-assemble-code '(1 (.align) 2) '(1 0 0 0 2 0 0 0))
  (test-assemble-beta '((set $ (+ $ 8)) $) '(0 0 0 0 0 0 0 0 8 0 0 0))
  (test-assemble-beta '($ 1 2 $) '(0 1 2 3))
  (test-assemble-beta '((add r1 2 3)) (hexs :80611000))
  (test-assemble-beta '(:start (add r1 r2 :start)) (hexs :80011000))
  (test-assemble-beta '(0 :start (add r1 r2 :start)) (hexs :00000000 :80211000))
  (test-assemble-beta '((BEQ 1 2)) (hexs :73e1ffff))
  (test-assemble-beta '((+ $ $)) '(0 0 0 0)) 
  (test-assemble-beta '((set $ 10) (+ $ $)) '(0 0 0 0 0 0 0 0 0 0 20 0))
  (test-assemble-beta '((betabr 0 0 0 0)) '(#xff #xff #x00 #x00))
  (test-assemble-beta '((betabr 0 0 4 0)) '(#xFF #xFF #x80 #x00))
  (test-assemble-beta '((betabr 0 4 0 0)) '(#xFF #xFF #x04 #x00))
  (test-assemble-beta '((betabr 4 0 0 0)) '(#xFF #xFF #x00 #x10))
  (test-assemble-beta '((% (+ (<< 0 26) (<< (% 0 32) 21)
                            (<< (% 0 32) 16)
                            (% (- (>> (- 4 $) 2) 1) 65536))
                         256))
                      '(0 0 0 0))
  
  (test-assemble-beta '((% (+ (<< 0 26) (<< (% 0 32) 21)
                            (<< (% 0 32) 16)
                            (% (- (>> (- 4 0) 2) 1) 65536))
                         256))
                      '(0 0 0 0))

  (test-assemble-beta '((% (>> (+ (<< 0 26) (<< (% 0 32) 21)
                                (<< (% 0 32) 16) (% (- (>> (- 4 $) 2) 1) 65536))
                            8)
                         256))
                      '(0 0 0 0))
  
  (test-assemble-beta '((% (>> (+ (<< 0 26) (<< (% 0 32) 21)
                                (<< (% 0 32) 16)
                                (% (- (>> (- 4 $) 2) 1) 65536))
                            16)
                         256))
                      '(0 0 0 0))
  
  (test-assemble-beta '((% (>> (>> (+ (<< 0 26) (<< (% 0 32) 21) (<< (% 0 32) 16)
                                    (% (- (>> (- 4 $) 2) 1) 65536))
                                16)
                            8)
                         256))
                      '(0 0 0 0))

  (test-assemble-beta '((betaopc 0 0 0 0)) '(#x00 #x00 #x00 #x00))
  (test-assemble-beta '((betaopc 0 0 0 4)) '(#x00 #x00 #x80 #x00))
  (test-assemble-beta '((betaopc 0 0 4 0)) '(#x04 #x00 #x00 #x00))
  (test-assemble-beta '((betaopc 0 4 0 0)) '(#x00 #x00 #x04 #x00))
  (test-assemble-beta '((betaopc 4 0 0 0)) '(#x00 #x00 #x00 #x10))
  (test-assemble-beta '((betaopc 4 4 4 4)) '(#x04 #x00 #x84 #x10))
  (test-assemble-beta '((betaopc 14 14 14 14)) '(#x0E #x00 #xCE #x39))

  ;;(defmacro BETABR (OP RA RC LABEL) (betaopc OP RA (- (>> (- LABEL $) 2) 1) RC))
  ;; OP=0 RA=0 RC=0 LABEL=4
  (test-assemble-beta '((- (>> (- 4 $) 2) 1)) '(0 0 0 0))
  (test-assemble-beta '((- (>> (- 0 $) 2) 1)) '(255 0 0 0))
  (test-assemble-beta '((betaopc 1 2 3 4)) '(#x03 #x00 #x82 #x04))

  (test-assemble-beta '((br :A)
                        (CMOVE 7 r0)
                        (CMOVE 7 r0)
                        :A
                        (CMOVE :A r0))
                      ;; this passes.
                      (hexs :73ff0002 :c01f0007 :c01f0007 :c01f000c))

  (test-assemble-beta '((push r0) (ADD r0 r0 r0))
                      (hexs :c3bd0004 :641dfffc :80000000))

  (test-assemble-beta ;; le-12-3
   ;; 
   '(
     :ones
     (push lp)
     (push bp)
     (move sp bp)
     (allocate 2)
     (push r1)
     (ld bp -12 r0)
     (andc r0 1 r1)
     (st r1 0 bp)
     (shrc r0 1 r1)
     (st r1 4 bp)

     :xx
     (BEQ R0 labl)

     :zz
     (LD BP 4 R1)
     (PUSH R1)
     
     :yy
     (BR ones LP)
     (DEALLOCATE 1)
     (LD BP 0 R1)
     (ADD R1 R0 R0)
     
     :labl
     (POP R1)
     (MOVE BP SP)
     (POP BP)
     (POP LP)
     (JMP LP))

   (hexs :c3bd0004 :679dfffc :c3bd0004 :677dfffc :837df800 :c3bd0008
         :c3bd0004 :643dfffc :601bfff4 :e0200001 :643b0000 :f4200001
         :643b0004 :73e00007 :603b0004 :c3bd0004 :643dfffc :739fffee
         :c7bd0004 :603b0000 :80010000 :603dfffc :c3bdfffc :83bbf800
         :637dfffc :c3bdfffc :639dfffc :c3bdfffc :6ffc0000))
  
  (test-assemble-beta '(:label_X_0 (LONG 0) (CMOVE 4 R0) (ST R0 :label_X_0))
                      (hexs :00000000 :c01f0004 :641f0000))

  (test-assemble-beta
   '(:f
     (PUSH LP)
     (PUSH BP)
     (MOVE SP BP)
     (ALLOCATE 1)
     (PUSH R1)

     (LD BP -12 R0)
     (ANDC R0 5 R1)
     (ST R1 0 BP)

     :xx
     (BEQ R0 bye)

     (SUBC R0 1 R0) 
     (PUSH R0)
     
     :yy
     (BR f LP)
     (DEALLOCATE 1)

     (LD BP 0 R1)
     (ADD R1 R0 R0)

     :bye
     (POP R1)
     (MOVE BP SP)
     (POP BP)
     (POP LP)
     (JMP LP))

   (hexs :c3bd0004 :679dfffc :c3bd0004 :677dfffc
         :837df800 :c3bd0004 :c3bd0004 :643dfffc
         :601bfff4 :e0200005 :643b0000 :73e00007
         :c4000001 :c3bd0004 :641dfffc :739ffff0
         :c7bd0004 :603b0000 :80010000 :603dfffc
         :c3bdfffc :83bbf800 :637dfffc :c3bdfffc
         :639dfffc :c3bdfffc :6ffc0000)) 


  
  (test-assemble-beta

   '((BR STEP1)  ;; start execution with Step 1

     ;; the array to be sorted
     :A
     (LONG 10) (LONG 56) (LONG 27) (LONG 69) (LONG 73) (LONG 99)
     (LONG 44) (LONG 36) (LONG 10) (LONG 72) (LONG 71) (LONG 1)

     (set ALEN (/ (- $ A) 4))  ;; determine number of elements in A

     ;; Please enter your code for each of the steps below...
     (set swapped r1)
     (set i r2)
     (set cur r3)
     (set prev r4)
     (set tmp r5)
     (set idx r6)

     :STEP1
     (CMOVE 0 swapped)

     :STEP2
     (CMOVE 0 i)

     :STEP3
     (ADDC i 1 i)
     (CMPLTC i 12 tmp) ;; 
     (BF tmp STEP5)
     
     
     :STEP4
     (MULC i 4 idx)
     (LD idx (- A 4) prev)
     (LD idx A cur)						
     (CMPLE prev cur tmp) ;; if A[i-1] <= A[i] then tmp=1 else tmp=0
     (BT tmp STEP3)     ;; if tmp == 1 then goto STEP3
     
     (ST prev A idx) 	  ;; swap A[i-1] and A[i]		
     (ST cur (- A 4) idx)		
     
     (CMOVE 1 swapped)   ;; set swapped to 1
     (BR STEP3)
     
     :STEP5

     (BT swapped STEP1) 


     :done
     (HALT))
   
   (hexs  :73ff000c :0000000a :00000038 :0000001b :00000045 :00000049
          :00000063 :0000002c :00000024 :0000000a :00000048 :00000047
          :00000001 :c03f0000 :c05f0000 :c0420001 :d4a2000c :73e50009
          :c8c20004 :60860000 :60660004 :98a41800 :77e5fff8 :64860004
          :64660000 :c03f0001 :73fffff4 :77e1fff1 :00000000))

  (test-assemble-beta     
   '( :fact
     (push lp)
     (push bp)
     (move sp bp)
     (push r1)
     (ld bp -12 r1)
     (cmplec r1 0 r0)
     (bt r0 else)

     (subc r1 1 r1)
     (push r1)
     (br fact lp)
     (deallocate 1)
     (ld bp -12 r1)
     (mul r1 r0 r0)
     (br rtn)

     :else
     (cmove 1 r0)

     :rtn
     (pop r1)
     (move bp sp)
     (pop bp)
     (pop lp)
     (jmp lp)

     
     )
   
   (hexs :c3bd0004 :679dfffc :c3bd0004 :677dfffc :837df800 :c3bd0004
         :643dfffc :603bfff4 :d8010000 :77e00008 :c4210001 :c3bd0004
         :643dfffc :739ffff2 :c7bd0004 :603bfff4 :88010000 :73ff0001
         :c01f0001 :603dfffc :c3bdfffc :83bbf800 :637dfffc :c3bdfffc
         :639dfffc :c3bdfffc :6ffc0000))


  )
