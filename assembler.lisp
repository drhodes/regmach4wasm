(in-package #:regmach4wasm)
(declaim (optimize (debug 3)))

;; $ is assemble at next byte
(defun done (expr) (list 'pass-done expr))
(defun macro-name (expr) (cadr expr))
(defun macro-args (expr) (caddr expr))
(defun macro-body (expr) (cdddr expr))

(defun comment (x) (list 'comment x))
(defun comment? (x) (and (listp x) (eq 'comment (car x))))
(defconstant max-macro-args 100)

(defun put-macro (env sym mac)
  (if (env-contains? env sym)
      ;; get the macro vector index by number of args.
      (let ((vec (env-get env sym))
            (num-args (length (macro-args mac))))
        (setf (elt vec num-args) mac))
      ;; else allocate a vector for the macros.
      (progn
        (env-put env sym (make-array max-macro-args))
        (put-macro env sym mac))))

(defun get-macro (env sym num-args)
  (if (env-contains? env sym)
      (let ((vec (env-get env sym)))
        (elt vec num-args))))

(defun asm-eval-defmacro (env expr)
  (env-put env (macro-name expr) expr)
  (comment 'macrodef))

(defun label? (expr) (keywordp expr))

(defun done? (expr)
  (and (listp expr)
       (not (null expr))
       (eq (car expr) 'done)))

(defun cur-byte-addr (env)
  (if (byte-addr-paused? env)
      (env-get env 'paused-byte-addr)
      (env-get env 'cur-byte-addr)))

(defun increment-cur-byte (env n)
  (check-type env environment)
  (check-type n number)
  (let ((addr (cur-byte-addr env)))
    (env-put env 'cur-byte-addr (+ addr n))))


;; need figure out how bsim handles these differences.
(defun asm-eval-number (env expr)
  (check-type expr number)
  (when (< expr -128)
    (format nil "expression results must fit within one byte; interpreted ~
                 as signed integer, ~a is too negative."  expr))
  (when (> expr 255)
    (format nil "expression results must fit within one byte, ~a is too large." expr))
  expr)

(defun asm-eval-label (env expr)
  (env-put env expr (cur-byte-addr env))
  (comment expr))

(defun align? (expr) (and (listp expr)
                          (eq '.align  (car expr))))

(defun asm-eval-align (env expr)
  (if (equal expr '(.align))
      (asm-eval-align env '(.align 4))
      (if (eq 0 (mod (cur-byte-addr env)
                     (asm-eval env (cadr expr))))
          nil
          (progn
            (increment-cur-byte env 1)
            (cons 0 (asm-eval-align env expr))))))

(defun asm-eval-op (env op expr)
  (apply op (mapcar (lambda (x) (asm-eval env x))
                    (cdr expr))))

(defun asm-eval-instruction (env expr)
  ;; if the instruction contains a label
  ;; then try to replace the label  
  (let ((f (lambda (sym)
             (cond
               ;; if the sym is a register, then
               ((register-p sym) sym)
               ;; if the sym is a in the env
               ((env-contains? env sym) (env-get env sym))
               ;; then replace the sym with it's env var else error
               ;; out because eval instruction needs to be done after
               ;; all the labels have been determined.
               ;; (t (error (format nil "unknown argument in instruction: [[~a]], ~a" sym expr)))
               (t sym)))))
    (cons (car expr) (mapcar f (cdr expr)))))

(defun asm-eval-set-var (env expr)
  (let ((sym (cadr expr))
        (val (caddr expr)))
    (env-put env sym val)
    (comment 'setvar)))

(defun asm-eval-left-shift (env expr) 
  (ash (asm-eval env (cadr expr))
       (asm-eval env (caddr expr))))

(defun asm-eval-right-shift (env expr) 
  (ash (asm-eval env (cadr expr))
       (- (asm-eval env (caddr expr)))))

(defun asm-eval-mod (env expr) 
  (mod (asm-eval env (cadr expr))
       (asm-eval env (caddr expr))))

(defun $? (expr) (eq expr '$))

(defun asm-eval (env expr)
  (format t "asm ~a: ~a~%" (cur-byte-addr env) expr)
  (cond
    ((numberp expr) expr)
    (($? expr) (cur-byte-addr env))
    ((symbolp expr) (env-lookup env expr))
    ((listp expr) (case (car expr)
                    (+ (asm-eval-op env #'+ expr))
                    (- (asm-eval-op env #'- expr))
                    (* (asm-eval-op env #'* expr))
                    (/ (asm-eval-op env #'/ expr))
                    (>> (asm-eval-right-shift env expr))
                    (<< (asm-eval-left-shift env expr))                    
                    (% (asm-eval-mod env expr))))
    (t (error (format nil "unhandled expr: ~a" expr)))))

(defun set-cur-byte-addr (env n)
  (env-put env 'cur-byte-addr n))

;; pass: define the macros
;; pass: establish pause-unpause-$ delimeters for toplevel macro calls.
;; pass: expand the macros
;; pass: locate the labels
;; pass: 

(defun asm-eval-pass1 (env expr)
  (if (listp expr)
      (case (car expr)
        (defmacro  (asm-eval-defmacro env expr))
        (otherwise expr))
      expr))

(defun defined-macro? (env sym)
  (and (symbolp sym)
       (env-contains? env sym)       
       (let ((val (env-get env sym)))
         (eq 'defmacro (car val)))))

;; ---------------------------------
;; Expand one macro.
(defun asm-eval-pass2 (env expr)
  (if (and (listp expr)
           (defined-macro? env (car expr)))      
      ;; expand the macro
      (let* ((macrodef (env-get env (car expr)))
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
        (env-put env sym (asm-eval env val)))))


(defun match-set-current-byte? (item)
  (and (listp item)
       (> (length item) 2) 
       (equal 'set (car item))
       (equal '$ (cadr item))
       ))

(defun repeat (val n)
  (if (eq n 0) (list)
      (cons val (repeat val (- n 1)))))

(defun affix-locations (env prog)
  (if (null prog) (list)
      (let ((item (car prog)))
        (cond
          ((label? item) ;; track where the labels are
           (env-put env item (cur-byte-addr env))
           (affix-locations env (cdr prog)))

          ((align? item) ;; .align
           (append (asm-eval-align env item)
                   (affix-locations env (cdr prog))))
          
          ((match-set-current-byte? item)
           ;; val here needs to be eval'd because it could be an
           ;; arbitary expression
           (let* ((val (asm-eval env (caddr item)))
                  ;;asdf
                  (diff (- val (cur-byte-addr env))))
             (set-cur-byte-addr env val)
             (append (repeat 0 diff) (affix-locations env (cdr prog)))))
          
          ((set? item) ;; assignment           
           (affix-assignment env item)
           (affix-locations env (cdr prog)))

          ((comment? item) ;; comment            
           (affix-locations env (cdr prog)))

          ((or (pause? item) (unpause? item))
           ;; keep the pause statements without incrementing 
           (cons item (affix-locations env (cdr prog))))
          
          ;; otherwise
          (t (increment-cur-byte env 1)
             (cons item (affix-locations env (cdr prog))))))))


(defun assemble (program)
  (let ((root-env (make-environment)))
    (env-put root-env 'macro-namespace (make-environment))
    (set-cur-byte-addr root-env 0)
    (let* ((result1 (loop for expr in program
                          collect (asm-eval-pass1 root-env expr)))
           ;; fix design-mistake-1 (see docs)
           (result15 (wrap-macro-calls root-env result1))
           (result2 (repeat-expand-macro root-env result15))
           (result3 (affix-locations root-env result2)))
      (set-cur-byte-addr root-env 0)
      (mapcar (lambda (x) (asm-eval root-env x)) (replace-symbols root-env result3))
      )))

(defun test-assemble-temp (code)
  ;; "link" in the beta
  (assemble (append beta.uasm code)))

(test-assemble-temp '(1 2 3 4 5))
(test-assemble-temp '((ADD r1 r2 r3)))
(test-assemble-temp '((ADD $ $ $) (ADD $ $ $)))
(test-assemble-temp '((set $ (+ $ 8)) $))



(defun byte-addr-paused? (env)
  (env-get env 'cur-byte-addr-paused))

(defun pause$ (env)
  (env-put env 'paused-byte-addr (cur-byte-addr env))
  (env-put env 'cur-byte-addr-paused t))

(defun unpause$ (env)
  (env-put env 'cur-byte-addr-paused nil))

(defun pause? (item) (eq 'pause-incrementing-$ item))
(defun unpause? (item) (eq 'unpause-incrementing-$ item))


(defun replace-symbols (env prog)
  ;; pre-condition: env[cur-byte] should be 0 when calling this function for the
  ;; first time.
  (if (null prog) (list)
      (let ((item (car prog)))
        (cond          
          ((label? item) (error "all labels should have been dropped by now"))
          ((align? item) (error "all .align directives have have been dropped"))          
          ((comment? item) (error "all comments should have dropped in the last pass"))
          
          ((set? item) (if (eq '$ (cadr item))
                           (let ((padding (caddr item)))
                             (break padding))))
          
          ((pause? item)
           (pause$ env)
           (replace-symbols env (cdr prog)))
          
          ((unpause? item)
           (unpause$ env)
           (replace-symbols env (cdr prog)))
          
          ;; otherwise
          (t (env-put env '$ (cur-byte-addr env))
             (increment-cur-byte env 1)
             (cons (bind-vars env item)
                   (replace-symbols env (cdr prog))))))))

(defun test-assemble-code (code exp)
  (unless (equal exp (assemble code))
    (expected exp (assemble code))))

;;these tests for later
(progn
  (test-assemble-code '($ $ $ $) '(0 1 2 3))
  (test-assemble-code '($ $ $) '(0 1 2))
  (test-assemble-code '(1 (.align 5) 2) '(1 0 0 0 0 2))
  (test-assemble-code '(1 (.align) 2) '(1 0 0 0 2)))

(defun test-assemble-beta (code exp)
  ;; "link" in the beta
  (test-assemble-code (append beta.uasm code) exp))

;; these tests for later
'(progn
  (test-assemble-beta '((add r1 2 3))
   (list #x00 #x10 #x61 #x80))
  (test-assemble-beta '(:start (add r1 r2 :start))
   (list #x00 #x10 #x01 #x80))
  (test-assemble-beta '(0 :start (add r1 r2 :start))
   (list 0 0 0 0 #x00 #x10 #x21 #x80))
  (test-assemble-beta '((BEQ 1 2)) (list #xff #xff #xe1 #x73))
  
  (test-assemble-beta '((BEQ 1 :loop) :loop) (list #x00 #x00 #xe1 #x73))
  (test-assemble-beta '((betabr 1 2 3 4)) (list #x00 #x00 #x62 #x04))


  (test-assemble-beta '((ADD $ $ $)) '(0 0 0 #x80))
  (test-assemble-beta '((+ $ $)) '(0)) 
  (test-assemble-beta '((set $ 10) (+ $ $)) '(0 0 0 0 0 0 0 0 0 0 20))
  
  (test-assemble-beta '((betabr 1 2 3 4)) '(#x00 #x00 #x62 #x04))
  (test-assemble-beta '((betabr 0 0 0 0)) '(#xff #xff #x00 #x00))
  (test-assemble-beta '((betabr 0 0 0 4)) '(#x00 #x00 #x00 #x00))
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
  
  (test-assemble-beta '($ (betabr 0 0 0 4) $) '(#x00 #x00 #x00 #x00 #xFF #xFF 0 0 8))
  (test-assemble-beta '($ $ $ $ (betabr 0 0 0 4) $) '(#x00 #x01 #x02 #x03 #xFF #xFF 0 0 8))

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
  (test-assemble-beta '((>> 1 0) (>> 1 1) (>> 1 2) (>> 1 3)
                        (<< 1 0) (<< 1 1) (<< 1 2) (<< 1 3))
   '(1 0 0 0 1 2 4 8))

  
  
  )




;; (break (assemble (append beta.uasm '( :start (BEQ 1 2) :loop))))
;; (break (assemble (append beta.uasm '(0 :start (add r1 r2 :start)))))

(defun test1 () (assemble beta.uasm))
(test1)

;; (defun test2 ()
;;   (assemble
;;       '((set r0 0)
;;         (set r1 1)
;;         (set r2 2)
;;         (set A 42)
;;         1 2
;;         (.align)
;;         $ 5
;;         (defmacro short (x) (% x #x100) (% (>> x 8) #x100)) 
;;         (defmacro long (x) (short x) (short (>> x 16)))
;;         :start
;;         (+ A A)
;;         (ADD r0 r1 r2))))
