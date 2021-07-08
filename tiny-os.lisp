(in-package #:regmach4wasm)

(defparameter tiny-os-machine-code
  (assemble-with-beta
   ;; -----------------------------------------------------------------------------
   ;; tiny-os
   '(                                     
     (set $ vec-reset)          (br i-reset) ;; on reset (start-up)
     (set $ vec-ii)             (br i-illop) ;; on illegal instruct (eg SVC)
     (set $ vec-segfault)       (br i-segfault) ;; on segmentation fault (user-mode addr > bounds)
     (set $ vec-clk)            (br i-clk)      ;; on clock interrupt
     (set $ vec-kbd)            (br i-kbd)    ;; on keyboard interrup
     (set $ vec-mouse)          (br i-badint) ;; on mouse interrupt

     ;; The following macro is the first instruction to be entered for
     ;; each asynchronous I/O interrupt handler.  It adjusts XP (the
     ;; interrupted PC) to account for the instruction skipped due to
     ;; the pipeline bubble.
                                      
     (defmacro enter-interrupt () (sub xp 4 xp))
     
     :user-m-state (storage 32)

     (defmacro ss (R) (st r (+ user-m-state (* 4 r))))
     
     (defmacro save-state ()
       (ss 00) (ss 01) (ss 02) (ss 03) (ss 04) (ss 05) (ss 06) (ss 07)
       (ss 08) (ss 09) (ss 10) (ss 11) (ss 12) (ss 13) (ss 14) (ss 15)
       (ss 16) (ss 17) (ss 18) (ss 19) (ss 20) (ss 21) (ss 22) (ss 23)
       (ss 24) (ss 25) (ss 26) (ss 27) (ss 28) (ss 29) (ss 30))
     
     (defmacro rs (r) (ld (+ user-m-state (* 4 r)) r))
     
     (defmacro restore-state ()
       (rs 00) (rs 01) (rs 02) (rs 03) (rs 04) (rs 05) (rs 06) (rs 07)
       (rs 08) (rs 09) (rs 10) (rs 11) (rs 12) (rs 13) (rs 14) (rs 15)
       (rs 16) (rs 17) (rs 18) (rs 19) (rs 20) (rs 21) (rs 22) (rs 23)
       (rs 24) (rs 25) (rs 26) (rs 27) (rs 28) (rs 29) (rs 30))

     :k-stack
     (long (+ $ 4))                     ; pointer to the 
     (storage 256)                      ; kernel stack.

     ;; =============================================================================
     ;; Handler for the segmentation fault
     ;; =============================================================================
                                      
     :i-segfault
     (call :kernel-write-msg)           ; type out an error msg
     (.text "segmentation fault: address >= bounds register while executing instruction at user-mode address 0x")
                                      
     (move xp r0)
     (call :kernel-hex-print)
     (call :kernel-write-msg)
     (.text "! .....")
     (halt)                             ; crash the system

     ;; =============================================================================
     ;; Handler for unexpected interrupts
     ;; =============================================================================

     :i-badint
     (call :kernel-write-msg)           ; type out an error msg,
     (.text "unexpected interrupt...")
     (halt)
                                      
     ;; =============================================================================
     ;; Handler for Illegal Instructions                                                               
     ;;  (including SVCs)                                                                              

     :i-illop
     (save-state)                      ; save the machine state.
     (ld :k-stack sp)                  ; install kernel stack pointer.

     (sub xp 4 r0)             ; u-mode address of illegal instruction
     (call :map-user-address)  ; convert to k-mode address
     (ld r0 0 r0)              ; fetch the illegal instruction
     (shrc r0 26 r0)           ; extract the 6-bit opcode
     (shlc r0 2 r0)            ; make it a WORD (4-byte) index
     (ld r0 :uuo-table r0)     ; fetch uuo-table[opcode]
     (jmp r0)                  ; and dispatch to the UUO handler

     (defmacro uuo (addr) (long (+ addr pc-supervisor))) ; auxilary macros
     (defmacro bad () (uuo :uuo-error))

     :uuo-table
     (bad)        (uuo svc-uuo) (bad)        (bad)      
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     ;;
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     ;;
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     ;;
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)
     (bad)        (bad)         (bad)        (bad)

     ;; here's the handler for truly unused opcodes (not SVCs):
     :uuo-error
     (call :kernel-write-msg)           ; type out an error msg.
     (.text "Illegal instruction ")
     (ld xp -4 r0)                      ; giving hex instr and loc.
     (call :kernel-hex-print)
     (call :kernel-write-msg)
     (.text " at location 0x")
     (move xp r0)
     (call :kernel-hex-print)
     (call :kernel-write-msg)
     (.text "! .....")
     (halt)                             ; then crash system.
     
     ;; here's the common exit sequence from kernel interrupt
     ;; handlers: Restore registers, and jump back to the interrupted
     ;; user-mode program.

     :i-rtn (restore-state)
     :kexit (jmp xp)                 ; good place for debug breakpoint

     ;; Alternate return from interrupt handler which BACKS UP PC, and
     ;; calls the scheduler prior to returning. This causes the
     ;; trapped SVC to be re-executed when the process is eventually
     ;; rescheduled

     :i-wait
     (ld (+ user-m-state (* 4 30)) r0)  ; grab xp from saves-m-state
     (subc r0 4 r0)                     ; back it up to point to
     (st r0 (+ user-m-state (* 4 30)))  ; svc instruction
     
     (call scheduler)
     (br :i-rtn)

     ;; sub-handler for SVCs, called from :i-illop on SVC opcode
     
     :svc-uuo
     (ld xp -4 r0)                    ; the faulting instruction
     (andc r0 #x7 r0)                 ; pick out the low bits
     (shlc r0 2 r0)                   ; made a word index,
     (ld r0 svc-table r0)             ; and fetch the table entry
     (jmp r0)                         ; todo: describe where this goes

     :svc-table
     (uuo halt-h)
     (uuo write-msg-h)
     (uuo write-char-h)
     (uuo get-key-h)
     (uuo hex-print-h)
     (uuo wait-h)
     (uuo signal-h)
     (uuo yield-h)
     
     (defmacro halt       () (svc 0))   ; user-mode halt instruction
     (defmacro write-msg  () (svc 1))   ; write message
     (defmacro write-char () (svc 2))   ; write character
     (defmacro get-key    () (svc 3))   ; get key
     (defmacro hex-print  () (svc 4))   ; hex print
     (defmacro wait       () (svc 5))   ; wait s, s in r0
     (defmacro signal     () (svc 6))   ; signal s, s in r0
     (defmacro yield      () (svc 7))   ; yield

     ;; -----------------------------------------------------------------------------
     ;; Keyboard handling

     :key-state (long 0)

     :get-key-h
     (ld key-state r0)
     (beq r0 :i-wait)                   ; on 0, just wait a while

     ;; key ready, return it and clear the key buffer
     (ld key-state r0)                  ; fetch char to return
     (st r0 user-m-state)               ; return it in r0.
     (st r32 key-state)                 ; clear kbd buffer
     (br :i-rtn)

     ;; Interrupt side: read key, store it into buffer.  NB: This is a
     ;; lightweight interrupt handler, which doesn't do a full state
     ;; save. It doesn't have to, since (1) it only uses r0, and (2)
     ;; it always returns to the same process it interrupts.  By not
     ;; saving all state, it manages to save a lot of time: 20 stores
     ;; on entry, 40 loads on exit

     :i-kbd
     (enter-interrupt)                  ; adjust the pc!
     (st r0 user-m-state)               ; save only r0
     (read-char)                        ; read the character,
     (st r0 :key-state)                 ; save its code.
     (ld user-m-state r0)               ; restore r0, and
     (jmp xp)                           ; and return to the user.

     :write-char-h
     (ld user-m-state r0)               ; the user's r0
     (write-char)                       ; write out the character
     (br :i-rtn)                        ; then return

     :write-msg-h
     (ld (+ user-m-state (* 4 30)) r0)  ; fetch interrupted xp, then
     (call :kernel-msg-aux)             ; print text following SVC
     (st r0 (+ user-m-state (* 4 30)))  ; store updated xp
     (br :i-rtn)
     
     ;; handler for hex-print, print hex value from r0
     :hex-print-h
     (ld user-m-state r0)               ; load user r0
     (call :kernel-hex-print)           ; print it out
     (br :i-rtn)                        ; and return to user

     ;; -----------------------------------------------------------------------------
     ;; Timesharing: 3-process round-robin scheduler

     ;; proc-table contains a 31-word data structure for each process,
     ;; include r0-r30. r31, which always contains 0, is omitted.  The
     ;; xp (r30) value stored for each process is the pc and points to
     ;; the next instruction to be executed.

     ;; the kernel cariable cur-proc always points to the proc-table
     ;; entry corresponding to the swapped in process
     
     :proc-table
     (storage 29)                       ; process 0: r0-r28
     (long p0-stack)                    ; process 0: sp
     (long p0-stack)                    ; process 0: xp (= pc)

     (storage 29)                       ; process 1: r0-r28
     (long p0-stack)                    ; process 1: sp
     (long p0-stack)                    ; process 1: xp (= pc)
     
     (storage 29)                       ; process 2: r0-r28
     (long p0-stack)                    ; process 2: sp
     (long p0-stack)                    ; process 2: xp (= pc)
 
     :cur-proc
     (long :proc-table)


     ;; Schedule a new process.
     ;; Swaps current process out of user-m-state, swaps in a new one.

     :scheduler
     (push LP)
     (cmove user-m-state r0)
     (ld cur-proc r1)
     (call copy-m-state)               ; copy user-m-state -> cur-proc

     (ld :cur-proc r0)
     (addc r0 (* 4 30) r0)              ; increment to next process.
     (cmpltc r0 :cur-proc r1)           ; end of proc-table?
     (bt r1 :sched1)                    ; node, it's ok
     (cmove :proc-table r0)             ; yep, back to process 0.

     :sched1
     (st r0 :cur-proc)                  ; here's the new process.
     (addc r31 :user-m-state r1)        ; swap new process in
     (call :copy-m-state)
     (ld tics r0)                       ; reset tics-left counter
     (st r0 :tics-left)                 ; to tics
     (pop LP)
     (jmp LP)                           ; and return to caller
     

     ;; -----------------------------------------------------------------------------
     ;; copy a 31-word m-state structre from the address in t0 to that in t1
     ;; trashes r2, leave r0-r1 unchanged.

     (defmacro cm (n)
       (ld r0 (* n 4) r2)
       (st r2 (* n 4) r1))

     :copy-m-state
     (cm 01) (cm 02) (cm 03) (cm 04) (cm 05) (cm 06) (cm 07) (cm 08) (cm 09) (cm 10)
     (cm 11) (cm 12) (cm 13) (cm 14) (cm 15) (cm 16) (cm 17) (cm 18) (cm 19) (cm 20)
     (cm 21) (cm 22) (cm 23) (cm 24) (cm 25) (cm 26) (cm 27) (cm 28) (cm 29) (cm 30)
     (jmp LP)

     ;; -----------------------------------------------------------------------------
     ;; Clock interrupt handler: Invoke the scheduler.
     ;; -----------------------------------------------------------------------------                                 
     
     ;; Here's the deal:

     ;; Each compute-bound process gets a quantum consisting of TICS
     ;; clock interrupts, where TICS is the number stored in the
     ;; variable Tics below. To avoid overhead, we do a full state
     ;; save only when the clock intterup will cause a process swap,
     ;; using the TicsLeft variable as a counter.

     ;; We do a limit state save (r0 only) to free up a register, then
     ;; count down tics-left stored below. When it becomes negative,
     ;; we do a fullstate save and call the scheduler; otherwise we
     ;; just return, having burned only a few clock cycles on the interrupt.

     ;; recall that the call to scheduler set ticsleft to tics, giving
     ;; the newly-swapped-in process a full quantum.
     
     :tics (long 2)             ; number of clock interrupts / quantum
     :tics-left (long 0)        ; number tics left in this quantum

     :i-clk
     (enter-interrupt)                  ; adjust the pc!
     (st r0 user-m-state)               ; save r0 only, for now.
     (ld tics-left r0)                  ; count down tics-left
     (subc r0 1 r0)
     (st r0 tics-left)            ; now there's one less?
     (cmpltc r0 0 r0)             ; if the new value is negative, then
     (bt r0 do-swap)              ; swap processes
     (ld user-m-state r0)         ; else restore r0, and
     (jmp xp)                     ; return to same user

     :do-swap
     (ld user-m-state r0)
     (save-state)
     (ld k-stack sp)
     (call scheduler)
     (br i-rtn)
     
     ;; end of tiny-os
     )))


;; (format t "~s" tiny-os-machine-code)

;;      ;; =============================================================================
;;      ;; procedure to print out a zero-terminated message, packed one
;;      ;; char byte. Char data follows branch; returns to next 4-byte
;;      ;; aligned location. Saves all regs.
;;      :kernel-write-msg   
;;      (push r0)
;;      (move lp r0)
;;      (call :kernel-msg-aux)
;;      (move r0 lp)
;;      (pop r0)
;;      (rtn)  

;;      ;; Auxiliary routine for sending a message to the console.
;;      ;; On entry, R0 should point to data; on return, R0 holds next
;;      ;; longword aligned location after data.
;;      ;; Note: Must be called while in supervisor mode.
                                      
;;      :kernel-msg-aux                     
;;      (push r1)                           
;;      (push r2)
;;      (push r3)
;;      (push r4)
;;      (move r0 r1)
                                      
;;      :write-word
;;      (ld r1 0 r2)                       ; fetch a 4-byte word into r2
;;      (addc r1 4 r1)                     ; increment word pointer
;;      (cmove 4 r3)                       ; byte/word counter

;;      :write-byte
;;      (andc r2 #x7f r0)            ; grab next byte -- low end first!
;;      (beq r0 :write-end)          ; zero byte means end of text.
;;      (wrchar)                     ; print it.
;;      (srac r2 8 r2)               ; shift out this byte
;;      (subc r3 1 r3)               ; count down... done with this word?
;;      (bne r3 write-word)          ; nope, continue
;;      (br write-word)              ; yup, on to next
                                      
;;      :write-end
;;      (move r1 r0)
;;      (pop r4) (pop r3) (pop r2) (pop r1)
;;      (rtn)
                                      

                                      
;;      ;; .segment p0   
;;      :i-reset  ;; on reset (start-up)
;;      :i-illop  ;; on illegal instruct (eg SVC)
;;      :i-kbd    ;; on keyboard interrup
;;      :i-badint ;; on mouse interrupt
                                      
;;      ;; -----------------------------------------------------------------------------
;;      :kernel-hex-print
;;      :kernel-write-msg

;;      ;; translate user-mode address in r0 to kernel-mode address, return
;;      ;; result in r0. Adds the BASE value for the current process to R0,
;;      ;; simulating the effect of the base-and-bounds memory
;;      ;; management. You DO NOT have to implement the check against the
;;      ;; BOUNDS value
                                      
;;      :map-user-address
;;      (jmp lp)
     
