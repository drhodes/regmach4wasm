;; -----------------------------------------------------------------------------
;; uasm


'((.include "beta.uasm")

  (br :STEP1) ;; start execution with Step 1

  ;; the array to be sorted
  :A
  (LONG 10) (LONG 56) (LONG 27) (LONG 69) (LONG 73) (LONG 99)
  (LONG 44) (LONG 36) (LONG 10) (LONG 72) (LONG 71) (LONG 1)

  ;; determine number of elements in A
  (= ALEN (/ (- $ A) 4)) 

  ;; Please enter your code for each of the steps below...
  ;; name some registers.
  (= swapped r1)
  (= i r2)
  (= cur r3)
  (= prev r4)
  (= tmp r5)
  (= idx r6)

  :STEP1 (cmove 0 swapped)
  :STEP2 (cmove 0 i)
  
  :STEP3 (addc i 1 i)  
  ;; if i > len(A) then goto STEP5
  (CMPLTC i 12 tmp)
  (BF tmp :STEP5)
  
  :STEP4
  (MULC i 4 idx)
  (LD idx A-4 prev)
  (LD idx A cur)						
  (CMPLE prev cur tmp) ;; if A[i-1] <= A[i] then tmp=1 else tmp=0
  (BT tmp :STEP3)       ;; if tmp == 1 then goto STEP3
  
  (ST prev A idx) ;; swap A[i-1] and A[i]		
  (ST cur A-4 idx)		
  
  (CMOVE 1 swapped) ;; set swapped to 1
  (BR :STEP3)
  
  :STEP5
  (BT swapped :STEP1)  

  :DONE
  ;; When step 5 is complete, execution continues with the checkoff
  ;; code.  You must include this code in order to receive credit for
  ;; completing the problem.
  .include "checkoff.uasm"
  
  )
