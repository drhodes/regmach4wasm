(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

;; (defparameter size-of-ram #x800000) ;; 8 MB.
(defparameter size-of-ram #x800)

(defstruct ram (bytes (make-array size-of-ram :element-type '(unsigned-byte 8))))

(defun ram-fmt-word (ram addr)
  ;;(assert (eq 0 (mod addr 4)))
  (concatenate 'string
               (format nil "~2,'0X" (ram-get ram (+ 3 addr)))
               (format nil "~2,'0X" (ram-get ram (+ 2 addr)))
               (format nil "~2,'0X" (ram-get ram (+ 1 addr)))
               (format nil "~2,'0X" (ram-get ram (+ 0 addr)))))

(defun ram-size (ram)
  "get the total size of ram in bytes"
  (check-type ram ram)
  (length (ram-bytes ram)))

(defun ram-get (ram addr)
  (check-type ram ram)
  (when (< addr 0) (error "negative address found "))
  (aref (ram-bytes ram) addr))

;; (defun ram-get-word (ram addr)
;;   (check-type ram ram)  
;;   (ram-get (+ 0 (- addr (mod addr 4)))))

(defun ram-fmt (ram)  
  (check-type ram ram)
  (let ((result (format nil "~%")))
    (loop for addr from 0 to (- (/ (ram-size ram) 4) 1)
          do
             (setf result (concatenate 'string
                                       result
                                       "0x"
                                       (ram-fmt-word ram (* 4 addr))
                                       (format nil "~%"))))
    result))

(defun ram-set (ram addr byte)
  (check-type ram ram)
  (when (> byte #xFF) (error "ram-set got a value for byte that is too big"))
  (when (< byte #x00) (error "ram-set got a value for byte that is too small"))
  (setf (aref (ram-bytes ram) addr) byte))

(defun ram-load-list-at (ram idx byte-list)
  (check-type ram ram)
  (dolist (byte byte-list)
    (ram-set ram idx byte)
    (incf idx)))

(defun ram-load-list (ram byte-list)
  (ram-load-list-at ram 0 byte-list))


;; -----------------------------------------------------------------------------

(let ((mem (make-ram)))
  (ram-load-list mem '(1 2 3 4 5 6))
  (ram-fmt mem))

(let ((mem (make-ram)))
  (ram-load-list-at mem 1 '(1 2 3 4 5 6))
  (unless (eq 1 (ram-get mem 1)) (error "bad memory access"))
  (unless (eq 6 (ram-get mem 6)) (error "bad memory access")))



(let ((mem (make-ram)))
  (ram-set mem 0 3)
  (ram-set mem 1 4)
  (ram-set mem 2 5)
  (ram-fmt mem))


(let ((mem (make-ram)))
  (ram-set mem 0 3)
  (ram-set mem 1 4)
  (ram-set mem 2 5)
  
  (and (eq (ram-get mem 0) 3)
       (eq (ram-get mem 1) 4)
       (eq (ram-get mem 2) 5)))




