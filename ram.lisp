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


(defun ram-fmt (ram)
    (let ((result (format nil "~%")))
      (loop for addr from 0 to (- (/ (ram-size ram) 4) 1)
            do
               (setf result (concatenate 'string
                                         result
                                         "0x"
                                         (ram-fmt-word ram (* 4 addr))
                                         (format nil "~%"))))
      result))
(ram-fmt (make-ram))

(defun ram-set (ram addr byte)
  (check-type ram ram)
  (when (> byte #xFF) (error "ram-set got a value for byte that is too big"))
  (when (< byte #x00) (error "ram-set got a value for byte that is too small"))
  (setf (aref (ram-bytes ram) addr) byte))

(defun ram-get (ram addr)
  (check-type ram ram)
  ;;(when (< 0 addr) (error "negative address found "))
  (aref (ram-bytes ram) addr))

(defun ram-size (ram)
  "get the total size of ram in bytes"
  (check-type ram ram)
  (length (ram-bytes ram)))

(defun ram-load-list (ram byte-list)
  (check-type ram ram)
  (let* ((idx 0))
    (dolist (byte byte-list)
      (ram-set ram idx byte)
      (incf idx))))


