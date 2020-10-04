(in-package #:regmach4wasm)
(declaim (optimize (debug 3))) 

(defparameter size-of-ram #x800000) ;; 8 MB.

(defstruct ram (bytes (make-array size-of-ram :element-type '(unsigned-byte 8))))

(defun ram-set (ram addr byte)
  (check-type ram ram)
  (when (> byte #xFF) (error "ram-set got a value for byte that is too big"))
  (when (< byte #x00) (error "ram-set got a value for byte that is too small"))
  (setf (aref (ram-bytes ram) addr) byte))

(defun ram-get (ram addr)
  (check-type ram ram)
  ;;(when (< 0 addr) (error "negative address found "))
  (aref (ram-bytes ram) addr))

(defun ram-load-list (ram byte-list)
  (check-type ram ram)
  (let* ((idx 0))
    (dolist (byte byte-list)
      (ram-set ram idx byte)
      (incf idx))))


