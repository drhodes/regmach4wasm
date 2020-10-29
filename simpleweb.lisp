(asdf:oos 'asdf:load-op :hunchentoot)

;; (defpackage :testserv
;;   (:use :cl :hunchentoot)
;;   (:export :start-server))
;; (in-package :testserv)

(in-package #:regmach4wasm)

;; Add a simple prefix dispatcher to the *dispatch-table*
(setq *dispatch-table*
      `(
        ,(create-prefix-dispatcher "/hello-world" 'hello-page)
        ,(create-prefix-dispatcher "/step-emu" 'step-emu)
        ,(create-prefix-dispatcher "/emu" 'hello-page)
        ,(create-prefix-dispatcher "/img" 'img-page)))

;; Handler functions either return generated Web pages as strings,
;; or write to the output stream returned by write-headers
(defun step-emu ()
  (emu-step test-emu)
  (hello-page))

(defun hello-page ()
  (format nil "<html><body> ~a </body></html>"
          (regfile-html (mcvm-regfile (emu-microcode-vm test-emu)))))

(defun fmt-reg (reg regfile)
  (format nil "<td>R~2,' d | ~8,'0x</td>" reg (regfile-get-reg regfile reg)))

(defun table-row (f i)
  (apply 'concatenate 'string
         (list  "<tr>"
                (funcall f (+ i 0)) (funcall f (+ i 8))
                (funcall f (+ i 16)) (funcall f (+ i 24))
                "</tr>")))

(defun regfile-html (regfile)
  (flet ((f (r) (fmt-reg r regfile)))
    (concatenate 'string
                 "<table border=0 style=\"width:700px\">"                 
                 (table-row #'f 0)
                 (table-row #'f 1)
                 (table-row #'f 2)
                 (table-row #'f 3)
                 (table-row #'f 4)
                 (table-row #'f 5)
                 (table-row #'f 6)
                 (table-row #'f 7)
                 "</table>")))

(regfile-html (make-regfile))

(defun start-server (&key (port 4243))
  (start (make-instance 'easy-acceptor :port port)))

;; (regmach4wasm:start-server :port 4243)

(defun img-page ()
  (setf (content-type*) "image/png")
  (let ((out (send-headers))   ; send-headers returns the output flexi-stream
        (bar-height (or (and (parameter "height") (parse-integer (parameter "height")))
                        150)))
    (with-canvas (:width 10 :height bar-height)
      (rectangle 0 0 10 bar-height)
      (set-gradient-fill 0 0
			 0 1 1 1
			 0 bar-height
			 1 0 0 1)
      (fill-and-stroke)
      (save-png-stream out)))) ; write the image data to the output stream obtained from send-headers
