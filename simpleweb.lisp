(asdf:oos 'asdf:load-op :hunchentoot)

(defpackage :testserv
  (:use :cl :hunchentoot)
  (:export :start-server))

(in-package :testserv)

;; Add a simple prefix dispatcher to the *dispatch-table*
(setq *dispatch-table*
      `(
        ,(create-prefix-dispatcher "/hello-world" 'hello-page)
        ,(create-prefix-dispatcher "/img" 'img-page)))

;; Handler functions either return generated Web pages as strings,
;; or write to the output stream returned by write-headers
(defun hello-page ()
  "<html><body>Hello Worlds!</body></html>")

(defun start-server (&key (port 4242))
  (start (make-instance 'easy-acceptor :port port)))

(testserv:start-server :port 4242)


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
