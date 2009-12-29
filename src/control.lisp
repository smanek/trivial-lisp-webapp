(in-package :webapp)

(let ((server nil))
  (defun start ()
    (setf server (make-instance 'hunchentoot:acceptor :port *server-port*))
    (hunchentoot:start server)
    (format t "Webserver started on port ~A.~%" *server-port*))

  (defun stop ()
    (format t "Shutting down")
    (hunchentoot:stop server)
    (portable-quit)))