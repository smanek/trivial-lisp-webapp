(in-package :webapp)

(hunchentoot:define-easy-handler (home :uri "/")
    ()
  (with-output-to-string (stream)
    (html-template:fill-and-print-template 
     #p "welcome.htmlf"
     nil
     :stream stream)))