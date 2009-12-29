(in-package :cl-user)

(defpackage :webapp
  (:use :cl :asdf :webapp-config)
  (:export start))

(in-package :webapp)

(defsystem :webapp
  :depends-on (:hunchentoot :html-template)  
  :version "0.1"
  :serial t
  :components
  ((:file "global") ;;global variables and settings
   (:file "pages")
   (:file "control"))) ;;Starts the server (brings up hunchentoot)
