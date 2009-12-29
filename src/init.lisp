(require :asdf)
(require :asdf-install)

(defpackage :webapp-config
  (:use :cl)
  (:export *server-port* *swank-port* *webapp-home*))

(in-package :webapp-config)

;;TCP Ports for the HTTP server and Swank
(defvar *server-port* 8080)
(defvar *swank-port* 4006)

;;;Directories

;;it's unneccesary to specify *default-pathname-defaults*, since that's the
;;default value of 'default-pathname.' I just do it to be explicit.
;;the value of *default-pathname-defaults* (on posix-y systems, at least)
;;is the absolute path to the 'scripts' directory
(defvar *webapp-home* (merge-pathnames "../" *default-pathname-defaults*))

;;tell ASDF where to find 
(push (merge-pathnames "asdf-systems/" *webapp-home*) asdf:*central-registry*)

;;load up Swank, so we can connect to the lisp later with Slime
(asdf:oos 'asdf:load-op :swank)
(swank:create-server :port *swank-port* :dont-close t)

;;load our webapp
(asdf:oos 'asdf:load-op :webapp)
(webapp:start)