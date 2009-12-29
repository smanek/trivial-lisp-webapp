(in-package :webapp)

;;set the template directory
(defvar *template-directory* (merge-pathnames "document-root/templates/" *webapp-home*))

;;and the static file path
(defvar *static-web-files* (merge-pathnames "document-root/static/" *webapp-home*))

;;set the template/static paths (arguably, could also be done in 'pages'
(setf html-template:*default-template-pathname* *template-directory*)

(push 
 (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-web-files*)
 hunchentoot:*dispatch-table*)