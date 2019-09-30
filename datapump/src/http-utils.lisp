
(in-package :datapump)

;;; ----------------------------------------------------

(defun push-text-content-type (resp)
  (let ((content-type (http:http-make-content-type "text" "html")))
    (setf (http:content-type-parameter content-type "charset") "utf-8")
    (http:content-type-push content-type resp)))
