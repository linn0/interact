
(in-package :datapump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :html))

;;; ----------------------------------------------------

(defun task-list (session resp)
  "Generate the page."
  (declare (ignore session))
  (let ((params (url:parse-query-string
                  (ccl:decode-string-from-octets 
                    (http:req-body (http:resp-request resp)) 
                    :external-format :utf-8))))
    (let ((site-id (parse-integer (second (assoc "site-id" params :test #'string-equal))))
          (start-page-no (parse-integer (second (assoc "start-page-no" params :test #'string-equal))))
          (end-page-no (parse-integer (second (assoc "end-page-no" params :test #'string-equal)))))
      (http-client-start :site-id site-id :start start-page-no :end end-page-no)))

  ;; set the response type
  (push-text-content-type resp)

  ;; build the page and return it
  (http:http-ok resp
           (html-render (<html>
                         (<head> (<title> "Task List"))
                         (<body>
                          (<h1> "task submited successfully")
                          (<hr>))))))
