
(in-package :datapump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :html))

;;; ----------------------------------------------------

(defun show-running-tasks ()
  (<table> :class "table"
    (<thead>
      (<tr>
        (mapcar
          (lambda (field) (<th> field))
          '("Id" "Function" "Arguments" "Start time" "Failures"))))
    (<tbody>
      (mapcar
        (lambda (task)
          (<tr>
            (<th> (task-id task))
            (<td> (task-function task))
            (<td> (task-arguments task))
            (<td>
              (multiple-value-bind (year month day hour minute second)
                  (simple-date:decode-timestamp (task-start-time task))
                (format nil "~D-~D-~D ~D:~D:~D" year month day hour minute second)))
            (<td> (task-failures task))))
        (pgsql:with-connection +db-conn-info+
          (pgsql:query-dao 'task
            (:order-by
              (:select '* :from 'task :where
                (:= 'state "running"))
              'start-time)))))))

(defun show-selectable-sites ()
  (<div> :class "control select"
    (<select> :name "site-id"
      (mapcar
        (lambda (site)
          (<option> :value (site-id site) (site-name site)))
        (pgsql:with-connection +db-conn-info+
          (pgsql:query-dao 'site
            (:select '* :from 'site)))))))

(defun show-tasks ()
  (<html>
    (<head> (<title> "系统任务管理器")
      (<link> :href "https://cdn.bootcss.com/bulma/0.7.5/css/bulma.min.css"
              :rel "stylesheet"))
    (<body>
      (<div> :class "section container"
        (<form> :action "/taskmgr" :method "GET" :class "field has-addons"
          (show-selectable-sites)
          (<div> :class "control"
            (<input> :class "input" :type "text" :name "start-page-no"
                     :style "width: 10rem" :placeholder "起始页号"))
          (<div> :class "control"
            (<input> :class "input" :type "text" :name "end-page-no"
                     :style "width: 10rem" :placeholder "结束页号"))
          (<div> :class "control"
            (<button> :class "button is-primary" :type "submit" "添加"))))
      (<div> :class "section container"
        (show-running-tasks)))))

(defun task-manage (session resp)
  "Generate the page."
  (declare (ignore session))
  (let* ((params (url:url-query (http:req-url (http:resp-request resp))))
         (site-id (second (assoc "site-id" params :test #'string-equal)))
         (start-page-no (second (assoc "start-page-no" params :test #'string-equal)))
         (end-page-no (second (assoc "end-page-no" params :test #'string-equal))))
      (if site-id
        (http-client-start
          :site-id (parse-integer site-id)
          :start (parse-integer start-page-no)
          :end (parse-integer end-page-no))))

  ;; set the response type
  (push-text-content-type resp)

  ;; build the page and return it
  (http:http-ok resp
    (html-render
      (show-tasks))))
