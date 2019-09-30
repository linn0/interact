;;;; Example TODO Server
;;;;

(in-package :datapump)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :html))

;;; ----------------------------------------------------

(defun movie-item-abs-url (item)
  (pgsql:with-connection +db-conn-info+
    (let ((site-url (pgsql:query
                      (:select 'url :from 'site :where 
                        (:= 'id (movie-site-id item)))
                      :single)))
      (url:url-resolve site-url (movie-item-url item)))))

(defun image-files (n)
  (pgsql:with-connection +db-conn-info+
    (pgsql:query-dao 'movie-item 
      (:limit 
        (:order-by (:select '* :from 'movie-item) (:desc 'addtime))
        20 (* n 20)))))

(defun items (session resp)
  "Generate the page."
  (declare (ignore session))
  (let* ((params (url:url-query (http:req-url (http:resp-request resp))))
         (page (parse-integer (second (assoc "page" params :test #'string-equal)))))

    ;; set the response type
    (push-text-content-type resp)

    ;; build the page and return it
    (http:http-ok resp
      (html-render 
        (<html>
          (<head> (<title> "Images List")
            (<style> :type "text/css"
              "<!-- 
              #header { height: 40px; position: absolute; top: 0; width: 100%; 
                        font-size:12px; text-decoration:none; text-align: center; }
              #items { width:720px; margin:0 auto; margin-top:20px; padding:0; font-size:12px; text-decoration:none; }
              #items li { width:135px; height:200px; float:left; margin-left:20px; display:inline; } 
              #items li a { display:block; } 
              #items li a img { border:1px solid #666; padding:1px; } 
              #items li span a { width:135px; height:30px; line-height:24px; text-align:center; white-space:nowrap; 
                                 text-overflow:clip; overflow: hidden; font-size:12px; text-decoration:none; }
              -->"))
          (<body>
            (<span> :id "header" (format nil "第 ~D 页 " page)
              (<a> :href (format nil "/items?page=~D" (max 0 (1- page))) "[上一页]") #\space
              (<a> :href (format nil "/items?page=~D" (1+ page)) "[下一页]"))
            (<ul> :id "items"
              (mapcar 
                (lambda (item) 
                  (let ((item-url (movie-item-abs-url item)))
                    (<li>
                      (<a> :href item-url
                        (<img> :src (ext:concat "/images/" (movie-item-image item)) :height "170" :width "130"))
                      (<span>
                        (<a> :href item-url (movie-item-title item))))))
                (image-files page)))))))))
