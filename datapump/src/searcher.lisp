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

(defun query-items-with-actor (actor n)
  (pgsql:with-connection +db-conn-info+
    (pgsql:query-dao 'movie-item 
      (:limit 
        (:order-by 
          (:select '* :from 'movie-item :where
            (:@> 'actor (make-array 1 :initial-element actor))) 
          (:desc 'addtime))
        10 (* n 10)))))

(defun query-items-with-keywords (keywords n)
  (pgsql:with-connection +db-conn-info+
    (pgsql:query-dao 'movie-item 
      (:limit 
        (:select 'main.* :from (:as 'movie-item 'main) 
          :left-join (:as 'movie-item-aux 'aux) :on (:= 'main.id 'aux.id)
          :where (:@@ 'aux.title (:to_tsquery "chinese" keywords))) 
        10 (* n 10)))))

(defun query-items-with-imgid (imgid n)
  (pgsql:with-connection +db-conn-info+
    (pgsql:query-dao 'movie-item 
      (:limit 
        (:order-by
          (:select 'main.* :from (:as 'movie-item 'main) 
            :left-join 
              (:as 
                (:limit
                  (:order-by 
                    (:select 'id 
                      (:as (:<-> 'pattern 
                        (:select 'pattern :from 'movie-item-aux :where 
                          (:= 'id imgid))) 'smlr) 
                      :from 'movie-item-aux)
                    (:<-> 'signature 
                      (:select 'signature :from 'movie-item-aux :where 
                        (:= 'id imgid))))
                  100) 
                'aux) :on (:= 'main.id 'aux.id))
          (:asc 'aux.smlr)) 
        10 (* n 10)))))

(defun show-items (items)
  (html-render 
    (<html>
      (<head> (<title> "Images List")
        (<link> :href "https://cdn.bootcss.com/bulma/0.7.5/css/bulma.min.css" :rel "stylesheet"))
      (<body>
        (<div> :class "section container"
          (<form> :action "/search" :method "GET" :class "field has-addons"
            (<div> :class "control select"
              (<select> :name "type"
                (<option> :value "keyword" "keyword")
                (<option> :value "actor" "actor")
                (<option> :value "imgid" "image")))
            (<div> :class "control"
              (<input> :class "input" :type "text" :name "query" :style "width: 30rem" :placeholder "输入搜索词"))
            (<div> :class "control"
              (<button> :class "button is-primary" :type "submit" "搜索"))))
        (<div> :class "section container"
          (mapcar 
            (lambda (item) 
              (let ((item-url (movie-item-abs-url item)))
                (<div> :class "box media"
                  (<figure> :class "media-left"
                    (<img> :src (ext:concat "/images/" (movie-item-image item)) :height "150" :width "110"))
                  (<div> :class "media-content"
                    (<a> :href item-url (movie-item-title item))))))
            items))
        (<div> :class "section container"
          (<nav> :class "pagination" :role "navigation" :aria-label "pagination"
            (<a> :class "pagination-previous" "上一页")
            (<a> :class "pagination-next" "下一页")
            (<ul> :class "pagination-list")))))))

(defparameter *query-item-calls*
  '(("keyword" query-items-with-keywords)
    ("actor"   query-items-with-actor)
    ("imgid"   query-items-with-imgid)))

(defun search-keywords (session resp)
  "Generate the page."
  (declare (ignore session))
  (let* ((params (url:url-query (http:req-url (http:resp-request resp))))
         (type (second (assoc "type" params :test #'string-equal)))
         (func (second (assoc type *query-item-calls* :test 'string-equal)))
         (query (second (assoc "query" params :test #'string-equal)))
         (page (parse-integer (second (assoc "page" params :test #'string-equal)))))
    ;; set the response type
    (push-text-content-type resp)
    ;; build the page and return it
    (http:http-ok resp
      (show-items 
        (funcall func query page)))))
