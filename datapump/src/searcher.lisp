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

(defun show-item (item)
  (let ((item-url (movie-item-abs-url item)))
    (<div> :class "box media"
      (<figure> :class "media-left"
        (<img> :src (ext:concat "/images/" (movie-item-image item))
               :height "150" :width "110"))
      (<div> :class "media-content"
        (<p> (<a> :href item-url (movie-item-title item)))
        (<p> (<em> "演员：")
          (map 'list
            (lambda (actor)
              (<a> :href (ext:concat "/search?type=actor&query=" actor)
                actor))
            (movie-item-actor item)))
        (<p> (<em> "类型：") (movie-site-id item))
        (<p> (<em> "更新时间：")
          (multiple-value-bind (year month day hour minute second)
              (simple-date:decode-timestamp (movie-item-addtime item))
            (format nil "~D-~D-~D ~D:~D:~D" year month day hour minute second)))
        (<p> (<a> :href
                    (format nil "/search?type=image&query=~D" (movie-item-id item))
                    "查看更多相似图片的内容..."))))))

(defun show-items (type query n items)
  (html-render 
    (<html>
      (<head> (<title> "更好看的内容，互动一下就知道！")
        (<link> :href "https://cdn.bootcss.com/bulma/0.7.5/css/bulma.min.css"
                :rel "stylesheet"))
      (<body>
        (<div> :class "section container"
          (<form> :action "/search" :method "GET" :class "field has-addons"
            (<div> :class "control select"
              (<select> :name "type"
                (<option> :value "keyword" "关键词")
                (<option> :value "actor" "演员")
                (<option> :value "image" "图片")))
            (<div> :class "control"
              (<input> :class "input" :type "text" :name "query"
                       :style "width: 30rem" :placeholder "输入搜索词"))
            (<div> :class "control"
              (<button> :class "button is-primary" :type "submit" "搜索"))))
        (<div> :class "section container"
          (mapcar #'show-item items))
        (<div> :class "section container"
          (<nav> :class "pagination" :role "navigation" :aria-label "pagination"
            (<a> :class "pagination-previous"
                 :href (format nil "/search?type=~A&page=~D&query=~A"
                         type (max 0 (1- n)) query)
                 "上一页")
            (<a> :class "pagination-next"
                 :href (format nil "/search?type=~A&page=~D&query=~A"
                         type (1+ n) query)
                 "下一页")
            (<ul> :class "pagination-list")))))))

(defun query-items-with-actor (query n)
  (show-items "actor" query n
    (pgsql:with-connection +db-conn-info+
      (pgsql:query-dao 'movie-item 
        (:limit 
          (:order-by 
            (:select '* :from 'movie-item :where
              (:@> 'actor (make-array 1 :initial-element query))) 
            (:desc 'addtime))
          10 (* n 10))))))

(defun query-items-with-keywords (query n)
  (show-items "keyword" query n
    (pgsql:with-connection +db-conn-info+
      (pgsql:query-dao 'movie-item 
        (:limit 
          (:select 'main.* :from (:as 'movie-item 'main) 
            :left-join (:as 'movie-item-aux 'aux) :on (:= 'main.id 'aux.id)
            :where (:@@ 'aux.title (:to_tsquery "chinese" query))) 
          10 (* n 10))))))

(defun query-image-similar-items (query n)
  (show-items "image" query n
    (let ((imgid (parse-integer query)))
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
                          (:as (:pattern_distance 'pattern 
                            (:select 'pattern :from 'movie-item-aux :where 
                              (:= 'id imgid))) 'smlr) 
                          :from 'movie-item-aux)
                        (:signature_distance 'signature 
                          (:select 'signature :from 'movie-item-aux :where 
                            (:= 'id imgid))))
                      100) 
                    'aux) :on (:= 'main.id 'aux.id))
              'aux.smlr)
            10 (* n 10)))))))

(defun query-items-main (query n)
  (show-items nil query n nil))

(defparameter *query-item-calls*
  '(("keyword" query-items-with-keywords)
    ("actor"   query-items-with-actor)
    ("image"   query-image-similar-items)
    ("main"    query-items-main)))

(defun search-query (session resp)
  "Generate the page."
  (declare (ignore session))
  (let* ((params (url:url-query (http:req-url (http:resp-request resp))))
         (type (second (assoc "type" params :test #'string-equal)))
         (func (second (assoc type *query-item-calls* :test 'string-equal)))
         (query (second (assoc "query" params :test #'string-equal)))
         (page (second (assoc "page" params :test #'string-equal))))

    ;; set the response type
    (push-text-content-type resp)
    ;; build the page and return it
    (http:http-ok resp
      (funcall func query
        (if page (parse-integer page) 0)))))
