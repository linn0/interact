;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; http-client.lisp --- internet data pump backend.
;;;

(in-package :datapump)

(defvar *http-proactor* (ext:make-device-proactor))

(defun url-resolve (base reference)
  (if (or (ext:starts-with-p reference "javascript:")
          (string= reference "#"))
    reference
    (url:url-resolve base reference)))

(defun select-first (selector root-node)
  (aref (clss:select selector root-node) 0))

(defun parse-timestamp (time-string)
  (ppcre:register-groups-bind ((#'parse-integer year month date hour minute second))
        ("(\\d{4})\-(\\d{1,2})\-(\\d{1,2}) (\\d{1,2}):(\\d{1,2}):(\\d{1,2})" time-string)
      (simple-date:encode-timestamp year month date hour minute second)))

(defun http-url-encode (string)
  "Convert a string into a URL-safe, encoded string."
  (with-output-to-string (url)
    (flet ((encode-char (c)
             (if (find c #(#\Space #\Tab #\Linefeed #\Return))
                 (format url "%~16,2,'0r" (char-code c))
               (princ c url))))
      (map nil #'encode-char string))))

(defun http-url-group (string)
  (let ((url (url:url-parse string)))
    (format nil "~a:~d" (url:url-domain url) (url:url-port url))))

(defun http-parse-page-list (url)
  (ext:attach (http:http-async-get url *http-proactor*)
    (lambda (response)
      (let ((input (http:resp-body response)))
        (log:debug "html url: ~A, response size: ~D" url (length input))
        (map 'vector #'plump:text
          (clss:select "div.Box.mb-3 div.Box-row a"
            (plump:parse input)))))))

(defun http-client-start (&key site-id start end)
  (pgsql:with-connection +db-conn-info+
    (make-task 'http-parse-site (list site-id start end)
                                (format nil "~d" site-id))))

(defun http-parse-site (site-id start end)
  (let ((site (pgsql:get-dao 'site site-id)))
    (ext:attach (http:http-async-get (site-url site) *http-proactor*)
      (lambda (response)
        (let* ((real-url (print-object (http:req-url (http:resp-request response)) nil))
               (group-id (http-url-group real-url))
               (url-pattern (url:url-resolve real-url (site-url-path site))))
          (pgsql:with-connection +db-conn-info+
            (pgsql:query
              (:update 'site
                       :set 'real-url real-url
                            'access-time (:now)
                       :where (:= 'id site-id)))
            (loop as index from start to end do
              (let* ((id (if (eq index 1) "" index))
                     (url (format nil url-pattern id)))
                (make-task 'http-parse-html (list site-id url) group-id)))))))))

(defun http-parse-html (site-id url)
  (ext:attach (http:http-async-get url *http-proactor*)
    (lambda (response)
      (let ((input (http:resp-body response)))
        (log:debug "html url: ~A, response size: ~D" url (length input))
        (pgsql:with-connection +db-conn-info+
          (ccl:dovector (elem (clss:select "div#readme h5 a" (plump:parse input)))
            (let* ((item-url (url-resolve url (plump:attribute elem "href")))
                   (url-path (url:url-path (url:url-parse item-url))))
              (unless (pgsql:query
                        (:select 'id :from 'blog-item :where (:= 'url url-path))
                        :single)
                (make-task 'html-parse-item (list site-id item-url)
                  (http-url-group item-url))))))))))

(defun html-parse-item (site-id item-url)
  (ext:attach (http:http-async-get item-url *http-proactor*)
    (lambda (response)
      (let* ((input (http:resp-body response))
             (doc (plump:parse input))
             (url-path (url:url-path (url:url-parse item-url))))
        (log:debug "item url: ~A, response size: ~D" item-url (length input))
        (let ((title (plump:text (select-first "div#readme > article > h2" doc)))
              (author (plump:text
                        (clss:select "div.Box-header.Details div.css-truncate > span.text-bold" doc)))
              (content (plump:text (select-first "div#readme > article" doc)))
              (addtime (parse-timestamp
                         (plump:attribute
                           (select-first "div.Box-header.Details relative-time" doc)
                           "datetime"))))
          (let* ((blog (make-instance 'blog-item :site-id site-id
                                       :title title :author author :content content
                                       :url url-path :addtime addtime :rating rating)))
            (pgsql:with-connection +db-conn-info+
              (pgsql:save-dao blog))))))))

(defun http-parse-image (item-id)
  (let* ((blog (pgsql:get-dao 'blog-item item-id))
         (file-path (ext:concat +http-images-folder+ (blog-item-image blog)))
         (tmp-file (ext:concat file-path ".tmp")))
    (unless (probe-file file-path)
      (ext:attach (http:http-async-get
                    (http-url-encode (blog-image-url blog))
                    *http-proactor* :keep-alive t)
        (lambda (response)
          (let ((input (http:resp-body response)))
            (log:debug "image url: ~A, response size: ~D" (blog-image-url blog) (length input))
            (with-open-file (ofile tmp-file
                                   :direction :output
                                   :element-type 'unsigned-byte
                                   :if-exists :supersede)
              (write-sequence input ofile))
            (rename-file tmp-file file-path)))))
    (make-task 'http-post-process
      (list (blog-item-id blog)) "post-process-item")))

(defun http-post-process (item-id)
  (let* ((blog (pgsql:get-dao 'blog-item item-id))
         (img-path (ext:concat "log/images/" (blog-item-image blog)))
         (suffix
           (string-downcase
             (subseq img-path
               (1+ (position #\. img-path :from-end t))))))
    (unless (member suffix '("png" "gif" "jpeg") :test #'string=)
      (setq suffix "jpeg"))
    (pgsql:query
      (:insert-into 'blog-item-aux
        (:select
          (blog-item-id blog)
          (:as (:to_tsvector "chinese" (blog-item-title blog)) 'title)
          (:as (:shuffle_pattern 'pattern) 'pattern)
          (:as (:pattern2signature 'pattern) 'signature)
          :from
          (:as
            (:select
              (:as (:raw (format nil "~A2pattern(pg_read_binary_file('~A'))"
                                     suffix img-path))
                'pattern))
            'x))))))
