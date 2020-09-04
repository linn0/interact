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

(defun http-parse-page-list (site-id url)
  (ext:attach (http:http-async-get url *http-proactor*)
    (lambda (response)
      (let ((input (http:resp-body response)))
        (ext:debug "html url: ~A, response size: ~D" url (length input))
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
          (ccl:dovector (elem (clss:select "ul#contents li h5 a" (plump:parse input)))
            (let* ((item-url (url-resolve url (plump:attribute elem "href")))
                   (url-path (url:url-path (url:url-parse item-url))))
              (unless (pgsql:query
                        (:select 'id :from 'movie-item :where (:= 'url url-path))
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
        (let ((image-url (url-resolve item-url
                           (plump:attribute (select-first "div.detail-pic > img" doc) "src")))
              (title (plump:text (select-first "div.detail-title > h2" doc)))
              (actors (map 'vector #'plump:text (clss:select "div.info > dl > dd > a" doc)))
              (play-list (ext:join
                           (map 'list (lambda (elem) (plump:attribute elem "href"))
                             (clss:select "p.play-list a" doc))
                           :separator ","))
              (detail (plump:text (select-first "div#detail-intro div.detail-desc-cnt" doc)))
              (rating (parse-integer (plump:text (select-first "strong#pingfen" doc))))
              (addtime (parse-timestamp (plump:text (select-first "span#addtime" doc)))))
          (let* ((relative-path (ext:join (list site-id
                                  (subseq image-url (1+ (position #\/ image-url :from-end t))))
                                  :separator "/"))
                 (movie (make-instance 'movie-item :site-id site-id
                                       :title title :actor actors :detail detail
                                       :url url-path :image-url image-url :play-url play-list
                                       :image relative-path :addtime addtime :rating rating)))
            (pgsql:with-connection +db-conn-info+
              (pgsql:save-dao movie)
              (make-task 'http-parse-image
                (list (movie-item-id movie))
                (http-url-group (movie-image-url movie))))))))))

(defun http-parse-image (item-id)
  (let* ((movie (pgsql:get-dao 'movie-item item-id))
         (file-path (ext:concat +http-images-folder+ (movie-item-image movie)))
         (tmp-file (ext:concat file-path ".tmp")))
    (unless (probe-file file-path)
      (ext:attach (http:http-async-get
                    (http-url-encode (movie-image-url movie))
                    *http-proactor* :keep-alive t)
        (lambda (response)
          (let ((input (http:resp-body response)))
            (log:debug "image url: ~A, response size: ~D" (movie-image-url movie) (length input))
            (with-open-file (ofile tmp-file
                                   :direction :output
                                   :element-type 'unsigned-byte
                                   :if-exists :supersede)
              (write-sequence input ofile))
            (rename-file tmp-file file-path)))))
    (make-task 'http-post-process
      (list (movie-item-id movie)) "post-process-item")))

(defun http-post-process (item-id)
  (let* ((movie (pgsql:get-dao 'movie-item item-id))
         (img-path (ext:concat "log/images/" (movie-item-image movie)))
         (suffix
           (string-downcase
             (subseq img-path
               (1+ (position #\. img-path :from-end t))))))
    (unless (member suffix '("png" "gif" "jpeg") :test #'string=)
      (setq suffix "jpeg"))
    (pgsql:query
      (:insert-into 'movie-item-aux
        (:select
          (movie-item-id movie)
          (:as (:to_tsvector "chinese" (movie-item-title movie)) 'title)
          (:as (:shuffle_pattern 'pattern) 'pattern)
          (:as (:pattern2signature 'pattern) 'signature)
          :from
          (:as
            (:select
              (:as (:raw (format nil "~A2pattern(pg_read_binary_file('~A'))"
                                     suffix img-path))
                'pattern))
            'x))))))
