;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; http-client.lisp --- internet data pump backend.
;;;

(in-package :datapump)

(defconstant +db-conn-info+ '("postgres" "postgres" "postgres" "localhost" :pooled-p t))

(defclass site ()
  ((id          :col-type integer   :initarg :id       :reader site-id    :col-default (:nextval 'site_id_seq))
   (url         :col-type string    :initarg :url      :reader site-url)
   (real-url    :col-type (or s-sql:db-null string)    :initarg :real-url :reader site-real-url)
   (url-path    :col-type (or s-sql:db-null string)    :initarg :url-path :reader site-url-path)
   (name        :col-type string    :initarg :name     :reader site-name)
   (detail      :col-type (or s-sql:db-null string)    :initarg :detail  :reader site-detail)
   (access-time :col-type (or s-sql:db-null timestamp) :initarg :access-time :accessor site-access-time))
  (:metaclass pgsql:dao-class)
  (:keys id))

(defclass blog-item ()
  ((id        :col-type integer   :initarg :id      :reader movie-item-id :col-default (:nextval 'movie_item_id_seq))
   (site-id   :col-type integer   :initarg :site-id :reader movie-site-id)
   (title     :col-type string    :initarg :title   :reader movie-item-title)
   (author    :col-type string    :initarg :author  :reader movie-item-author)
   (content   :col-type (or s-sql:db-null string)   :initarg :content   :reader movie-item-content)
   (url       :col-type string    :initarg :url     :reader movie-item-url)
   (image-url :col-type (or s-sql:db-null string)   :initarg :image-url :reader movie-image-url)
   (image     :col-type (or s-sql:db-null string)   :initarg :image     :reader movie-item-image)
   (addtime   :col-type timestamp :initarg :addtime :reader movie-item-addtime :col-default (:now))
   (rating    :col-type real      :initarg :rating  :reader movie-item-rating :col-default 0))
  (:metaclass pgsql:dao-class)
  (:keys id))

(defun init-site-dao ()
  (pgsql:with-connection +db-conn-info+
    (pgsql:execute (:create-sequence 'site_id_seq))
    (pgsql:execute (pgsql:dao-table-definition 'site))

    (pgsql:execute (:create-sequence 'blog_item_id_seq))
    (pgsql:execute (pgsql:dao-table-definition 'blog-item))
    (pgsql:execute (:create-index 'blog_item_url_idx :on blog-item :using :hash :fields url))
    (pgsql:execute (:create-index 'blog_item_author_idx :on blog-item :using :gin :fields author))

    (dolist (site-args (list
        '(:id 1 :url "https://github.com/digoal/blog" :name "digoal's blog" :url-path "/tree/master/~A")
        '(:id 2 :url "http://www.pgsql.tech" :name "PostgreSQL中文技术" :url-path "/article_0?page=~D")))
      (pgsql:upsert-dao (apply #'make-instance 'site
        (nconc site-args '(:real-url :null :detail :null :access-time :null)))))))
