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

(defclass movie-item ()
  ((id        :col-type integer   :initarg :id      :reader movie-item-id :col-default (:nextval 'movie_item_id_seq))
   (site-id   :col-type integer   :initarg :site-id :reader movie-site-id)
   (title     :col-type string    :initarg :title   :reader movie-item-title)
   (actor     :col-type (or s-sql:db-null text[])   :initarg :actor     :reader movie-item-actor)
   (detail    :col-type (or s-sql:db-null string)   :initarg :detail    :reader movie-item-detail)
   (url       :col-type string    :initarg :url     :reader movie-item-url)
   (image-url :col-type (or s-sql:db-null string)   :initarg :image-url :reader movie-image-url)
   (play-url  :col-type (or s-sql:db-null string)   :initarg :play-url  :reader movie-play-url)
   (video-url :col-type (or s-sql:db-null string)   :initarg :video-url :reader movie-video-url)
   (image     :col-type (or s-sql:db-null string)   :initarg :image     :reader movie-item-image)
   (addtime   :col-type timestamp :initarg :addtime :reader movie-item-addtime :col-default (:now))
   (rating    :col-type real      :initarg :rating  :reader movie-item-rating :col-default 0))
  (:metaclass pgsql:dao-class)
  (:keys id))

(defclass movie-item-aux ()
  ((id        :col-type integer   :initarg :id        :reader movie-item-id)
   (title     :col-type tsvector  :initarg :title     :reader movie-title)
   (pattern   :col-type pattern   :initarg :pattern   :reader movie-image-pattern)
   (signature :col-type signature :initarg :signature :reader movie-image-signature))
  (:metaclass pgsql:dao-class)
  (:keys id))

(defun init-site-dao ()
  (pgsql:with-connection +db-conn-info+
    (pgsql:execute (:create-sequence 'site_id_seq))
    (pgsql:execute (pgsql:dao-table-definition 'site))

    (pgsql:execute (:create-sequence 'movie_item_id_seq))
    (pgsql:execute (pgsql:dao-table-definition 'movie-item))
    (pgsql:execute (:create-index 'movie_item_url_idx :on movie-item :using :hash :fields url))
    (pgsql:execute (:create-index 'movie_item_actor_idx :on movie-item :using :gin :fields actor))

    (pgsql:execute (pgsql:dao-table-definition 'movie-item-aux))
    (pgsql:execute (:create-index 'movie_item_imgsig_idx :on movie-item-aux :using :gist :fields signature))

    (dolist (site-args (list 
        '(:id 1 :url "http://www.aqdyb.com/" :name "爱情电影网-撸丝片" :url-path "/lusi/index~D.html")
        '(:id 2 :url "http://www.aqdyb.com/" :name "爱情电影网-伦理片" :url-path "/lunli/index~D.html")
        '(:id 3 :url "http://www.aqdyb.com/" :name "爱情电影网-社保片" :url-path "/shebao/index~D.html")
        '(:id 4 :url "http://www.aqdyb.com/" :name "爱情电影网-动漫片" :url-path "/lldm/index~D.html")))
      (pgsql:upsert-dao (apply #'make-instance 'site 
        (nconc site-args '(:real-url :null :detail :null :access-time :null)))))))
