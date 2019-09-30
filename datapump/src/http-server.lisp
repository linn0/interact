;;;; Example TODO Server
;;;;

(in-package :datapump)

(defconstant +public-folder+ "/home/postgres/dbapp/interact/html")

;;; ----------------------------------------------------

(http:define-http-router http-url-router
  (:get "/items" 'items)
  (:get "/player" 'player)
  (:post "/task-list" 'task-list))

;;; ----------------------------------------------------

(defun http-server-start ()
  (init-site-dao)
  (start-scheduler)
  (http:http-start-server 'http-url-router 
                          :public-folder +public-folder+ 
                          :home-page "/index.html"))
