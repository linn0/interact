;;;; Example TODO Server
;;;;

(in-package :datapump)

(defconstant +public-folder+ "/home/postgres/app/interact/html")

;;; ----------------------------------------------------

(http:define-http-router http-url-router
  (:get "/search"  'search-query)
  (:get "/player"  'player)
  (:get "/taskmgr" 'task-manage))

;;; ----------------------------------------------------

(defun http-server-start ()
  (init-site-dao)
  (start-scheduler)
  (http:http-start-server 'http-url-router 
                          :public-folder +public-folder+ 
                          :home-page "/index.html"))
