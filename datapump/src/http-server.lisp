;;;; Example TODO Server
;;;;

(in-package :datapump)

;;; ----------------------------------------------------

(http:define-http-router http-url-router
  (:get "/search"  'search-query)
  (:get "/taskmgr" 'task-manage))

;;; ----------------------------------------------------

(defun http-server-start ()
  (init-site-dao)
  (start-scheduler)
  (http:http-start-server 'http-url-router 
                          :public-folder +public-folder+ 
                          :home-page "/index.html"))
