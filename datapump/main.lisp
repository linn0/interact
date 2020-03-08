;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; main.lisp --- internet data pump backend.
;;;
;;; (load "/home/postgres/app/interact/datapump/main.lisp")
;;;

(defconstant +app-home+ "/home/postgres/app/interact/")

(mapc (lambda (dir) (push (namestring dir) asdf:*central-registry*))
  (uiop:subdirectories +app-home+))
(mapc (lambda (system) (asdf:load-system system))
  '("simple-date" "cl-postgres" "s-sql" "postmodern"
    "plump-dom" "plump-lexer" "plump-parser" "plump" "clss" "salza2" "http" "datapump"))

(defun cleanup ()
  (log:debug "datapump application exitting")
  (log4cl:flush-all-appenders))

(defun init ()
  (let ((log-path (ext:concat +app-home+ "datapump/logs/" "datapump-%Y%m%d.log")))
    (log:config :debug :daily log-path)
    (log:debug "datapump application started, log path: ~A" log-path))
  (push #'cleanup ccl:*lisp-cleanup-functions*))

(defun run ()
  (datapump:http-server-start))

(defun start ()
  "main entry function"
  (init)
  (run))

(start)
(sleep 600)
