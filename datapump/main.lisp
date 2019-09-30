;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; main.lisp --- internet data pump backend.
;;;
;;; (load "/home/postgres/dbapp/interact/datapump/main.lisp")
;;;

(require :asdf)
(defconstant +application-folder+ "/home/postgres/dbapp/interact/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc 
    (lambda (system)
      (push (format nil "~Acustom/~A/" +application-folder+ system)
        asdf:*central-registry*)
      (asdf:load-system system))
    '("ext" "log4cl" "cffi" "cl-ppcre")))

(mapc (lambda (dir) (push (namestring dir) asdf:*central-registry*))
  (uiop:subdirectories +application-folder+))
(mapc (lambda (system) (asdf:load-system system))
  '("simple-date" "cl-postgres" "s-sql" "postmodern"
    "plump-dom" "plump-lexer" "plump-parser" "plump" "clss" "salza2" "http" "datapump"))

(defun cleanup ()
  (log:debug "datapump application exitting")
  (log4cl:flush-all-appenders))

(defun init ()
  (let ((log-path (ext:concat +application-folder+ "datapump/logs/" "datapump-%Y%m%d.log")))
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
