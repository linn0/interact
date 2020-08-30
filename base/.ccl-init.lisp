;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; .ccl-init.lisp --- Clozure CL Init File.
;;;

(require :asdf)

(mapc (lambda (dir) (push (namestring dir) asdf:*central-registry*))
  (uiop:subdirectories "home:app;interact;custom"))
(mapc (lambda (system) (asdf:load-system system))
  '("ext" "cffi" "log4cl" "cl-ppcre"))
