;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; .ccl-init.lisp --- Clozure CL Init File.
;;;

(require :asdf)

(mapc (lambda (dir) (push (namestring dir) asdf:*central-registry*))
  (uiop:subdirectories "home:app;interact;base"))

(mapc (lambda (system) (asdf:load-system system))
  '("ext" "cl-ppcre" "simple-date" "cl-postgres" "s-sql" "postmodern"
    "plump-dom" "plump-lexer" "plump-parser" "plump" "clss" "salza2" "http"))