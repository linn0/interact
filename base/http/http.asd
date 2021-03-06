(defpackage :http-asd
  (:use :cl :asdf))

(in-package :http-asd)

(defsystem :http
  :name "http"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "HTTP interface for ClozureCL."
  :serial t
  :components ((:file "base64")
               (:file "sha1")
               (:file "parse")
               (:file "re")
               (:file "lexer")
               (:file "url")
               (:file "markup")
               (:file "html")
               (:file "http")
               (:file "headers")
               (:file "cookie")
               (:file "content-type")
               (:file "content-encoding")
               (:file "request")
               (:file "response")
               (:file "status")
               (:file "event-stream")

               ;; server files
               (:file "uuid")
               (:file "session")
               (:file "server")
               (:file "router")))
