;;;; ext.asd
;;;;  
;;;; Copyright 2017 linn0 <petagres@outlook.com>
;;;;
;;;; This software is released under the GPL License.
;;;; https://opensource.org/licenses/gpl-license

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :thread-support *features*))

(defsystem ext
  :author "linn0 <petagres@outlook.com>"
  :license "GPL"
  :description "Extended Common Lisp Programming Language Syntax."
  :components ((:module "src"
                :components
                ((:file "package")

                 (:module "syntax"
                  :components
                  ((:file "reader")
                   (:file "arrows")
                   (:file "definitions")
                   (:file "binding")
                   (:file "symbols")
                   (:file "macros")
                   (:file "control-flow")
                   (:file "functions")))

                 (:module "types"
                  :components
                  ((:file "conditions")
                   (:file "array")
                   (:file "strings")
                   (:file "hash-tables")
                   (:file "lists")
                   (:file "queue")))

                 (:module "streams"
                  :components
                  ((:file "in-memory")
                   (:file "stream")
                   (:file "logging")))
                 (:file "features")

                 (:module "parallel"
                  :components
                  ((:file "thread")))
                 (:module "promise"
                  :components
                  ((:file "syntax")
                   (:file "promise")
                   (:file "util")
                   (:file "pooler")))

                 (:module "timer"
                  :components
                  ((:file "time")
                   (:file "timers")
                   (:file "pqueue")
                   (:file "scheduler")))

                 #+linux-target
                 (:module "linux"
                  :components
                  ((:file "utils")
                   (:file "proactor")
                   (:file "async-socket")
                   (:file "async-ssl")))

                 #+windows-target
                 (:module "windows"
                  :components
                  ((:file "utils")
                   (:file "proactor")
                   (:file "async-socket")
                   (:file "async-ssl")))))))
