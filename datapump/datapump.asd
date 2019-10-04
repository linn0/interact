#|
 This file is a part of Data-Pump
 (c) 2017 Interact http://net.interact.com/ (vvk7@qq.com)
 Author: linn0 <petagres@outlook.com>
|#

(defsystem datapump
  :name "Data-Pump"
  :version "0.0.0"
  :license "GPL"
  :author "linn0 <petagres@outlook.com>"
  :maintainer "linn0 <petagres@outlook.com>"
  :description "Internet Data Pump Backend Server."
  :long-description ""
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "site-dao")
                 (:file "task-scheduler")
                 (:file "http-client")
                 (:file "http-utils")
                 (:file "taskmgr")
                 (:file "searcher")
                 (:file "player")
                 (:file "http-server"))))
  :depends-on (:cl-postgres :s-sql :postmodern
               :plump-dom :plump-lexer :plump-parser :plump
               :clss :salza2 :http))
