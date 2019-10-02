#|
 This file is a part of Data-Pump
 (c) 2017 Interact http://net.interact.com/ (vvk7@qq.com)
 Author: Xiaolingfeng <vvk7@qq.com>
|#

(defsystem datapump
  :name "Data-Pump"
  :version "0.0.0"
  :license "GPL"
  :author "Xiaolingfeng <vvk7@qq.com>"
  :maintainer "Xiaolingfeng <vvk7@qq.com>"
  :description "Internet Data Pump Backend Server."
  :long-description ""
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "site-dao")
                 (:file "task-scheduler")
                 (:file "http-client")
                 (:file "http-utils")
                 (:file "task-list")
                 (:file "searcher")
                 (:file "player")
                 (:file "http-server"))))
  :depends-on (:cl-postgres :s-sql :postmodern
               :plump-dom :plump-lexer :plump-parser :plump
               :clss :salza2 :http))
