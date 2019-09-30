;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; task-scheduler.lisp --- task scheduler.
;;;

(in-package :datapump)

(defclass task ()
  ((id          :col-type integer   :initarg :id          :reader task-id    :col-default (:nextval 'task_id_seq))
   (name        :col-type string    :initarg :name        :reader task-name  :col-default "")
   (type        :col-type string    :initarg :type        :reader task-type)
   (group-id    :col-type string    :initarg :group-id    :reader task-group-id :col-default "")
   (source      :col-type integer   :initarg :source      :reader task-source :col-default 0)
   (function    :col-type string    :initarg :function    :reader task-function)
   (arguments   :col-type string    :initarg :arguments   :reader task-arguments)
   (start-time  :col-type timestamp :initarg :start-time  :reader task-start-time :col-default (:now))
   (end-time    :col-type (or s-sql:db-null timestamp) :accessor task-end-time)
   (failures    :col-type integer   :accessor task-failures   :col-default 0)
   (state       :col-type string    :initarg :state       :accessor task-state)
   (result      :col-type string    :accessor task-result     :col-default "")
   (cause       :col-type string    :accessor task-cause      :col-default ""))
  (:metaclass pgsql:dao-class)
  (:keys id))

(defvar *task-scheduler* (ext:make-scheduler))

(defun start-scheduler ()
  (pgsql:with-connection +db-conn-info+
    (pgsql:execute (:create-sequence 'task_id_seq))
    (pgsql:execute (pgsql:dao-table-definition 'task))
    (pgsql:query 
      (:update 'task :set 'state "pending"
               :where (:= 'state "running"))))

  (let ((delay 1))
    (ext:schedule-timer-relative *task-scheduler*
      (ext:make-timer #'schedule-tasks)
      delay delay)))

(defmacro function-to-string (func)
  `(concatenate 'string
                (package-name (symbol-package ,func)) 
                "::"
                (symbol-name ,func)))

(defun function-as-string (func-str)
  (let ((fence (search "::" func-str :from-end t)))
    (find-symbol (subseq func-str (+ fence 2)) (subseq func-str 0 fence))))

(defun make-task (function arguments group-id)
  (pgsql:with-connection +db-conn-info+
    (pgsql:insert-dao
      (make-instance 'task :type "promise"
                           :function (function-to-string function)
                           :arguments (format nil "~s" arguments)
                           :group-id group-id
                           :state "pending"))))

(defun schedule-tasks ()
  (pgsql:with-connection +db-conn-info+
    (dolist (task (concatenate 'list
                    (pgsql:query-dao 'task
                      (:limit
                        (:order-by
                          (:select '* :from 'task :where
                            (:= 'state "pending"))
                          'start-time)
                        10))
                    (pgsql:query-dao 'task 
                      (:limit 
                        (:order-by 
                          (:select '* :from 'task :where 
                            (:and 
                              (:= 'state "failed")
                              (:< 'failures 5)
                              (:< 'end-time (:- (:now) (simple-date:encode-interval :minute 3)))))
                          'start-time) 
                        10))))
      (if (< (pgsql:query (:select (:count 'id) :from 'task :where 
                              (:and 
                                (:= 'state "running")
                                (:= 'group-id (task-group-id task))))
                            :single) 10)
        (run-task task)))))

(defun run-task (task)
  (setf (task-state task) "running")
  (pgsql:update-dao task)
  (ext:chain 
    (apply 
      (function-as-string (task-function task))
      (read-from-string (task-arguments task)))
    (:then (result)
      (log:info "task ~d completed, result: ~s~%" (task-id task) result)
      (pgsql:with-connection +db-conn-info+
        (pgsql:query 
          (:update 'task 
                   :set 'state "completed" 
                        'end-time (:now) 
                        'result (format nil "~s" result)
                   :where (:= 'id (task-id task))))))
    (:catch (condition)
      (log:error "task ~d failed, condition: ~s~%" (task-id task) condition)
      (let ((stack-trace (ext:thread-stack-trace :start-frame-number 5)))
        (pgsql:with-connection +db-conn-info+
          (pgsql:query 
            (:update 'task 
                     :set 'state "failed" 'end-time (:now) 
                          'cause (format nil "~a~%signal-error: ~a, stack trace:~%~a" (task-cause task) condition stack-trace)
                          'failures (1+ (task-failures task))
                     :where (:= 'id (task-id task)))))))))
