;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; pooler.lisp

(in-package #:ext)


;;;
;;; The 'POOL' Structure
;;;
;;; capacity - max number of pool-items a pool can have. This affects the concurrency.
;;; threshold - number of idle connections which a pool should maintain
;;; timeout - number of seconds of no use of pool before it is re-inited
;;; last-acccess - last access timestamp
;;;

(defstruct (pool (:constructor make-pool (&key name capacity item-maker item-destroyer)))
  (name "Default Pool" :type simple-string :read-only t)
  (queue (ext:make-queue))
  (item-maker nil :type function :read-only t)
  (item-destroyer nil :type function :read-only t)
  (capacity 20 :type fixnum)
  (count (make-array 1 :element-type 'integer
                       :initial-element 0)
                       :type (vector integer)))



;;; API

(defun new-pool-item (pool)
  "Creates a new POOL-ITEM using the item-maker funciton stored in the pool"
  (prog1
    (funcall (pool-item-maker pool))
    (ccl::atomic-incf (svref (pool-count pool) 0))))


(defun destroy-pool-item (pool item)
  "Destroys the POOL-ITEM using the item-destroyer funciton stored in the pool"
  (ccl::atomic-decf (svref (pool-count pool) 0))
  (funcall (pool-item-destroyer pool) item))

(defun pool-item-count (pool)
  "Return the approximate number of items in the queue."
  (svref (pool-count pool) 0))

(defun fetch-from (pool)
  "Tries a couple of times to fetch from pool. Grows the pool."
  (let ((item (ext:queue-pop (pool-queue pool))))
	  (if (null item)
		 (new-pool-item pool)
		 (ext:promisify item))))


(defun return-to (pool item)
  "Returns a pool object to the pool"
  (if (< (pool-item-count pool) (pool-capacity pool))
    (ext:queue-push (pool-queue pool) item)
  	(destroy-pool-item pool item))) ; we dont want to update last access here


;;; EOF
