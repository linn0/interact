;;;; 
;;;; scheduler.lisp
;;;; 
;;;; Created: 2003-11-07 by Zach Beane <xach@xach.com>
;;;; 
;;;; Controlling the queue of scheduled events and running expired
;;;; timers.
;;;; 
;;;; 
;;;; See the file COPYING for license information.
;;;; scheduler.lisp,v 1.9 2003/11/19 17:21:39 xach Exp

(in-package #:ext)

;;;
;;; Control messages
;;;

(defun make-control-message (command timer)
  (cons command timer))


(defun control-message-command (message)
  (car message))


(defun control-message-timer (message)
  (cdr message))


;;;
;;; Control stack
;;;

(defclass timer-scheduler ()
  ((control-queue :initform nil :reader scheduler-control-queue)
   (timer-queue :initform nil :reader scheduler-timer-queue)
   (loop-thread :initform nil)))


(defmethod next-control-message ((scheduler timer-scheduler) timeout)
  (queue-pop (scheduler-control-queue scheduler) :wait-p timeout))


(defmethod add-control-message ((scheduler timer-scheduler) message)
  (queue-push (scheduler-control-queue scheduler) message))


;;;
;;; Public interface (all manipulate the control stack)
;;;

(defmethod schedule-timer ((scheduler timer-scheduler) timer absolute-time &optional repeat-time)
  (setf (%timer-expire-time timer) (universal-time-to-precise-time absolute-time)
        (%timer-repeat-time timer) repeat-time)
  (add-control-message scheduler (make-control-message :schedule timer))
  (values))


(defmethod schedule-timer-relative ((scheduler timer-scheduler) timer relative-time &optional repeat-time)
  (setf (%timer-expire-time timer) (+ (get-precise-time) relative-time)
        (%timer-repeat-time timer) repeat-time)
  (add-control-message scheduler (make-control-message :schedule timer)))


(defmethod unschedule-timer ((scheduler timer-scheduler) timer)
  (add-control-message scheduler (make-control-message :unschedule timer)))


;;; Not public, but related

(defmethod reschedule-timer ((scheduler timer-scheduler) timer)
  (setf (%timer-expire-time timer) (+ (get-precise-time)
                                      (%timer-repeat-time timer)))
  (add-control-message scheduler (make-control-message :reschedule timer)))

;;;
;;; Expiring timers
;;;

(defmethod expire-timer ((scheduler timer-scheduler) timer)
  (with-slots (function repeat-time)
      timer
    (if (%timer-thread timer)
    	(ccl:process-run-function "timer function thread" function)
    	(funcall function))
    (when repeat-time
      (reschedule-timer scheduler timer))))


(defmethod expire-pending-timers ((scheduler timer-scheduler))
  (loop
   (let ((next-timer (peek-schedule scheduler)))
     (unless next-timer
       (return-from expire-pending-timers))
     (if (> (get-precise-time) (%timer-expire-time next-timer))
         (expire-timer scheduler (priority-queue-extract-maximum (scheduler-timer-queue scheduler)))
         (return-from expire-pending-timers)))))


;;;
;;; The timer scheduler
;;;

(defmethod initialize-instance :after ((scheduler timer-scheduler) &rest initargs)
  (declare (ignore initargs))
  (with-slots (control-queue timer-queue loop-thread) scheduler
    (setq control-queue (make-queue))
    (setq timer-queue (make-priority-queue :key #'%timer-expire-time))
    (setq loop-thread 
      (ccl:process-run-function "timer scheduler loop thread" 
        (lambda () 
          (handler-bind
              ((error (lambda (e)
                        (let ((stack-trace (thread-stack-trace)))
                          (format t "schedule timers failed: ~A, stack trace:~%~A" e stack-trace)
                          (finish-output)))))
            (schedule-timers scheduler)))))))

(defmethod close ((scheduler timer-scheduler) &key abort)
  (declare (ignore abort))
  (with-slots (loop-thread) scheduler
    (format t "scheduler thread stopped~%")
    (ccl:process-kill loop-thread)))

(defmethod peek-schedule ((scheduler timer-scheduler))
  (priority-queue-maximum (scheduler-timer-queue scheduler)))


(defmethod process-control-message ((scheduler timer-scheduler) message)
  (ecase (control-message-command message)
    ((:schedule :reschedule)
       (priority-queue-insert (scheduler-timer-queue scheduler) (control-message-timer message)))
     (:unschedule
       (priority-queue-remove (scheduler-timer-queue scheduler) (control-message-timer message)))))

(defmethod time-to-next-timer ((scheduler timer-scheduler))
  (let ((timer (peek-schedule scheduler)))
    (when timer
      (- (%timer-expire-time timer) (get-precise-time)))))


(defmethod schedule-timers ((scheduler timer-scheduler))
  (loop
    (expire-pending-timers scheduler)
    (let ((delay (time-to-next-timer scheduler)))
      (let ((message (next-control-message scheduler (if delay delay 1))))
        (if message
          (process-control-message scheduler message))))))

;;;
;;; Starting the system
;;;

(defun make-scheduler ()
  (make-instance 'timer-scheduler))
