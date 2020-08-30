;;;; a lock-free queue implementation for CCL.

(in-package :ext)


(defconstant +cas-sleep+ (/ 1 internal-time-units-per-second)
  "The initial duration in seconds for which a thread should sleep
while in a CAS retry loop.")

(defconstant +max-cas-sleep+ 0.1
  "The maximum duration in seconds for which a thread in a CAS retry
loop should sleep.")


(defmacro cas (place old new)
  "Atomically attempt to set the new value of PLACE to be NEW if it
was EQ to OLD, returning non-nil if successful."
  `(ccl::conditional-store ,place ,old ,new))


(defmacro atomic-incf (place)
  "Atomically increment the value of PLACE.  For CCL, SBCL, and LW, it
should be an accessor form for a struct slot holding an integer."
  `(ccl::atomic-incf ,place))


(defmacro with-cas-retry (&body forms)
  "Execute FORMS with RETRY lexically bound to a function which sleeps
for the current CAS sleep interval before retrying FORMS.  The sleep
interval starts at +CAS-SLEEP+ and exponentially increases up to
+MAX-CAS-SLEEP+."
  (let ((b (gensym "BLOCK"))
        (r (gensym "RETRY"))
        (f (gensym "F"))
        (w (gensym "WAITTIME")))
    `(let ((,w +cas-sleep+))
       (block ,b
         (tagbody
          ,r
          (flet ((retry ()
                   (sleep ,w)
                   (setq ,w (min +max-cas-sleep+
                                 (* 1.5 ,w)))
                   (go ,r)))
            (flet ((,f () ,@forms))
              (return-from ,b (,f)))))))))


(defconstant +empty+ (if (boundp '+empty+)
                         (symbol-value '+empty+)
                       (gensym "EMPTY"))
  "sentinel for the value of an empty or popped queue cell.")


;; note: some implementations only support compare-and-swap on struct
;; slot places, so we use a struct for queue cells:

(defstruct (queue-cell
            (:constructor make-queue-cell (&optional value)))
  (value +empty+) ; a value or +empty+
  (next nil :type t)) ; a queue cell or nil


;; note: the queue counts can overflow/wraparound on 32-bit sbcl
(defstruct queue
  head
  tail
  (dequeued-count (make-array 1 :element-type 'integer
                                :initial-element 0)
                  :type (vector integer))
  (count (make-array 1 :element-type 'integer
                       :initial-element 0)
                       :type (vector integer)))


(defmethod print-object ((queue queue) stream)
  (format stream "(")
  (do ((index 0 (1+ index))
       (cur (queue-head queue) (queue-cell-next cur))) 
      ((null cur))
    (format stream "~A~S" (if (eq index 0) "" " ") (queue-cell-value cur)))
  (format stream ")"))


(defun queue-size (q)
  "Return the approximate number of items in the queue."
  (max 0 (svref (queue-count q) 0)))


(defun queue-push (queue item)
  "Add ITEM to the tail of QUEUE, returning as values ITEM and QUEUE."
  (symbol-macrolet
      ((head (queue-head queue))
       (tail (queue-tail queue)))
    (let ((new (make-queue-cell item)))
      (with-cas-retry
        (values (let ((cur tail))
                  ;; if we have no head, update it:
                  (cas head nil cur)

                  (cond
                   ;; no current tail, try to add:
                   ((null cur)
                    (unless (cas tail nil new)
                      (retry))
                    ;; if we had no tail, we had no head:
                    (setf head new))
                   ;; try to update old tail next pointer to point to
                   ;; the new item:
                   (t
                    (unless (cas (queue-cell-next cur) nil new)
                      (retry))
                    ;; if there was already a tail item in the queue,
                    ;; its next pointer now points to the new item.
                    ;; at this point, other threads trying to push
                    ;; items will retry until we update the tail
                    ;; pointer:
                    (setf tail new)))

                  ;; the tail has been updated, and the old tail cell
                  ;; now points to the new tail if the old tail
                  ;; existed.  if the old tail didn't exist, one
                  ;; thread will have succeeded in setting a new tail.

                  (ccl::atomic-incf (svref (queue-count queue) 0))

                  item)
                queue)))))


(defun queue-empty-p (queue)
  "Return non-nil when QUEUE is \(probably\) empty."
  (with-cas-retry
    (let ((head (queue-head queue)))
      (or (null head)
          ;; when a queue cell is popped, its value is set to the
          ;; sentinel value +empty+.  if we see that value, our view
          ;; of the queue is inconsistent and we must retry:
          (when (eq +empty+ (queue-cell-value head))
            (retry))))))


(defun queue-peek (queue)
  "Return a value \(if any\) which was recently stored in the head of
QUEUE."
  (with-cas-retry
    (let ((head (queue-head queue)))
      (when head
        (let ((v (queue-cell-value head)))
          (when (eq +empty+ v)
            (format t "retry ...~%")
            (retry))
          v)))))


(defun queue-dequeue (queue)
  "Dequeue the next item from the head of QUEUE."
  (symbol-macrolet
      ((head (queue-head queue))
       (tail (queue-tail queue)))
    (with-cas-retry
      (let ((cur head))
        (cond
         ((null cur)
          (values nil t))
         (t
          (let ((value (queue-cell-value cur))
                (next (queue-cell-next cur)))
            ;; if this happens, the head caught up with the tail at
            ;; some point in the past:
            (when (eq +empty+ value)
              (cas head cur next)
              (retry))

            ;; try to update head to point to next.  if another thread
            ;; already did, retry:
            (unless (cas head cur next)
              (retry))
            ;; the queue's head pointer has now been updated to point
            ;; to NEXT, which may be nil.  if nil, at this point
            ;; another thread may update the head slot.

            ;; mark old head cell as an empty cell:
            (assert (not (eq +empty+ (queue-cell-value cur))))
            (setf (queue-cell-value cur) +empty+)

            ;; TODO: if call cas failed
            (if (eq tail cur)
              (cas tail cur next))

            (ccl::atomic-decf (svref (queue-count queue) 0))

            (values value nil))))))))


(defun queue-pop (queue &key wait-p)
  "Pop the next item from QUEUE, returning as values the item which
was popped and whether the queue was empty.  If WAIT-P is a number,
busy-wait up to that many seconds to pop an item.  If WAIT-P is
otherwise non-nil, busy-wait forever."
  (cond
   (wait-p
    (let ((end-time (when (numberp wait-p)
                      (+ (get-internal-real-time)
                         (* wait-p internal-time-units-per-second)))))
      (block nil
        (with-cas-retry
          (multiple-value-bind (item empty-p)
              (queue-dequeue queue)
            (unless empty-p
              (assert (not (eq +empty+ item)))
              (return (values item empty-p))))
          (when (and end-time
                     (>= (get-internal-real-time) end-time))
            (return (values nil t)))
          (retry)))))

   (t
    (queue-dequeue queue))))
