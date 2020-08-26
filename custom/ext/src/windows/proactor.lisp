
(in-package #:ext)

(defgeneric async-socket-device (socket)
  (:documentation "return socket device file descriptor."))

(defgeneric async-socket-completion-key (socket)
  (:documentation "return socket IO completion key."))

(defgeneric async-socket-set-proactor (socket proactor)
  (:documentation "set a socket's proactor."))

(defgeneric async-connect (socket)
  (:documentation "async connect a socket."))

(defgeneric socket-send (socket)
  (:documentation "async send data to a socket."))

(defgeneric socket-receive (socket)
  (:documentation "async receive data from a socket."))

(defgeneric async-write (socket data)
  (:documentation "async write data to a socket."))

(defgeneric async-receive (socket type size condition)
  (:documentation "async receive data from a socket."))

(defgeneric async-read (socket size)
  (:documentation "async read data from a socket."))

(defgeneric async-read-until (socket condition)
  (:documentation "async read data until condition  satisfied from a socket."))

(defgeneric async-read-some (socket &optional size)
  (:documentation "async read some data from a socket."))

(defgeneric handle-overlapped-entry (socket overlapped-entry)
  (:documentation "handle IO events on a socket."))

(defclass device-proactor ()
  ((completion-port :reader device-proactor-completion-port)
   (device-table :initform nil)
   (loop-thread :initform nil)
   (loop-timeout-in-millisecs :initform #$INFINITE)
   (scheduler :initform nil :reader device-proactor-scheduler)))

(defmethod initialize-instance :after ((proactor device-proactor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (completion-port device-table loop-thread scheduler) proactor
    (setq completion-port (#_CreateIoCompletionPort #$INVALID_HANDLE_VALUE +null-ptr+ 0 0))
    (setq device-table (make-hash-table))
    (setq loop-thread 
      (ccl:process-run-function "socket proactor loop thread" 
        (lambda () 
          (handler-bind
              ((error (lambda (e)
                        (let ((stack-trace (thread-stack-trace)))
                          (format t "loop events failed: ~A, stack trace:~%~A" e stack-trace)
                          (finish-output)))))
            (loop-events proactor)))))
    (setq scheduler (make-scheduler))))

(defmethod close ((proactor device-proactor) &key abort)
  (declare (ignore abort))
  (with-slots (completion-port device-table loop-thread scheduler) proactor
    (close scheduler)
    (maphash 
      (lambda (handle socket)
        (declare (ignore handle))
        (close socket))
      device-table)
    (#_CloseHandle completion-port)))

(defmethod register ((proactor device-proactor) socket)
  (with-slots (completion-port device-table) proactor
    (let ((handle (%int-to-ptr (async-socket-device socket))))
      (#_CreateIoCompletionPort handle completion-port
        (%ptr-to-int (async-socket-completion-key socket)) 0)
      (setf (gethash handle device-table) socket)
      (setf (async-socket-proactor socket) proactor))))

(defmethod remove-device ((proactor device-proactor) device)
  (with-slots (device-table) proactor
    (remhash (%int-to-ptr device) device-table)))

(define-condition simple-handle-error (simple-error)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-completion-entries-per-loop+ 64)
  (defconstant WINNT_ERROR_ABANDONED_WAIT_0 735))

(defmethod poll-events ((proactor device-proactor))
  (with-slots (completion-port device-table loop-timeout-in-millisecs) proactor
    (rletz ((overlapped-entries (:array :overlapped-entry 
                                             #.+max-completion-entries-per-loop+))
            (entries-removed #>ULONG))
      (if (eq 0 (external-call "GetQueuedCompletionStatusEx"
                  :address completion-port 
                  :address overlapped-entries 
                  :unsigned-fullword +max-completion-entries-per-loop+
                  :address entries-removed 
                  :unsigned-fullword loop-timeout-in-millisecs 
                  :signed-fullword 0
                  :signed-fullword))
        (let ((errno (ccl::%get-winsock-error)))
          (case (abs errno)
            (#.#$WAIT_TIMEOUT
              (return-from poll-events))
            ((#.#$ERROR_INVALID_HANDLE #.WINNT_ERROR_ABANDONED_WAIT_0)
              (error 'simple-handle-error
                     :format-control "requested to shutdown proactor"))
            (t
              (windows-socket-error completion-port "GetQueuedCompletionStatusEx" errno)))))
      (dotimes (i (pref entries-removed #>ULONG))
        (let ((overlapped-entry (paref overlapped-entries :overlapped-entry i)))
          (handle-overlapped-entry
            (gethash
              (pref
                (pref overlapped-entry :overlapped-entry.completion-key)
                :overlapped-io-completion-key.device)
              device-table)
            overlapped-entry))))))

(defmethod loop-events ((proactor device-proactor))
  (handler-case
    (loop (poll-events proactor))
    (simple-handle-error (condition)
      (format t "proactor stopped: ~s~%" condition))))

(defun make-device-proactor ()
  (make-instance 'device-proactor))
