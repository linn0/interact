
(in-package #:ext)

(defgeneric async-socket-device (socket)
  (:documentation "return socket device file descriptor."))

(defgeneric async-socket-set-proactor (socket proactor)
  (:documentation "set a socket's proactor."))

(defgeneric async-connect (socket)
  (:documentation "async connect a socket."))

(defgeneric send-data (socket)
  (:documentation "async send data to a socket."))

(defgeneric recv-data (socket events)
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

(defgeneric handle-events (socket events)
  (:documentation "handle IO events on a socket."))

(defclass device-proactor ()
  ((epollfd :reader device-proactor-epollfd)
   (device-table :initform nil)
   (loop-thread :initform nil)
   (loop-timeout-in-millisecs :initform -1)
   (scheduler :initform nil :reader device-proactor-scheduler)))

(defmethod initialize-instance :after ((proactor device-proactor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (epollfd device-table loop-thread scheduler) proactor
    (setq epollfd (external-call "epoll_create" :signed-fullword 64 :signed-fullword))
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
  (with-slots (epollfd device-table loop-thread scheduler) proactor
    (close scheduler)
    (maphash 
      (lambda (handle socket)
        (declare (ignore handle))
        (close socket))
      device-table)
    (#_close epollfd)))

(defun epoll_ctl (epollfd op device event)
  (if (> 0 (external-call "epoll_ctl"
             :signed-fullword epollfd
             :signed-fullword op
             :signed-fullword device
             :address event
             :signed-fullword))
    (linux-socket-error device "epoll_ctl" (ccl::%get-errno))))

(defmethod register ((proactor device-proactor) socket)
  (with-slots (epollfd device-table) proactor
    (let ((device (async-socket-device socket)))
      (setf (gethash device device-table) socket)
      (async-socket-set-proactor socket proactor)
      (let ((event (make-record :epoll_event :events 0)))
        (setf (pref event :epoll_event.data.fd) device)
        (epoll_ctl epollfd EPOLL_CTL_ADD device event)
        (free event)))))

(defmethod register-events ((proactor device-proactor) socket events)
  (with-slots (epollfd) proactor
    (let ((device (async-socket-device socket)))
      (let ((event (make-record :epoll_event :events events)))
        (setf (pref event :epoll_event.data.fd) device)
        (epoll_ctl epollfd EPOLL_CTL_MOD device event)
        (free event)))))

(defmethod remove-device ((proactor device-proactor) device)
  (with-slots (epollfd device-table) proactor
    (let ((event (make-record :epoll_event :events 0)))
      (setf (pref event :epoll_event.data.fd) device)
      (epoll_ctl epollfd EPOLL_CTL_DEL device event)
      (free event))
    (remhash device device-table)))

(define-condition simple-handle-error (simple-error)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-events-per-loop+ 64))

(defmethod poll-events ((proactor device-proactor))
  (with-slots (epollfd device-table loop-timeout-in-millisecs) proactor
    (rletz ((events (:array :epoll_event #.+max-events-per-loop+)))
      (let ((nfds
              (external-call "epoll_wait"
                :signed-fullword epollfd
                :address events
                :signed-fullword +max-events-per-loop+
                :signed-fullword loop-timeout-in-millisecs
                :signed-fullword)))
        (if (< nfds 0)
          (let ((errno (ccl::%get-errno)))
            (if (/= errno (- #$EINTR))
              (linux-socket-error epollfd "epoll_wait" errno)))
          (dotimes (i nfds)
            (let* ((event (paref events :epoll_event i))
                   (socket (gethash 
                             (pref event :epoll_event.data.fd)
                             device-table)))
              (if socket
                (handle-events socket
                  (pref event :epoll_event.events))))))))))

(defmethod loop-events ((proactor device-proactor))
  (handler-case
    (loop (poll-events proactor))
    (simple-handle-error (condition)
      (format t "proactor stopped: ~s~%" condition))))

(defun make-device-proactor ()
  (make-instance 'device-proactor))

(defun make-async-socket (&key
                          (address-family :internet)
                          (ssl nil)
                          remote-host remote-port
                          connect-timeout input-timeout output-timeout
                          keepalive
                          proactor)
  "Create and return a new async socket."
  (declare (ignore keepalive))

  (when (and ssl (null *foreign-libraries*))
    (nconc *foreign-libraries*
      (mapcar (lambda (lib) (open-shared-library lib))
        '("/lib64/libcrypto.so" "/lib64/libssl.so")))

    (external-call "SSL_library_init")
    (external-call "SSL_load_error_strings"))

  (let ((socket (make-instance
                  (if ssl
                    (find-class 'async-ssl-socket)
                    (find-class 'async-socket))
                  :remote-address
                    (resolve-address :address-family address-family
                                     :host remote-host :port remote-port)
                  :connect-timeout connect-timeout
                  :input-timeout input-timeout
                  :output-timeout output-timeout)))
    (if proactor
      (register proactor socket))
    socket))