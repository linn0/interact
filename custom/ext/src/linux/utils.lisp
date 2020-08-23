
(in-package #:ext)

(defclass socket-request ()
  ((type :initarg :type :initform nil :accessor socket-request-type)
   (state :initarg :state :initform :new :accessor socket-request-state)
   (buffer :initarg :buffer :initform nil :reader socket-request-buffer)
   (buffer-size :initarg :buffer-size :initform 0 :reader socket-request-buffer-size)
   (data-size :initarg :data-size :initform 0 :reader socket-request-data-size)
   (data :initarg :data :initform nil :accessor socket-request-data)
   (condition :initarg :condition :initform nil :accessor socket-request-condition)
   (callback :initarg :callback :initform nil :accessor socket-request-callback)
   (errback :initarg :errback :initform nil :accessor socket-request-errback)))

(def-foreign-type epoll_data_t
  (:union :epoll_data
     (:ptr :address)
     (:fd :signed-fullword)
     (:u32 :unsigned-fullword)
     (:u64 :unsigned-doubleword)))

(def-foreign-type epoll_event
  (:struct :epoll_event
     (:events :unsigned-fullword)
     (:data :epoll_data_t)))

(defconstant EPOLL_CTL_ADD 1)
(defconstant EPOLL_CTL_DEL 2)
(defconstant EPOLL_CTL_MOD 3)

(defconstant EPOLLIN    1)
(defconstant EPOLLPRI   2)
(defconstant EPOLLOUT   4)
(defconstant EPOLLERR   8)
(defconstant EPOLLHUP   16)
(defconstant EPOLLRDHUP 8192)
(defconstant EPOLLET    2147483648)

(defvar *socket-error-identifiers*
  (list #$EADDRINUSE :address-in-use
  #$ECONNABORTED :connection-aborted
  #$ENOBUFS :no-buffer-space
  #$ENOMEM :no-buffer-space
  #$ENFILE :no-buffer-space
  #$ETIMEDOUT :connection-timed-out
  #$ECONNREFUSED :connection-refused
  #$ENETUNREACH :host-unreachable
  #$EHOSTUNREACH :host-unreachable
  #$EHOSTDOWN :host-down
  #$ENETDOWN :network-down
  #$EADDRNOTAVAIL :address-not-available
  #$ENETRESET :network-reset
  #$ECONNRESET :connection-reset
  #$ESHUTDOWN :shutdown
  #$EACCES :access-denied
  #$EPERM :access-denied
  #$ENOENT :no-entry))

(defun make-socket-error (stream where errno)
  (setq errno (abs errno))
  (make-condition 'socket-error
     :stream stream
     :code errno
     :identifier (getf *socket-error-identifiers* errno :unknown)
     :situation where
     :format-control "~a (error #~d) during ~a"
     :format-arguments (list
            (ccl::%strerror errno)
            errno where)))

(defun linux-socket-error (stream where errno)
  "Creates and signals (via error) one of two socket error 
conditions, based on the state of the arguments."
  (format t "fd ~d (error #~d) during ~a~%" stream errno where)
  (error (make-socket-error stream where errno)))

(declaim (inline socket-call))
(defun linux-socket-call (stream where res)
  (if (< res 0)
    (linux-socket-error stream where res)
    res))
