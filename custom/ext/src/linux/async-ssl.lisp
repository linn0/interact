
(in-package #:ext)

(defclass async-ssl-socket ()
  (device
   (remote-address :initarg :remote-address)
   (request-queue :initform (make-queue))
   (buffer :initform nil :accessor async-ssl-socket-buffer)
   (proactor :initform nil)
   (timer :initform nil)
   (connect-timeout :initarg :connect-timeout :initform nil)
   (input-timeout :initarg :input-timeout :initform nil)
   (output-timeout :initarg :output-timeout :initform nil)
   (ssl :initarg :ssl :initform nil)
   (ssl-context :initarg :ssl-context :initform nil)))

(defmethod initialize-instance :after ((socket async-ssl-socket) &rest initargs)
  (declare (ignore initargs))
  (with-slots (device remote-address timer ssl-context ssl) socket
    (unless remote-address
      (error 'simple-error
             :format-control "~a"
             :format-arguments '("remote-address initarg nil")))
    (setq device (#_socket #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP))
    (ccl::set-socket-fd-blocking device nil)
    (setq timer (make-timer (lambda () (cancel-async-io socket))))
    (setq ssl-context
      (external-call "SSL_CTX_new" :address
        (external-call "SSLv23_client_method" :address) :address))
    (setq ssl (external-call "SSL_new" :address ssl-context :address))))

(defmethod async-socket-device ((socket async-ssl-socket))
  (slot-value socket 'device))

(defmethod async-socket-set-proactor ((socket async-ssl-socket) proactor)
  (setf (slot-value socket 'proactor) proactor))

(defmethod close ((socket async-ssl-socket) &key abort)
  (declare (ignore abort))
  (with-slots (ssl ssl-context device proactor) socket
    (remove-device proactor device)
    (external-call "SSL_shutdown" :address ssl)
    (external-call "SSL_free" :address ssl)
    (#_close device)
    (external-call "SSL_CTX_free" :address ssl-context)
    (remove-timeout-handler socket)
    (setq device nil)))

(defmethod cancel-async-io ((socket async-ssl-socket))
  (with-slots (device request-queue) socket
    (let ((request (queue-peek request-queue)))
      (if request
        (format t "~s ~d timeout during ~a~%" socket device 
          (concatenate 'string "ASYNC-" 
            (symbol-name (socket-request-type request))))))
    (if device
      (close socket))))

(defmethod set-timeout-handler ((socket async-ssl-socket) timeout)
  (if (numberp timeout)
    (with-slots (proactor timer) socket
      (schedule-timer-relative (device-proactor-scheduler proactor) timer timeout))))

(defmethod remove-timeout-handler ((socket async-ssl-socket))
  (with-slots (proactor timer) socket
    (unschedule-timer (device-proactor-scheduler proactor) timer)))

(defmethod async-connect ((socket async-ssl-socket))
  (with-slots (device proactor remote-address request-queue connect-timeout) socket
    (prog1 (create-promise
             (lambda (resolver rejecter)
               (queue-push request-queue 
                 (make-instance 'socket-request 
                                :type :connect
                                :callback resolver
                                :errback rejecter))
               (register-events proactor socket (logior EPOLLOUT EPOLLRDHUP))))
      (set-timeout-handler socket connect-timeout)
      ; actually, SOCKET can pass as it is, but without a related type designator
      (if (> 0 (#_connect device
                  (ccl::sockaddr remote-address) 
                  (ccl::sockaddr-length remote-address)))
        (let ((errno (ccl::%get-errno)))
          (if (/= errno (- #$EINPROGRESS))
            (linux-socket-error device "connect" errno)))))))

(defmethod finish-socket-request ((socket async-ssl-socket))
  (with-slots (proactor request-queue) socket
    (let ((request (queue-peek request-queue)))
      (with-slots (state data buffer type) request
        (setf state :completed)
        (setf data nil)
        (queue-pop request-queue :wait-p 1)
        (if buffer (#_free buffer))))))

(defmethod handle-events ((socket async-ssl-socket) events)
  (with-slots (device proactor request-queue) socket
    (let ((request (queue-peek request-queue)))
      (if request
        (ecase (socket-request-type request)
          (:connect
            (handle-connect socket events))
          (:handshake
            (handshake socket events))
          (:write
            (if (logand events EPOLLOUT)
              (send-data socket)))
          ((:read-some :read-until :read)
            (recv-data socket events)))))))

(defmethod handle-connect ((socket async-ssl-socket) events)
  (with-slots (device ssl proactor request-queue output-timeout) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue))
          value)
      (setf (socket-request-state request) :running)

      (setq value
        (external-call "SSL_set_fd" :address ssl :int device :int))

      (when (/= value 1)
        (remove-timeout-handler socket)
        (funcall (socket-request-errback request)
          (make-ssl-error device "ASYNC-CONNECT" value))
        (finish-socket-request socket)
        (return-from handle-connect))

      ;; finished an operation
      (external-call "SSL_set_connect_state" :address ssl)
      (setf (socket-request-type request) :handshake)
      (handshake socket events))))

(defmethod handshake ((socket async-ssl-socket) events)
  (declare (ignore events))
  (with-slots (device ssl proactor request-queue output-timeout) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue))
          value)
      (setf (socket-request-state request) :running)

      (with-slots (data data-size buffer buffer-size callback errback) request
        (setq value
          (external-call "SSL_do_handshake" :address ssl :int))

        (when (/= value 1)
          (let ((errno (external-call "SSL_get_error" :address ssl :int value :int)))
            (cond
              ((= errno +ssl-error-want-write+)
                (register-events proactor socket (logior EPOLLOUT EPOLLRDHUP)))
              ((= errno +ssl-error-want-read+)
                (register-events proactor socket (logior EPOLLIN EPOLLRDHUP)))
              (t
                (remove-timeout-handler socket)
                (funcall errback (make-ssl-error device "HANDSHAKE" errno))
                (finish-socket-request socket))))
          (return-from handshake))

        ;; finished an operation
        (remove-timeout-handler socket)
        (funcall callback)
        (finish-socket-request socket)))))

(defmethod send-data ((socket async-ssl-socket))
  (with-slots (device ssl proactor request-queue output-timeout) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue))
          nwriten)
      (setf (socket-request-state request) :running)

      (with-slots (data data-size buffer buffer-size callback errback) request
        (let* ((wdata (subseq data data-size))
               (size (length wdata)))
          (if (> size buffer-size)
            (setq size buffer-size))
          (dotimes (index size)
            (setf (paref buffer (:array :unsigned-byte) index) (aref wdata index)))
          (setq nwriten
            (external-call "SSL_write" :address ssl :address buffer :int size :int)))

        (if (< nwriten 0)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nwriten :int)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "ASYNC-WRITE" errno))
            (finish-socket-request socket)
            (return-from send-data)))

        (setf data-size (+ data-size nwriten))

        (if (> (length data) data-size)
          (return-from send-data))

        ;; finished an operation
        (remove-timeout-handler socket)
        (funcall callback data-size)
        (finish-socket-request socket)))))

; send a vector of binary data
(defmethod async-write ((socket async-ssl-socket) data)
  (create-promise
    (lambda (resolver rejecter)
      (with-slots (proactor request-queue output-timeout) socket
        (let ((buffer-size +default-write-buffer-size+))
          (queue-push request-queue
            (make-instance 'socket-request
              :type :write
              :buffer (#_malloc buffer-size)
              :buffer-size buffer-size
              :data data
              :data-size 0
              :callback resolver
              :errback rejecter)))
        (set-timeout-handler socket output-timeout)
        (register-events proactor socket (logior EPOLLOUT EPOLLRDHUP))))))

(defmethod recv-data ((socket async-ssl-socket) events)
  (with-slots (ssl device proactor request-queue) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue)))
      (with-slots (data data-size buffer-size type state condition callback errback) request

        (when (and (eq state :new) (> (length (async-ssl-socket-buffer socket)) 0))
          (setf data (async-ssl-socket-buffer socket))
          (let ((position (funcall condition data)))
            (when position
              (setf (async-ssl-socket-buffer socket) (subseq data position))
              (remove-timeout-handler socket)
              (funcall callback (subseq data 0 position))
              (finish-socket-request socket)
              (return-from recv-data))))

        (when (eq state :new)
          (setf state :running)
          (register-events proactor socket (logior EPOLLIN EPOLLRDHUP)))

        (if (logand events EPOLLIN)
          (let ((size) (nread))
            (ecase type
              (:read (setq size (min 
                                  (- data-size (length data)) 
                                  buffer-size)))
              ((:read-some :read-until) (setq size buffer-size)))
            (setq nread
              (external-call "SSL_read" :address ssl
                :address (socket-request-buffer request)
                :int size :int))

            (if (< nread 0)
              (let ((errno (external-call "SSL_get_error" :address ssl :int nread :int)))
                (when (/= errno +ssl-error-want-read+)
                  (remove-timeout-handler socket)
                  (funcall errback (make-ssl-error device "ASYNC-READ" errno))
                  (finish-socket-request socket))
                (return-from recv-data))

              (let ((buffer (socket-request-buffer request)))
                (setf data (concatenate '(vector (unsigned-byte 8)) data 
                             (make-vector-from-carray buffer nread)))
                (let ((position (funcall condition data)))
                  (cond
                    ((eq nread 0)
                      (setf (async-ssl-socket-buffer socket) nil)
                      (remove-timeout-handler socket)
                      (funcall callback data))
                    (position
                      (setf (async-ssl-socket-buffer socket) (subseq data position))
                      (remove-timeout-handler socket)
                      (funcall callback (subseq data 0 position)))
                    (t
                      (return-from recv-data)))
                  ;; finished an operation
                  (finish-socket-request socket))))))))))

; receive a vector of binary data
(defmethod async-receive ((socket async-ssl-socket) type size condition)
  (create-promise
    (lambda (resolver rejecter)
      (with-slots (request-queue proactor input-timeout) socket
        (let ((buffer-size 
                (ecase type
                  (:read +default-read-buffer-size+)
                  ((:read-some :read-until) (if size size +default-read-buffer-size+)))))
          (queue-push request-queue
            (make-instance 'socket-request 
              :type type
              :buffer (#_malloc buffer-size)
              :buffer-size buffer-size
              :data-size size
              :condition condition
              :callback resolver
              :errback rejecter)))
        (set-timeout-handler socket input-timeout)
        (register-events proactor socket (logior EPOLLIN EPOLLOUT EPOLLRDHUP))))))

; receive a vector of binary data
(defmethod async-read-some ((socket async-ssl-socket) &optional size)
  (async-receive socket :read-some size
    (lambda (data) (if data (length data)))))

; receive a vector of binary data
(defmethod async-read ((socket async-ssl-socket) size)
  (async-receive socket :read size 
    (lambda (data) (if (>= (length data) size) size))))

; receive a vector of binary data
(defmethod async-read-until ((socket async-ssl-socket) condition)
  (async-receive socket :read-until nil condition))