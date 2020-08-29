
(in-package #:ext)

(defclass overlapped-request ()
  ((type :initarg :type :initform nil :reader overlapped-request-type)
   (state :initarg :state :initform :new :accessor overlapped-request-state)
   (buffer :initarg :buffer :initform nil :reader overlapped-request-buffer)
   (buffer-size :initarg :buffer-size :initform 0 :reader overlapped-request-buffer-size)
   (data-size :initarg :data-size :initform 0 :reader overlapped-request-data-size)
   (data :initarg :data :initform nil :accessor overlapped-request-data)
   (condition :initarg :condition :initform nil :accessor overlapped-request-condition)
   (callback :initarg :callback :initform nil :accessor overlapped-request-callback)
   (errback :initarg :errback :initform nil :accessor overlapped-request-errback)))

(defclass async-socket ()
  (device
   completion-key
   (remote-address :initarg :remote-address)
   (request-queue :initform (make-queue) :reader async-socket-request-queue)
   (overlapped-record :initform (make-record :overlapped-extended))
   (buffer :initform nil :accessor async-socket-buffer)
   (proactor :initform nil)
   (timer :initform nil :accessor async-socket-timer)
   (connect-timeout :initarg :connect-timeout :initform nil)
   (input-timeout :initarg :input-timeout :initform nil)
   (output-timeout :initarg :output-timeout :initform nil)))

(defmethod initialize-instance :after ((socket async-socket) &rest initargs)
  (declare (ignore initargs))
  (with-slots (device completion-key remote-address overlapped-record timer) socket
    (format t "async socket start to initialize~%")
    (unless remote-address
      (error 'simple-error
             :format-control "~a"
             :format-arguments '("remote-address initarg nil")))
    (setq device (#_WSASocketA #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP +null-ptr+ 0 #$WSA_FLAG_OVERLAPPED))
    (setq completion-key (make-record :overlapped-io-completion-key :device (%int-to-ptr device)))
    (setq timer (make-timer (lambda () (cancel-async-io socket))))))

(defmethod async-socket-device ((socket async-socket))
  (slot-value socket 'device))

(defmethod async-socket-completion-key ((socket async-socket))
  (slot-value socket 'completion-key))

(defmethod async-socket-set-proactor ((socket async-socket) proactor)
  (setf (slot-value socket 'proactor) proactor))

(defmethod cancel-async-io ((socket async-socket))
  (with-slots (device request-queue overlapped-record) socket
    (let* ((request (queue-peek request-queue))
           (request-type (overlapped-request-type request)))
      (format t "~s ~d timeout during ~a~%" socket device 
        (concatenate 'string "ASYNC-" (symbol-name request-type))))
    (if (eq 0 (external-call "CancelIoEx"
                :address (%int-to-ptr device)
                :address overlapped-record
                :signed-fullword))
      (let ((errno (ccl::%get-winsock-error)))
        (windows-socket-error device "CancelIoEx" errno)))))

(defmethod set-timeout-handler ((socket async-socket) timeout)
  (if (numberp timeout)
    (with-slots (proactor timer) socket
      (schedule-timer-relative (device-proactor-scheduler proactor) timer timeout))))

(defmethod remove-timeout-handler ((socket async-socket))
  (with-slots (proactor timer) socket
    (unschedule-timer (device-proactor-scheduler proactor) timer)))

(defmethod async-connect ((socket async-socket))
  (with-slots (device remote-address request-queue overlapped-record connect-timeout) socket
    (let ((local-address (resolve-address :address-family :internet :host "0.0.0.0" :port 0)))
      (windows-socket-call device "bind" 
        (ccl::c_bind device (ccl::sockaddr local-address) (ccl::sockaddr-length local-address))))
    (#_memset overlapped-record 0 (ccl::foreign-size :overlapped-extended :bytes))
    (let ((func-ptr (c_get_connect_ex_func device)))
      (prog1 (create-promise
               (lambda (resolver rejecter)
                 (queue-push request-queue 
                   (make-instance 'overlapped-request 
                                  :type :connect
                                  :callback resolver
                                  :errback rejecter))))
        (set-timeout-handler socket connect-timeout)
        (format t "async socket start to connect~%")
        ; actually, SOCKET can pass as it is, but without a related type designator
        (if (eq 0 (%ff-call func-ptr
                            :address (%int-to-ptr device)
                            :address (ccl::sockaddr remote-address) 
                            :signed-fullword (ccl::sockaddr-length remote-address)
                            :address +null-ptr+ :unsigned-fullword 0 :address +null-ptr+
                            :address overlapped-record
                            :signed-fullword))
          (let ((errno (ccl::%get-winsock-error)))
            (if (/= errno (- #$WSA_IO_PENDING))
              (windows-socket-error device "ConnectEx" errno))))))))

(defmethod close ((socket async-socket) &key abort)
  (declare (ignore abort))
  (with-slots (device completion-key overlapped-record proactor) socket
    (remove-device proactor device)
    (#_closesocket device)
    (free completion-key)
    (free overlapped-record)
    (remove-timeout-handler socket)
    (setq device nil 
          completion-key nil
          overlapped-record nil)))

(defun finish-overlapped-request (request-queue)
  (let ((request (queue-peek request-queue)))
    (with-slots (state data buffer) request
      (setf state :completed)
      (setf data nil)
      (queue-pop request-queue :wait-p 1)
      (if buffer (#_free buffer)))))

(defmethod handle-overlapped-entry ((socket async-socket) overlapped-entry)
  (let ((overlapped (pref overlapped-entry :overlapped-entry.overlapped)))
    (with-slots (device request-queue) socket
      (let* ((request (queue-peek request-queue))
             (request-type (overlapped-request-type request)))
        (with-slots (data condition) request
          (remove-timeout-handler socket)
          ;; check whether operation completed, if then call the callback
          (rletz ((bytes-transferred #>DWORD))
            (if (eq 0 (#_GetOverlappedResult (%int-to-ptr device) overlapped bytes-transferred 0))
              (let ((errno (ccl::%get-winsock-error))
                    (method (concatenate 'string "ASYNC-" (symbol-name request-type))))
                (funcall (overlapped-request-errback request) 
                  (make-socket-error device method errno)))
              (let ((callback (overlapped-request-callback request))
                    (bytes-transferred (pref bytes-transferred #>DWORD)))
                (ecase request-type
                  (:connect
                    (format t "async socket connect succeed~%")
                    (funcall callback))
                  (:write
                    (format t "async socket sent ~D bytes~%" bytes-transferred)
                    (funcall callback bytes-transferred))
                  ((:read-some :read-until :read)
                    (format t "async socket received ~D bytes~%" bytes-transferred)
                    (let ((buffer (overlapped-request-buffer request)))
                      (setf data (concatenate '(vector (unsigned-byte 8)) data 
                                   (make-vector-from-carray buffer bytes-transferred)))
                      (let ((position (funcall condition data)))
                        (cond
                          ((eq bytes-transferred 0)
                            (setf (async-socket-buffer socket) nil)
                            (funcall callback data))
                          (position
                            (setf (async-socket-buffer socket) (subseq data position))
                            (funcall callback (subseq data 0 position)))
                          (t
                            (socket-receive socket)
                            (return-from handle-overlapped-entry))))))))))
          ;; finished an operation
          (finish-overlapped-request request-queue)))
      (let ((request (queue-peek request-queue)))
        (when (and request (eq (overlapped-request-state request) :new))
          (ecase (overlapped-request-type request)
            (:write (socket-send socket))
            ((:read-some :read-until :read) (socket-receive socket))))))))

(defmethod socket-send ((socket async-socket))
  (with-slots (device request-queue overlapped-record output-timeout) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue)))
      (setf (overlapped-request-state request) :running)
      (#_memset overlapped-record 0 (ccl::foreign-size :overlapped-extended :bytes))
      (setf (pref overlapped-record :overlapped-extended.wsabuf.buf) (overlapped-request-buffer request))
      (setf (pref overlapped-record :overlapped-extended.wsabuf.len) (overlapped-request-data-size request))
      (set-timeout-handler socket output-timeout)
      ; actually, SOCKET can pass as it is, but without a related type designator
      (if (/= 0 (#_WSASend device (%inc-ptr overlapped-record (ccl::foreign-size :overlapped :bytes)) 1
                  +null-ptr+ 0 overlapped-record +null-ptr+))
        (let ((errno (ccl::%get-winsock-error)))
          (if (/= errno (- #$WSA_IO_PENDING))
            (windows-socket-error device "WSASend" errno)))))))

; send a array of binary data
(defmethod async-write-array ((socket async-socket) array size)
  (with-slots (request-queue) socket
    (let* ((request (make-instance 'overlapped-request 
                                 :type :write
                                 :buffer array
                                 :data-size size))
           promise)
      (if 
        (and 
          (prog1 (queue-empty-p request-queue)
            (setq promise 
              (create-promise
                (lambda (resolver rejecter)
                  (setf (overlapped-request-callback request) resolver
                        (overlapped-request-errback request) rejecter)
                  (queue-push request-queue request)))))
          (eq (queue-peek request-queue) request))
            (socket-send socket))
      promise)))

; send a vector of binary data
(defmethod async-write ((socket async-socket) data)
  (async-write-array socket (make-carray-from-vector data) (length data)))

(defmethod socket-receive ((socket async-socket))
  (with-slots (device request-queue overlapped-record input-timeout) socket
    ; remember to free the overlapped io buffer
    (let ((request (queue-peek request-queue)))
      (with-slots (data data-size buffer-size state type condition callback) request
        (when (and (eq state :new) (> (length (async-socket-buffer socket)) 0))
          (setf data (async-socket-buffer socket))
          (let ((position (funcall condition data)))
            (when position
              (setf (async-socket-buffer socket) (subseq data position))
              (funcall callback (subseq data 0 position))
              (finish-overlapped-request request-queue)
              (return-from socket-receive))))
        (setf state :running)
        (#_memset overlapped-record 0 (ccl::foreign-size :overlapped-extended :bytes))
        (let (size)
          (ecase type
            (:read (setq size (min 
                                (- data-size (length data)) 
                                buffer-size)))
            ((:read-some :read-until) (setq size buffer-size)))
          (setf (pref overlapped-record :overlapped-extended.wsabuf.buf) (overlapped-request-buffer request)
                (pref overlapped-record :overlapped-extended.wsabuf.len) size)))

      (set-timeout-handler socket input-timeout)
      ; actually, SOCKET can pass as it is, but without a related type designator
      (if (/= 0 
            (rletz ((recv-bytes #>DWORD)
                    (flags #>DWORD))
              (#_WSARecv device (%inc-ptr overlapped-record (ccl::foreign-size :overlapped :bytes))
                    1 recv-bytes flags overlapped-record +null-ptr+)))
        (let ((errno (ccl::%get-winsock-error)))
          (if (/= errno (- #$WSA_IO_PENDING))
            (windows-socket-error device "WSARecv" errno)))))))

; receive a vector of binary data
(defmethod async-receive ((socket async-socket) type size condition)
  (with-slots (request-queue) socket
    (let* ((buffer-size (ecase type
                          (:read +default-read-buffer-size+)
                          ((:read-some :read-until) (if size size +default-read-buffer-size+))))
           (buffer (#_malloc buffer-size))
           (request (make-instance 'overlapped-request 
                                 :type type
                                 :buffer buffer
                                 :buffer-size buffer-size
                                 :data-size size
                                 :condition condition))
           promise)
      (if 
        (and 
          (prog1 (queue-empty-p request-queue)
            (setq promise 
              (create-promise
                (lambda (resolver rejecter)
                  (setf (overlapped-request-callback request) resolver
                        (overlapped-request-errback request) rejecter)
                  (queue-push request-queue request)))))
          (eq (queue-peek request-queue) request))
        (socket-receive socket))
      promise)))

; receive a vector of binary data
(defmethod async-read-some ((socket async-socket) &optional size)
  (async-receive socket :read-some size
    (lambda (data) (if data (length data)))))

; receive a vector of binary data
(defmethod async-read ((socket async-socket) size)
  (async-receive socket :read size 
    (lambda (data) (if (>= (length data) size) size))))

; receive a vector of binary data
(defmethod async-read-until ((socket async-socket) condition)
  (async-receive socket :read-until nil condition))

(defun make-async-socket (&key
                      (address-family :internet)
                      remote-host remote-port
                      connect-timeout input-timeout output-timeout
                      keepalive
                      proactor)
  "Create and return a new async socket."
  (declare (ignore keepalive))
  (let ((socket (make-instance 'async-socket :remote-address 
                  (resolve-address :address-family address-family 
                                   :host remote-host :port remote-port)
                  :connect-timeout connect-timeout
                  :input-timeout input-timeout
                  :output-timeout output-timeout)))
    (if proactor 
      (register proactor socket))
    socket))
