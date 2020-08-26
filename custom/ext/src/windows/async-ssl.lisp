
(in-package #:ext)

(defclass overlapped-ssl-request ()
  ((type :initarg :type :initform nil :reader overlapped-request-type)
   (state :initarg :state :initform :new :accessor overlapped-request-state)
   (buffer :initarg :buffer :initform nil :reader overlapped-request-buffer)
   (buffer-size :initarg :buffer-size :initform 0 :reader overlapped-request-buffer-size)
   (data-size :initarg :data-size :initform 0 :reader overlapped-request-data-size)
   (in-data :initarg :data :initform nil)
   (out-data :initarg :data :initform nil)
   (condition :initarg :condition :initform nil :accessor overlapped-request-condition)
   (callback :initarg :callback :initform nil :accessor overlapped-request-callback)
   (errback :initarg :errback :initform nil :accessor overlapped-request-errback)))

(defclass async-ssl-socket ()
  (device
   (completion-key :reader async-socket-completion-key)
   (remote-address :initarg :remote-address)
   (request-queue :initform (make-queue))
   (in-overlapped :initform (make-record :overlapped-extended))
   (out-overlapped :initform (make-record :overlapped-extended))
   (buffer :initform nil :accessor async-socket-buffer)
   (in-data :initform nil)
   (out-data :initform nil)

   (proactor :initform nil :accessor async-socket-proactor)
   (timer :initform nil :accessor async-socket-timer)
   (request-queue :initform (make-queue))
   (connect-timeout :initarg :connect-timeout :initform nil)
   (input-timeout :initarg :input-timeout :initform nil)
   (output-timeout :initarg :output-timeout :initform nil)

   (state :initform nil)
   (writing :initform nil)
   (reading :initform nil)
   (condition :initform nil)
   (callback ::initform nil)
   (errback :initform nil)

   (ssl :initarg :ssl :initform nil)
   (ssl-context :initarg :ssl-context :initform nil)
   (read-bio :initarg :read-bio :initform nil)
   (write-bio :initarg :write-bio :initform nil)
   (out-buff :initform nil)
   (out-buff-size :initform 0)
   (in-buff :initform nil)
   (in-buff-size :initform 0)))

(defmethod initialize-instance :after ((socket async-ssl-socket) &rest initargs)
  (declare (ignore initargs))
  (with-slots (device completion-key remote-address timer
               out-buff in-buff out-buff-size in-buff-size
               ssl ssl-context read-bio write-bio) socket
    (unless remote-address
      (error 'simple-error
             :format-control "~a"
             :format-arguments '("remote-address initarg nil")))
    (setq device (#_WSASocketA #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP +null-ptr+ 0 #$WSA_FLAG_OVERLAPPED))
    (setq completion-key (make-record :overlapped-io-completion-key :device (%int-to-ptr device)))
    (setq timer (make-timer (lambda () (cancel-async-io socket))))

    (setq out-buff-size +default-write-buffer-size+)
    (setq in-buff-size +default-read-buffer-size+)
    (setq out-buff (#_malloc out-buff-size))
    (setq in-buff (#_malloc in-buff-size))

    (setq ssl-context
      (external-call "SSL_CTX_new" :address
        (external-call "SSLv23_client_method" :address) :address))
    (setq ssl (external-call "SSL_new" :address ssl-context :address))

    (setq read-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))
    (setq write-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))
    (external-call "SSL_set_bio" ssl read-bio write-bio)))

(defmethod cancel-async-io ((socket async-ssl-socket))
  (with-slots (device type out-overlapped) socket
    (format t "~s ~d timeout during ~a~%" socket device
      (concatenate 'string "ASYNC-" (symbol-name type)))
    (if (eq 0 (external-call "CancelIoEx"
                :address (%int-to-ptr device)
                :address out-overlapped
                :signed-fullword))
      (let ((errno (ccl::%get-winsock-error)))
        (windows-socket-error device "CancelIoEx" errno)))))

(defmethod set-timeout-handler ((socket async-ssl-socket) timeout)
  (if (numberp timeout)
    (with-slots (proactor timer) socket
      (schedule-timer-relative (device-proactor-scheduler proactor) timer timeout))))

(defmethod remove-timeout-handler ((socket async-ssl-socket))
  (with-slots (proactor timer) socket
    (unschedule-timer (device-proactor-scheduler proactor) timer)))

(defmethod async-connect ((socket async-ssl-socket))
  (with-slots (device remote-address type callback errback in-overlapped connect-timeout) socket
    (let ((local-address (resolve-address :address-family :internet :host "0.0.0.0" :port 0)))
      (windows-socket-call device "bind"
        (ccl::c_bind device (ccl::sockaddr local-address) (ccl::sockaddr-length local-address))))
    (#_memset in-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
    (let ((func-ptr (c_get_connect_ex_func device)))
      (prog1 (create-promise
               (lambda (resolver rejecter)
                 (setf type :connect
                       callback resolver
                       errback rejecter)))
        (set-timeout-handler socket connect-timeout)
        ; actually, SOCKET can pass as it is, but without a related type designator
        (if (eq 0 (%ff-call func-ptr
                            :address (%int-to-ptr device)
                            :address (ccl::sockaddr remote-address)
                            :signed-fullword (ccl::sockaddr-length remote-address)
                            :address +null-ptr+ :unsigned-fullword 0 :address +null-ptr+
                            :address in-overlapped
                            :signed-fullword))
          (let ((errno (ccl::%get-winsock-error)))
            (if (/= errno (- #$WSA_IO_PENDING))
              (windows-socket-error device "ConnectEx" errno))))))))

(defmethod close ((socket async-ssl-socket) &key abort)
  (declare (ignore abort))
  (with-slots (device completion-key proactor
               out-overlapped in-overlapped out-buff in-buff) socket
    (remove-device proactor device)
    (#_closesocket device)
    (free completion-key)
    (free out-overlapped)
    (free in-overlapped)
    (remove-timeout-handler socket)
    (setq device nil
          completion-key nil
          out-overlapped nil
          in-overlapped nil)
    (#_free out-buff)
    (#_free in-buff)))

(defmethod handle-connect ((socket async-ssl-socket))
  (with-slots (device state ssl errback) socket
    ; remember to free the overlapped io buffer
    (let (value)
      (setf state :running)

      (setq value
        (external-call "SSL_set_fd" :address ssl :int device :int))

      (when (/= value 1)
        (remove-timeout-handler socket)
        (funcall errback (make-ssl-error device "ASYNC-CONNECT" value))
        (setf state :failed)
        (return-from handle-connect))

      ;; finished an operation
      (external-call "SSL_set_connect_state" :address ssl))))

(defmethod handle-read ((socket async-ssl-socket) bytes-transferred)
  (with-slots (device buffer data state condition ssl read-bio) socket
    ; remember to free the overlapped io buffer
    (let ((nwriten (external-call "BIO_write"
                                  :address read-bio :address buffer
                                  :signed-fullword bytes-transferred
                                  :signed-fullword)))
      (if (/= nwriten bytes-transferred)
        (let ((errno (external-call "SSL_get_error" :address ssl :int nwriten :int)))
          (remove-timeout-handler socket)
          (funcall errback (make-ssl-error device "BIO_write" errno))
          (setf state :failed)
          (return-from handle-read))))

    (let ((nread (external-call "SSL_read"
                                :address ssl :address buffer
                                :signed-fullword +default-read-buffer-size+
                                :signed-fullword)))
      (if (<= nread 0)
        (let ((errno (external-call "SSL_get_error" :address ssl :int nread :int)))
          (remove-timeout-handler socket)
          (funcall errback (make-ssl-error device "BIO_write" errno))
          (setf state :failed)
          (return-from handle-read)))

      (setf in-data (concatenate '(vector (unsigned-byte 8)) in-data
                   (make-vector-from-carray buffer nread)))

      (let ((position (funcall condition in-data)))
        (cond
          ((eq bytes-transferred 0)
            (setf (async-socket-buffer socket) nil)
            (funcall callback in-data)
            (setf state :completed))
          (position
            (setf (async-socket-buffer socket) (subseq in-data position))
            (funcall callback (subseq in-data 0 position))
            (setf state :completed)))))))

(defmethod handle-overlapped-entry ((socket async-ssl-socket) overlapped-entry)
  (let ((overlapped (pref overlapped-entry :overlapped-entry.overlapped)))
    (with-slots (device errback state) socket
      (remove-timeout-handler socket)
      ;; check whether operation completed, if then call the callback
      (rletz ((bytes-transferred #>DWORD))
        (if (eq 0 (#_GetOverlappedResult (%int-to-ptr device) overlapped bytes-transferred 0))
          (let ((errno (ccl::%get-winsock-error))
                (method (concatenate 'string "ASYNC-" (symbol-name state))))
            (funcall errback (make-socket-error device method errno)))
          (let ((bytes-transferred (pref bytes-transferred #>DWORD)))
            (handle-read socket bytes-transferred)
            (socket-send socket)
            (socket-receive socket)))))))

(defmethod socket-send ((socket async-ssl-socket))
  (with-slots (device out-data out-buff out-buff-size writing state
               out-overlapped output-timeout ssl write-bio) socket
    ; remember to free the overlapped io buffer
    (when (and (> (length out-data) 0) (not writing))
      (let* ((data-size (min out-buff-size (length out-data)))
             (buffer (#_malloc data-size)))
        (dotimes (index data-size)
          (setf (paref out-buff (:array :unsigned-byte) index) (aref data index))))
      (let ((nwriten (external-call "SSL_write"
                                    :address ssl :address out-buff
                                    :signed-fullword data-size
                                    :signed-fullword)))
        (if (/= nwriten data-size)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nwriten :int)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "SSL_write" errno))
            (return-from socket-send)))

        (setf data-size 0)
        (setf state :running)))

    (when (> (external-call "BIO_ctrl_pending" :address write-bio) 0)
      (let ((nread (external-call "BIO_read"
                                  :address write-bio :address buffer
                                  :signed-fullword buffer-size
                                  :signed-fullword)))
        (if (<= nread 0)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nread :int)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "BIO_read" errno))
            (return-from socket-send)))

        (setf data-size nread)
        (#_memset out-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
        (setf (pref out-overlapped :overlapped-extended.wsabuf.buf) buffer)
        (setf (pref out-overlapped :overlapped-extended.wsabuf.len) data-size)
        (set-timeout-handler socket output-timeout)
        ; actually, SOCKET can pass as it is, but without a related type designator
        (if (/= 0 (#_WSASend device (%inc-ptr out-overlapped (ccl::foreign-size :overlapped :bytes)) 1
                    +null-ptr+ 0 out-overlapped +null-ptr+))
          (let ((errno (ccl::%get-winsock-error)))
            (if (/= errno (- #$WSA_IO_PENDING))
              (windows-socket-error device "WSASend" errno))))))))

; send a array of binary data
(defmethod async-write-array ((socket async-ssl-socket) buffer buffer-size size)
  (with-slots (state callback errback) socket
    (assert (not state))
    (setf state :write)
    (prog1 (create-promise
             (lambda (resolver rejecter)
               (setf callback resolver
                     errback rejecter)))
      (handle-read socket bytes-transferred)
      (socket-send socket)
      (socket-receive socket))))

; send a vector of binary data
(defmethod async-write ((socket async-ssl-socket) data)
  (let* ((buffer-size (max +default-write-buffer-size+ (length data)))
         (buffer (#_malloc buffer-size)))
    (dotimes (index (length data))
      (setf (paref buffer (:array :unsigned-byte) index) (aref data index)))
    (async-write-array socket buffer buffer-size (length data))))

(defmethod socket-receive ((socket async-ssl-socket))
  (with-slots (device in-overlapped input-timeout
               data data-size buffer-size state type condition callback) socket
    ; remember to free the overlapped io buffer
    (#_memset in-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
    (setf (pref in-overlapped :overlapped-extended.wsabuf.buf) in-buff
          (pref in-overlapped :overlapped-extended.wsabuf.len) in-buff-size))

  (set-timeout-handler socket input-timeout)
  ; actually, SOCKET can pass as it is, but without a related type designator
  (if (/= 0
        (rletz ((recv-bytes #>DWORD)
                (flags #>DWORD))
          (#_WSARecv device (%inc-ptr in-overlapped (ccl::foreign-size :overlapped :bytes))
                1 recv-bytes flags in-overlapped +null-ptr+)))
    (let ((errno (ccl::%get-winsock-error)))
      (if (/= errno (- #$WSA_IO_PENDING))
        (windows-socket-error device "WSARecv" errno)))))

; receive a vector of binary data
(defmethod async-receive ((socket async-ssl-socket) type size cnd)
  (with-slots (state condition callback errback in-data) socket
    (let ((position (funcall condition in-data)))
      (assert (not state))
      (setf state type)
      (if position
        (prog1 (ext:promisify in-data)
          (let ((data (subseq in-data 0 position))
                (remain (subseq in-data position)))
            (setf in-data remain)
            (setf state nil)))
        (create-promise
          (lambda (resolver rejecter)
            (setf condition cnd
                  callback resolver
                  errback rejecter)))))))

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

(defun make-async-ssl-socket (&key
                      (address-family :internet)
                      remote-host remote-port
                      connect-timeout input-timeout output-timeout
                      keepalive
                      proactor)
  "Create and return a new async socket."
  (declare (ignore keepalive))
  (unless *foreign-libraries*
    (nconc *foreign-libraries*
      (mapcar (lambda (lib) (open-shared-library lib))
        '("C:/Program Files/OpenSSL-Win64/libcrypto-1_1-x64.dll"
          "C:/Program Files/OpenSSL-Win64/libssl-1_1-x64.dll")))

    (external-call "SSL_library_init")
    (external-call "SSL_load_error_strings")
    (external-call "ERR_load_BIO_strings"))

  (let ((socket (make-instance 'async-ssl-socket :remote-address
                  (resolve-address :address-family address-family
                                   :host remote-host :port remote-port)
                  :connect-timeout connect-timeout
                  :input-timeout input-timeout
                  :output-timeout output-timeout)))
    (if proactor
      (register proactor socket))
    socket))
