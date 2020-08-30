
(in-package #:ext)

(defclass async-ssl-socket ()
  (device
   completion-key
   (remote-address :initarg :remote-address)
   (in-overlapped :initform (make-record :overlapped-extended))
   (out-overlapped :initform (make-record :overlapped-extended))
   (in-data :initform nil)
   (out-data :initform nil)

   (proactor :initform nil)
   (timer :initform nil)
   (connect-timeout :initarg :connect-timeout :initform nil)
   (input-timeout :initarg :input-timeout :initform nil)
   (output-timeout :initarg :output-timeout :initform nil)

   (state :initform nil)
   (writing :initform nil)
   (reading :initform nil)
   (closing :initform nil)
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
  (with-slots (device state completion-key remote-address timer
               out-buff in-buff out-buff-size in-buff-size
               ssl ssl-context read-bio write-bio) socket
    (format t "async ssl socket start to initialize~%")
    (unless remote-address
      (error 'simple-error
             :format-control "~a"
             :format-arguments '("remote-address initarg nil")))
    (setq device (#_WSASocketA #$AF_INET #$SOCK_STREAM #$IPPROTO_TCP +null-ptr+ 0 #$WSA_FLAG_OVERLAPPED))
    (setq completion-key (make-record :overlapped-io-completion-key :device (%int-to-ptr device)))
    (setq timer
      (make-timer
        (lambda ()
          (format t "~s ~d timeout during ~a~%" socket device
            (concatenate 'string "ASYNC-" (symbol-name state)))
          (cancel-async-io socket))))

    (setq out-buff-size +default-write-buffer-size+)
    (setq in-buff-size +default-read-buffer-size+)
    (setq out-buff (#_malloc out-buff-size))
    (setq in-buff (#_malloc in-buff-size))

    (setq ssl-context
      (external-call "SSL_CTX_new" :address
        (external-call "TLS_client_method" :address) :address))
    (setq ssl (external-call "SSL_new" :address ssl-context :address))

    (setq read-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))
    (setq write-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))))

(defmethod async-socket-device ((socket async-ssl-socket))
  (slot-value socket 'device))

(defmethod async-socket-completion-key ((socket async-ssl-socket))
  (slot-value socket 'completion-key))

(defmethod async-socket-set-proactor ((socket async-ssl-socket) proactor)
  (setf (slot-value socket 'proactor) proactor))

(defmethod close ((socket async-ssl-socket) &key abort)
  (declare (ignore abort))
  (with-slots (closing reading) socket
    (setf closing t)
    (if reading
      (cancel-async-io socket))))

(defmethod cancel-async-io ((socket async-ssl-socket))
  (with-slots (device out-overlapped) socket
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
  (with-slots (device remote-address state callback errback in-overlapped connect-timeout) socket
    (let ((local-address (resolve-address :address-family :internet :host "0.0.0.0" :port 0)))
      (windows-socket-call device "bind"
        (ccl::c_bind device (ccl::sockaddr local-address) (ccl::sockaddr-length local-address))))
    (#_memset in-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
    (let ((func-ptr (c_get_connect_ex_func device)))
      (prog1 (create-promise
               (lambda (resolver rejecter)
                 (setf state :connect
                       callback resolver
                       errback rejecter)))
        (set-timeout-handler socket connect-timeout)
        (format t "async ssl socket start to connect~%")
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

(defmethod handle-close ((socket async-ssl-socket))
  (with-slots (device state completion-key proactor ssl ssl-context
               out-overlapped in-overlapped out-buff in-buff) socket
    (remove-device proactor device)
    (external-call "SSL_shutdown" :address ssl)
    (external-call "SSL_free" :address ssl)
    (#_closesocket device)
    (external-call "SSL_CTX_free" :address ssl-context)
    (format t "async ssl socket closed~%")
    (free completion-key)
    (free out-overlapped)
    (free in-overlapped)
    (remove-timeout-handler socket)
    (setq device nil
          completion-key nil
          out-overlapped nil
          in-overlapped nil)
    (#_free out-buff)
    (#_free in-buff)
    (setf state :closed)))

(defmethod handle-connect ((socket async-ssl-socket))
  (with-slots (device state ssl callback errback read-bio write-bio) socket
    ; remember to free the overlapped io buffer
    (let (value)
      (setq value
        (external-call "SSL_set_fd" :address ssl :int device :int))

      (when (/= value 1)
        (remove-timeout-handler socket)
        (funcall errback (make-ssl-error device "SSL_set_fd" value))
        (return-from handle-connect))

      ;; finished an operation
      (external-call "SSL_set_bio" :address ssl :address read-bio :address write-bio)
      (external-call "SSL_set_connect_state" :address ssl)

      (setq value
        (external-call "SSL_do_handshake" :address ssl :int))

      (when (/= value 1)
        (let ((errno (external-call "SSL_get_error" :address ssl :int value :int)))
          (when (not
                  (member errno
                    (list +ssl-error-want-write+ +ssl-error-want-read+)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "SSL_do_handshake" errno)))))

      (setf state nil)
      (format t "async ssl socket connect succeed~%")
      (funcall callback))))

(defmethod handle-read ((socket async-ssl-socket) bytes-transferred)
  (with-slots (device state in-buff in-buff-size in-data condition
               callback errback ssl read-bio reading) socket
    (if reading
      (return-from handle-read))

    (when (> bytes-transferred 0)
      (format t "BIO_write ~D bytes~%" bytes-transferred)
      (let ((nwriten (external-call "BIO_write"
                                    :address read-bio :address in-buff
                                    :signed-fullword bytes-transferred
                                    :signed-fullword)))
        (if (/= nwriten bytes-transferred)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nwriten :int)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "BIO_write" errno))
            (return-from handle-read)))))

    (let ((nread (external-call "SSL_read"
                                :address ssl :address in-buff
                                :signed-fullword in-buff-size
                                :signed-fullword)))
      (if (<= nread 0)
        (let ((errno (external-call "SSL_get_error" :address ssl :int nread :int)))
          (when (not
                  (member errno
                    (list +ssl-error-none+ +ssl-error-want-write+ +ssl-error-want-read+)))
            (remove-timeout-handler socket)
            (funcall errback (make-ssl-error device "SSL_read" errno)))
          (return-from handle-read)))

      (setf in-data (concatenate '(vector (unsigned-byte 8)) in-data
                      (make-vector-from-carray in-buff nread)))

      (let ((position (funcall condition in-data)))
        (cond
          ((eq bytes-transferred 0)
            (setf state nil)
            (funcall callback in-data))
          (position
            (let ((data (subseq in-data 0 position)))
              (setf in-data (subseq in-data position))
              (setf state nil)
              (funcall callback data))))))))

(defmethod handle-overlapped-entry ((socket async-ssl-socket) overlapped-entry)
  (with-slots (device callback errback state out-data closing
               in-overlapped out-overlapped writing reading) socket
    (if (eq state :closed)
      (return-from handle-overlapped-entry))
    (when closing
      (handle-close socket)
      (return-from handle-overlapped-entry))

    (remove-timeout-handler socket)
    ;; check whether operation completed, if then call the callback
    (let ((overlapped (pref overlapped-entry :overlapped-entry.overlapped)))
      (rletz ((bytes-transferred #>DWORD))
        (if (eq 0 (#_GetOverlappedResult (%int-to-ptr device) overlapped bytes-transferred 0))
          (let ((errno (ccl::%get-winsock-error))
                (method (concatenate 'string "ASYNC-" (symbol-name state))))
            (funcall errback (make-socket-error device method errno)))
          (let ((bytes-transferred (pref bytes-transferred #>DWORD)))
            (cond
              ((eq state :connect)
                (format t "async ssl socket connected ~D bytes~%" bytes-transferred)
                (handle-connect socket))
              ((eq (%ptr-to-int overlapped)
                   (%ptr-to-int (pref in-overlapped :overlapped-extended.overlapped)))
                (setf reading nil)
                (format t "async ssl socket received ~D bytes~%" bytes-transferred))
              ((eq (%ptr-to-int overlapped)
                   (%ptr-to-int (pref out-overlapped :overlapped-extended.overlapped)))
                (setf writing nil)
                (format t "async ssl socket sent ~D bytes~%" bytes-transferred)
                (when (eq (length out-data) 0)
                  (setf state nil)
                  (funcall callback bytes-transferred))
                (setq bytes-transferred 0))
              (t (error "invalid overlapped entry")))

            (handle-read socket bytes-transferred)
            (socket-send socket)
            (socket-receive socket)))))))

(defmethod socket-send ((socket async-ssl-socket))
  (with-slots (device errback out-data out-buff out-buff-size writing
               out-overlapped output-timeout ssl write-bio) socket
    (if writing
      (return-from socket-send))

    (when (> (length out-data) 0)
      (let ((data-size (min out-buff-size (length out-data)))
            nwriten)
        (dotimes (index data-size)
          (setf (paref out-buff (:array :unsigned-byte) index) (aref out-data index)))
        (setq nwriten (external-call "SSL_write"
                                      :address ssl :address out-buff
                                      :signed-fullword data-size
                                      :signed-fullword))
        (if (/= nwriten data-size)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nwriten :int)))
            (format t "want write bytes: ~D, errno: ~D~%" data-size errno)
            (when (not
                    (member errno
                      (list +ssl-error-none+ +ssl-error-want-write+ +ssl-error-want-read+)))
              (remove-timeout-handler socket)
              (funcall errback (make-ssl-error device "SSL_write" errno))))
          (setf out-data (subseq out-data nwriten)))))

    (when (> (external-call "BIO_ctrl_pending" :address write-bio :size_t) 0)
      (let ((nread (external-call "BIO_read"
                                  :address write-bio :address out-buff
                                  :signed-fullword out-buff-size
                                  :signed-fullword)))
        (if (<= nread 0)
          (let ((errno (external-call "SSL_get_error" :address ssl :int nread :int)))
            (when (not
                    (member errno
                      (list +ssl-error-none+ +ssl-error-want-write+ +ssl-error-want-read+)))
              (remove-timeout-handler socket)
              (funcall errback (make-ssl-error device "BIO_read" errno)))
            (return-from socket-send)))

        (#_memset out-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
        (setf (pref out-overlapped :overlapped-extended.wsabuf.buf) out-buff)
        (setf (pref out-overlapped :overlapped-extended.wsabuf.len) nread)
        (set-timeout-handler socket output-timeout)
        (setf writing t)

        ; actually, SOCKET can pass as it is, but without a related type designator
        (if (/= 0 (#_WSASend device (%inc-ptr out-overlapped (ccl::foreign-size :overlapped :bytes)) 1
                    +null-ptr+ 0 out-overlapped +null-ptr+))
          (let ((errno (ccl::%get-winsock-error)))
            (if (/= errno (- #$WSA_IO_PENDING))
              (windows-socket-error device "WSASend" errno))))))))

; send a vector of binary data
(defmethod async-write ((socket async-ssl-socket) data)
  (with-slots (state out-data callback errback) socket
    (assert (not state))
    (prog1 (create-promise
             (lambda (resolver rejecter)
               (setf state :write
                     callback resolver
                     errback rejecter)))

      (setf out-data
        (concatenate '(vector (unsigned-byte 8)) out-data data)))))

(defmethod socket-receive ((socket async-ssl-socket))
  (with-slots (device in-overlapped input-timeout reading
               in-buff in-buff-size) socket
    (if reading
      (return-from socket-receive))

    ; remember to free the overlapped io buffer
    (#_memset in-overlapped 0 (ccl::foreign-size :overlapped-extended :bytes))
    (setf (pref in-overlapped :overlapped-extended.wsabuf.buf) in-buff
          (pref in-overlapped :overlapped-extended.wsabuf.len) in-buff-size)

    (set-timeout-handler socket input-timeout)
    (setf reading t)

    ; actually, SOCKET can pass as it is, but without a related type designator
    (if (/= 0
          (rletz ((recv-bytes #>DWORD)
                  (flags #>DWORD))
            (#_WSARecv device (%inc-ptr in-overlapped (ccl::foreign-size :overlapped :bytes))
                  1 recv-bytes flags in-overlapped +null-ptr+)))
      (let ((errno (ccl::%get-winsock-error)))
        (if (/= errno (- #$WSA_IO_PENDING))
          (windows-socket-error device "WSARecv" errno))))))

; receive a vector of binary data
(defmethod async-receive ((socket async-ssl-socket) type size cnd)
  (declare (ignore type size))
  (with-slots (state condition callback errback in-data) socket
    (let ((position (funcall condition in-data)))
      (assert (not state))
      (if position
        (let ((data (subseq in-data 0 position)))
          (setf in-data (subseq in-data position))
          (ext:promisify data))
        (create-promise
          (lambda (resolver rejecter)
            (setf state :read
                  condition cnd
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