
(in-package #:ext)

(defvar *foreign-libraries* nil)
(defvar *ssl-context* nil)

(defconstant +ssl-verify-none+ #x00)

(defun init-ssl ()
  (nconc *foreign-libraries* 
    (mapcar (lambda (lib) (open-shared-library lib))
      '("D:/Project/interact/c/libressl-2.5.5/x64/libcrypto-41.dll"
        "D:/Project/interact/c/libressl-2.5.5/x64/libssl-43.dll")))

  (external-call "SSL_library_init")
  (external-call "SSL_load_error_strings")
  (external-call "ERR_load_BIO_strings")

  (setq *ssl-context* (external-call "SSL_CTX_new" :address (external-call "SSLv23_method" :address) :address))
  (external-call "SSL_CTX_set_verify" :address *ssl-context* :signed-fullword +ssl-verify-none+ :address +null-ptr+))

(defun close-ssl ()
  (external-call "EVP_cleanup")
  (external-call "CRYPTO_cleanup_all_ex_data")
  (external-call "ERR_free_strings")

  (mapc #'(lambda (lib) (close-shared-library lib)) *foreign-libraries*))

(defclass async-ssl-socket (async-socket)
  ((ssl :initarg :ssl :initform nil :accessor async-socket-ssl)
   (ssl-context :initarg :ssl-context :initform nil :accessor async-socket-ssl-context)
   (read-bio :accessor async-socket-ssl-read-bio :initarg :read-bio :initform nil)
   (write-bio :accessor async-socket-ssl-write-bio :initarg :write-bio :initform nil)
   (request-queue :initform (make-queue))
   (ssl-buffer :initform nil :accessor async-socket-ssl-buffer)
   (ssl-receiving :initform nil :accessor async-socket-ssl-receiving)))

(defmethod initialize-instance :after ((socket async-ssl-socket) &rest initargs)
  (declare (ignore initargs))
  (with-slots (ssl ssl-context read-bio write-bio) socket
    (setq ssl-context *ssl-context*)
    (setq ssl (external-call "SSL_new" :address ssl-context :address))
    (setq read-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))
    (setq write-bio (external-call "BIO_new" :address (external-call "BIO_s_mem" :address) :address))
    (external-call "SSL_set_bio" ssl read-bio write-bio)))

(defmethod async-connect ((socket async-ssl-socket))
  (with-slots (request-queue) socket
    (prog1 (create-promise
             (lambda (resolver rejecter)
               (queue-push request-queue 
                 (make-instance 'overlapped-request 
                                :type :connect
                                :callback resolver
                                :errback rejecter))))
      (ext:attach (call-next-method socket)
        (lambda () 
          (external-call "SSL_set_connect_state" :address (async-socket-ssl socket)))))))

(defmethod handle-ssl-error ((socket async-ssl-socket) retcode)
  (let* ((errcode (external-call "SSL_get_error" :address (async-socket-ssl socket) 
                                                 :signed-fullword retcode :signed-fullword))
         (errmsg-buffer-size 1024)
         (errmsg-buffer (#_malloc errmsg-buffer-size))
         errmsg)
    (do () 
        ((eq errcode 0))
      (external-call "ERR_error_string_n" :signed-fullword errcode 
                                          :address errmsg-buffer 
                                          :signed-fullword errmsg-buffer-size)
      (setf errmsg (concatenate '(vector (unsigned-byte 8)) errmsg 
        (make-vector-from-carray errmsg-buffer (#_strlen errmsg-buffer))))
      (setq errcode (external-call "ERR_get_error" :unsigned-fullword)))
    (if errmsg
      (format t "ssl error: ~a~%" (ccl:decode-string-from-octets errmsg :external-format :us-ascii)))
    (#_free errmsg-buffer)))

(defmethod ssl-run-state ((socket async-ssl-socket))
  (with-slots (ssl write-bio request-queue ssl-buffer ssl-receiving) socket
    (when (and (not ssl-receiving) (null ssl-buffer))
      (setf ssl-receiving t)
      (ext:attach (async-read-some socket)
        (lambda (data) 
          (setf ssl-receiving nil)
          (let ((nwriten (external-call "BIO_write" 
                                        :address read-bio :address (make-carray-from-vector data) 
                                        :signed-fullword (length data)
                                        :signed-fullword)))
            (if (/= nwriten (length data))
              (handle-ssl-error socket nwriten))))))

    (let* ((request (queue-peek request-queue))
           (request-type (overlapped-request-type request))
           (callback (overlapped-request-callback request))
           (buffer (#_malloc +default-read-buffer-size+)))
      (unless ssl-buffer
        (let ((nread (external-call "SSL_read" 
                                    :address ssl :address buffer 
                                    :signed-fullword +default-read-buffer-size+ 
                                    :signed-fullword)))
          (when (and (eq request-type :connect) 
                   (eq (external-call "SSL_state" :address ssl) 3))
            (funcall callback)
            (finish-overlapped-request request-queue))
          (if (> nread 0)
            (setf ssl-buffer (concatenate '(vector (unsigned-byte 8)) ssl-buffer 
                         (make-vector-from-carray buffer nread)))
            (handle-ssl-error socket nread))))

      (when (eq request-type :write)
        (let ((nwriten (external-call "SSL_write" 
                                      :address ssl :address (overlapped-request-buffer request)
                                      :signed-fullword (overlapped-request-data-size request)
                                      :signed-fullword)))
          (if (/= nwriten (overlapped-request-data-size request))
            (handle-ssl-error socket nwriten))))
      (#_free buffer))

    (when (> (external-call "BIO_ctrl_pending" :address write-bio) 0)
      (let ((buffer (#_malloc +default-read-buffer-size+))
            (nread (external-call "BIO_read" 
                                  :address write-bio :address buffer 
                                  :signed-fullword +default-read-buffer-size+ 
                                  :signed-fullword)))
        (if (> nread 0)
          (async-write-array socket buffer nread)
          (handle-ssl-error socket nread))))))

(defmethod async-write ((socket async-ssl-socket) data)
  (with-slots (request-queue) socket
    (create-promise
      (lambda (resolver rejecter)
        (queue-push request-queue
          (make-instance 'overlapped-request 
                         :type :write
                         :buffer (make-carray-from-vector data)
                         :data-size (length data)
                         :callback resolver
                         :errback rejecter))))))

; receive a vector of binary data
(defmethod async-receive ((socket async-socket) type size condition)
  (with-slots (request-queue) socket
    (let ((buffer-size 
            (ecase type
              (:read +default-read-buffer-size+)
              ((:read-some :read-until) 
                (if size size +default-read-buffer-size+)))))
      (create-promise
        (lambda (resolver rejecter)
          (queue-push request-queue
            (make-instance 'overlapped-request 
                           :type type
                           :buffer (#_malloc buffer-size)
                           :buffer-size buffer-size
                           :data-size size
                           :condition condition
                           :callback resolver
                           :errback rejecter)))))))
