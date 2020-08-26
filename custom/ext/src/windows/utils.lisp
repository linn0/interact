
(in-package #:ext)

(defvar *foreign-libraries* nil)

(defconstant +default-read-buffer-size+ 4096)
(defconstant +default-write-buffer-size+ 4096)

(def-foreign-type windows-guid
  (:struct :windows-guid
     (:data1 #>DWORD)
     (:data2 #>WORD)
     (:data3 #>WORD)
     (:data4 (:array #>BYTE 8))))

(def-foreign-type overlapped-io-completion-key
  (:struct :overlapped-io-completion-key
     (:device #>HANDLE)))

(def-foreign-type overlapped
  (:struct :overlapped
     (:internal #>ULONG_PTR)
     (:internal-high #>ULONG_PTR)
     (:un (:union nil
            (:offset (:struct nil
                       (:offset-low #>ULONG_PTR)
                       (:offset-high #>ULONG_PTR)))
            (:pointer #>PVOID)))
     (:event #>HANDLE)))

(def-foreign-type winsock-wsabuf
  (:struct :winsock-wsabuf
     (:len :unsigned-fullword)
     (:buf :address)))

(def-foreign-type overlapped-extended
  (:struct :overlapped-extended
     (:overlapped :overlapped)
     (:wsabuf :winsock-wsabuf)))

(def-foreign-type overlapped-entry
  (:struct :overlapped-entry
     (:completion-key #>ULONG_PTR)
     (:overlapped (:* :overlapped))
     (:internal #>ULONG_PTR)
     (:bytes-transferred #>DWORD)))

(defvar *socket-error-identifiers*
  (list 
  #$WSA_INVALID_PARAMETER :invalid-argument
  #$WSAEINVAL :invalid-argument
  #$WSAEADDRINUSE :address-in-use
  #$WSAECONNABORTED :connection-aborted
  #$WSAENOBUFS :no-buffer-space
  #$ENOMEM :no-buffer-space
  #$ENFILE :no-buffer-space
  #$WSAETIMEDOUT :connection-timed-out
  #$WSAECONNREFUSED :connection-refused
  #$WSAENETUNREACH :host-unreachable
  #$WSAEHOSTUNREACH :host-unreachable
  #$WSAEHOSTDOWN :host-down
  #$WSAENETDOWN :network-down
  #$WSAEADDRNOTAVAIL :address-not-available
  #$WSAENETRESET :network-reset
  #$WSAECONNRESET :connection-reset
  #$WSAESHUTDOWN :shutdown
  #$EACCES :access-denied
  #$EPERM :access-denied
  ))

(defun make-socket-error (stream where errno)
  (setq errno (abs errno))
  (make-condition 'socket-error
     :stream stream
     :code errno
     :identifier (getf *socket-error-identifiers* errno :unknown)
     :situation where
     :format-control "~a (error #~d) during ~a"
     :format-arguments (list
            (ccl::%windows-error-string errno)
            errno where)))

(defun windows-socket-error (stream where errno)
  "Creates and signals (via error) one of two socket error 
conditions, based on the state of the arguments."
  (error (make-socket-error stream where errno)))

(declaim (inline windows-socket-call))
(defun windows-socket-call (stream where res)
  (if (< res 0)
    (windows-socket-error stream where res)
    res))

(defun make-ssl-error (stream where errno)
  (setq errno (abs errno))
  (let* ((errmsg-buffer-size 1024)
         (errmsg-buffer (#_malloc errmsg-buffer-size))
         errmsg)
    (do () ((eq errno 0))
      (external-call "ERR_error_string_n" :signed-fullword errno
                                          :address errmsg-buffer
                                          :signed-fullword errmsg-buffer-size)
      (setf errmsg (concatenate '(vector (unsigned-byte 8)) errmsg
        (make-vector-from-carray errmsg-buffer (#_strlen errmsg-buffer))))
      (setq errno (external-call "ERR_get_error" :unsigned-fullword)))

    (#_free errmsg-buffer)
    (format t "~a (error #~d) during ~a~%"
      (ccl:decode-string-from-octets errmsg :external-format :us-ascii)
      errno where)
    (make-condition 'socket-error
      :stream stream
      :code errno
      :identifier :unknown
      :situation where
      :format-control "~a (error #~d) during ~a"
      :format-arguments
        (list
          (ccl:decode-string-from-octets errmsg :external-format :us-ascii)
          errno where))))

(defun c_get_connect_ex_func (socket)
  (rletz ((connect-function :address)
          (wsaid-connectex :windows-guid :data1 #x25a207b9 :data2 #xddf3 :data3 #x4660)
          (bytes-returned #>DWORD))
    (let ((high-bytes #(#x8e #xe9 #x76 #xe5 #x8c #x74 #x06 #x3e)))
      (dotimes (index (length high-bytes))
        (setf (paref (pref wsaid-connectex :windows-guid.data4) (:array #>BYTE 8) index)
          (svref high-bytes index))))
    (#_WSAIoctl socket #$SIO_GET_EXTENSION_FUNCTION_POINTER
                wsaid-connectex (ccl::foreign-size :windows-guid :bytes)
                connect-function (ccl::foreign-size :address :bytes)
                bytes-returned +null-ptr+ +null-ptr+)
    (pref connect-function :address)))
