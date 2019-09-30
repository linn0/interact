
(in-package #:ext)

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
