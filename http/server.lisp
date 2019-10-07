;;;; HTTP Interface for ClozureCL
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :http)

;;; ----------------------------------------------------

(defclass server-config ()
  ((process-name    :initarg :name            :initform "HTTP")
   (host            :initarg :host            :initform "0.0.0.0")
   (port            :initarg :port            :initform 80)
   (public-folder   :initarg :public-folder   :initform nil)
   (home-page       :initarg :home-page       :initform nil)
   (session-class   :initarg :session-class   :initform 'http-session)
   (session-timeout :initarg :session-timeout :initform 10)
   (not-found       :initarg :not-found-route :initform nil)
   (server-error    :initarg :error-route     :initform nil))
  (:documentation "Basic HTTP server configuration options."))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((config server-config) &key)
  "Make sure all the settings are valid."
  (with-slots (public-folder)
      config
    (when public-folder
      (setf public-folder
            (or (probe-file public-folder)
                (warn "Invalid public folder ~s..." public-folder))))))

;;; ----------------------------------------------------

(defun http-start-server (router &rest initargs)
  "Start a server process that will process incoming HTTP requests."
  (let* ((config (apply 'make-instance 'server-config initargs)))
    (with-slots (host port process-name)
        config

      ;; create the server socket
      (let ((sock (make-socket :type :stream
                               :connect :passive
                               :reuse-address t
                               :local-host host
                               :local-port port)))

        ;; start the server process
        (process-run-function process-name 'http-server-loop
                              sock
                              router
                              config)))))

;;; ----------------------------------------------------

(defun http-stop-server (&optional (name "HTTP"))
  "Find the server process and kill it."
  (let ((ps (remove name
                    (all-processes)
                    :test-not #'string=
                    :key #'process-name)))

    ;; kill any process with the same name as the server
    (mapc #'process-kill ps)))

;;; ----------------------------------------------------

(defun http-server-loop (socket router config)
  "Infinite loop, accepting new connections and routing them."
  (let ((session-map (make-hash-table :test 'equal))
        (session-lock (make-read-write-lock)))
    (unwind-protect
         (loop
            (let ((http (accept-connection socket :wait t)))
              (process-run-function "Request" 'http-process-request
                                    http
                                    router
                                    config
                                    session-map
                                    session-lock))

            ;; remove old sessions from the map
            (http-timeout-sessions session-map session-lock config))

      ;; server process finished
      (close socket))))

;;; ----------------------------------------------------

(defun http-timeout-sessions (session-map session-lock config)
  "Remove old sessions from the session map."
  (let ((time-limit (slot-value config 'session-timeout)))
    (flet ((timeout (sid session)
             (when (http-session-timed-out-p session time-limit)
               (remhash sid session-map))))
      (with-write-lock (session-lock)
        (maphash #'timeout session-map)))))

;;; ----------------------------------------------------

(defun http-process-request (http router config session-map session-lock)
  "Process a single HTTP request and send a response."
  (let ((resp (http-make-response http)))
    (if resp
        (let* ((req (resp-request resp))

               ;; lookup the session from the request
               (session (let ((sid (http-read-session-id req)))
                          (when sid
                            (with-read-lock (session-lock)
                              (gethash sid session-map))))))

          ;; if there's no session, make a new one
          (unless session
            (with-slots (id)
                (setf session (http-make-session resp config))

              ;; add the session to the session map
              (with-write-lock (session-lock)
                (setf (gethash id session-map) session))))

          ;; refresh the session with a new timestamp
          (setf (session-time session) (get-universal-time))

          ;; route the response, then send the response back
          (unwind-protect
               (progn
                 (http-route-response resp session router config)
                 (http-write-response resp))

            ;; close the connection unless the server wants it alive
            (let ((connection (http-header resp "Connection")))
              (unless (string-equal connection "keep-alive")
                (shutdown http :direction :output)))))

      ;; failed to parse the request and make a response, just quit
      (close http))))

;;; ----------------------------------------------------

(defun http-request-url (req)
  "Convert a string into a URL-safe, encoded string."
  (with-output-to-string (str)
    (print-object (req-url req) str)))

(defun http-route-response (resp session router config)
  "Attempt to route the request to the appropriate handler."
  (with-slots (not-found server-error) config
    ;; conditions trigger a server error
    (handler-bind ((error (lambda (c)
        (let ((stack-trace (ext:thread-stack-trace)))
          (log:error "http route response failed: ~A, stack trace:~%~A" c stack-trace))
        (if server-error
            (funcall server-error session resp c)
          (http-internal-server-error resp (princ-to-string c))))))

        ;; look for a continuation route
        (let (;(url (http-request-url (resp-request resp)))
              (cont (http-find-continuation session resp)))
          ; (log:debug "received http request url: ~A" url)
          (if cont
              (with-slots (route args)
                  cont
                (apply route session resp args))

            ;; attempt to use the route function to handle the response
            (unless (funcall router session resp)

              ;; is the request path in the public folder
              (let ((path (http-public-file-p config resp)))
                (if path
                  (http-file-response path resp)

                  (if not-found
                      (funcall not-found session resp)
                    (http-not-found resp))))))
          ; (log:debug "processed http request url: ~A" url)
          ))))

;;; ----------------------------------------------------

(defun http-public-file-p (config resp)
  "Pathname to public file if valid, otherwise NIL."
  (with-slots (public-folder home-page) config
    (when public-folder
      (let ((path (url-path (req-url (resp-request resp)))))
        (if (and (string= path "/") home-page)
          (setq path home-page))
        (probe-file (merge-pathnames (subseq path 1) public-folder))))))

(defun slurp-file-string (path)
  "Read the contents of the INPUT stream as a string"
  (with-open-file (input path
                     :direction :input
                     :external-format :utf-8)
    (subseq (uiop:slurp-stream-string input :stripped nil) 1)))

(defun slurp-file-octet-vector (path)
  "Read the contents of the INPUT stream as a string"
  (with-open-stream (output (ext:make-in-memory-output-stream))
    (with-open-file (input path
                       :direction :input
                       :element-type '(unsigned-byte 8))
      (uiop:copy-stream-to-stream input output :element-type '(unsigned-byte 8)))
    (ext:get-output-stream-sequence output)))

(defun http-file-response (path resp)
  (let ((content-type (content-type-of-pathname path)))
    ;; set the response type
    (when (content-type-text-p content-type)
      (setf (content-type-parameter content-type "charset") "utf-8")
      (setf (http-header resp "Content-Encoding") "gzip"))
    (content-type-push content-type resp)
    ;; build the page and return it
    (http-ok resp
      (if (content-type-text-p content-type)
        (slurp-file-string path)
        (slurp-file-octet-vector path)))))
