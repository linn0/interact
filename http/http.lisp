;;;; HTTP interface for ClozureCL
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

(defpackage :http
  (:use :cl :ccl :sha1 :parse :re :lexer :url)
  (:export

   ;; macros
   #:with-headers
   #:with-response

   ;; HTTP socket streams
   #:http-open-stream
   #:http-open-async-stream
   #:http-open-event-stream

   ;; content type class and functions
   #:content-type-parse
   #:content-type-push
   #:content-type-text-p
   #:content-type-of-pathname
   #:content-type-external-format
   #:content-type-mime-type
   #:content-type-mime-subtype
   #:content-type-parameters
   #:content-type-parameter

   ;; cookie class and functions
   #:cookie-parse
   #:cookie-parse-all
   #:cookie-push
   #:cookie-key
   #:cookie-value
   #:cookie-attributes
   #:cookie-attribute

   ;; creating content-types and cookies
   #:http-make-content-type
   #:http-make-cookie

   ;; header functions
   #:http-headers
   #:http-header
   #:http-read-headers
   #:http-write-headers

   ;; creating requests and responsed
   #:http-make-request
   #:http-make-response

   ;; content encodings
   #:http-register-content-encoding
   #:http-registered-content-encoding

   ;; request functions
   #:http-follow
   #:http-perform
   #:http-async-perform
   #:http-head
   #:http-get
   #:http-async-get
   #:http-options
   #:http-trace
   #:http-delete
   #:http-put
   #:http-post
   #:http-patch

   ;; request accessors
   #:req-protocol
   #:req-url
   #:req-method
   #:req-body
   #:req-keep-alive
   #:req-read-body

   ;; response accessors
   #:resp-stream
   #:resp-protocol
   #:resp-code
   #:resp-status
   #:resp-body
   #:resp-request

   ;; server functions
   #:http-start-server
   #:http-stop-server

   ;; route macros
   #:define-http-router

   ;; sessions and continuations
   #:http-make-continuation))

(in-package :http)

;;; ----------------------------------------------------

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(vector octet))

(defvar *http-pools* (make-hash-table :test #'equal))

;;; ----------------------------------------------------

(defun http-make-content-type (type subtype &key format parameters)
  "Make a content-type that can be pushed to a request/response."
  (make-instance 'content-type
                 :type type
                 :subtype subtype
                 :format format
                 :parameters parameters))

;;; ----------------------------------------------------

(defun http-make-cookie (key value &key attributes)
  "Create a cookie that can be pushed to a request/response."
  (make-instance 'cookie :key key :value value :attributes attributes))

;;; ----------------------------------------------------

(defparameter +http-io-timeout-secs+ 60)

(defun http-open-stream (req &key (timeout +http-io-timeout-secs+))
  "Open a TCP stream to a given URL."
  (let* ((url (req-url req))
         (stream (make-socket 
                 :remote-host (url-domain url)
                 :remote-port (url-port url)
                 :auto-close t
                 :format :bivalent
                 :connect-timeout timeout
                 :input-timeout timeout
                 :output-timeout timeout
                 :keepalive (req-keep-alive req))))
    stream))

;;; ----------------------------------------------------

(defun http-make-stream (req)
  "Open a asynchronous TCP stream to a given URL."
  (let ((url (req-url req))
        (timeout (req-timeout req))
        stream)
    (if (equal (url-scheme url) "https")
      (setq stream (ext:make-async-ssl-socket
                     :remote-host (url-domain url)
                     :remote-port (url-port url)
                     :connect-timeout timeout
                     :input-timeout timeout
                     :output-timeout timeout
                     :keepalive (req-keep-alive req)))
      (setq stream (ext:make-async-socket
                     :remote-host (url-domain url)
                     :remote-port (url-port url)
                     :connect-timeout timeout
                     :input-timeout timeout
                     :output-timeout timeout
                     :keepalive (req-keep-alive req))))
    (ext:register (req-reactor req) stream)
    (ext:attach 
      (ext:async-connect stream)
      (lambda () stream))))

;;; ----------------------------------------------------

(defun http-open-async-stream (req)
  (if (req-keep-alive req)
    (let* ((url (req-url req))
           (url-key (format nil "~a:~d" (url-domain url) (url-port url)))
           (pool (gethash url-key *http-pools*)))
      (when (null pool)
        (setq pool (ext:make-pool :name url-key
                                  :item-maker (lambda () (http-make-stream req))
                                  :item-destroyer (lambda (stream) (unless (null stream) (close stream)))
                                  :capacity 20))
        (setf (gethash url-key *http-pools*) pool))
      (ext:fetch-from pool))
    (http-make-stream req)))

;;; ----------------------------------------------------

(defun http-close-stream-if-wants (resp &optional stream)
  ;; determine if the connection should terminate
  (cond ((null resp)
          (close stream))

        ;; the request wants to close it
        ((null (req-keep-alive (resp-request resp)))
          (close (resp-stream resp))
          (setf (resp-stream resp) nil))

        ;; see if the server wants to close it
        (t (let ((connection (http-header resp "Connection")))
             (when (string-equal connection "close")
               (format t "the request wants to keep connection ~S alive, but the server wants to close it.~%" (resp-stream resp))
               (close (resp-stream resp))
               (setf (resp-stream resp) nil))))))

;;; ----------------------------------------------------

(defun http-perform (req &optional stream)
  "Perform a generic HTTP request, return the response."
  (let ((http (if stream
                  stream
                (http-open-stream req))))

    ;; send the request
    (http-write-request req http)

    ;; parse the response
    (let ((resp (http-read-response req http)))
      (http-close-stream-if-wants resp http)
      resp)))

;;; ----------------------------------------------------

(defun http-close-stream (req resp stream)
  ;; determine if the connection should terminate

  ;; the request wants to close it
  (if (null (req-keep-alive req))
    (close stream)

    (let* ((url (req-url req))
           (url-key (format nil "~a:~d" (url-domain url) (url-port url)))
           (pool (gethash url-key *http-pools*)))

      (if (null resp)
        (ext:destroy-pool-item pool stream)

        ;; see if the server wants to close it
        (let ((connection (http-header resp "Connection")))
           (if (string-equal connection "close")
             (ext:destroy-pool-item pool stream)
             (ext:return-to pool stream))))))

  (unless (null resp)
    (setf (resp-stream resp) nil)))

;;; ----------------------------------------------------

(defun http-async-perform (req)
  "Perform a generic HTTP request, return the response."
  (let (response stream)
    (ext:chain (http-open-async-stream req)
      (:then (http)
        (setq stream http)
        (ext:async-write stream 
          (with-output-to-vector (req-text)
            (http-write-request req req-text))))
      (:then (bytes-transferred)
        (declare (ignore bytes-transferred))
        (ext:async-read-until stream 
          (lambda (data) 
            (let* ((delimiter (map 'vector #'char-code 
                                #(#\Return #\Linefeed #\Return #\Linefeed)))
                   (position (search delimiter data)))
              (if position (+ position (length delimiter)))))))
      (:then (data)
        (http-async-read-response req stream data))
      (:then (resp)
        (setq response resp))
      (:finally ()
        (http-close-stream req response stream)))))

;;; ----------------------------------------------------

(defun http-follow-request (resp)
  "Create a new request for a redirect response."
  (when (<= 300 (resp-code resp) 399)
    (let* ((req (resp-request resp))
           (url (req-url req))
           (reactor (req-reactor req))

           ;; get the target location
           (loc (http-header resp "Location"))

           ;; the redirect location might be relative (to the domain)
           (new-url (url-parse loc :relative-url url)))

      ;; create the new request - keep the same method unless a 303
      (make-instance 'request
                     :url new-url
                     :reactor reactor
                     :keep-alive (req-keep-alive req)
                     :read-body (req-read-body req)
                     :body (req-body req)
                     :headers (http-headers req)
                     :method (if (= (resp-code resp) 303)
                                 "GET"
                               (req-method req))))))

;;; ----------------------------------------------------

(defun http-follow (resp &key (redirect-limit 3))
  "Create a response redirect."
  (loop
     while (plusp redirect-limit)

     ;; if the response requires a redirect, follow it
     do (if (find (resp-code resp) '(301 302 303 304 305 307))
            (let ((req (http-follow-request resp)))
              (setf resp (http-perform req))
              (decf redirect-limit))
          (loop-finish))

     ;; quit returning the last response
     finally (return resp)))

;;; ----------------------------------------------------

(defun http-async-follow (request &key (redirect-limit 3))
  "Create a response redirect."
  (ext:attach request
    (lambda (resp) 
      (if (and (plusp redirect-limit)
               (find (resp-code resp) '(301 302 303 304 305 307)))
        (let ((req (http-follow-request resp)))
          (when (resp-stream resp)
            (close (resp-stream resp))
            (setf (resp-stream resp) nil))
          (http-async-follow
            (http-async-perform req)
            :redirect-limit (1- redirect-limit)))
        resp))))

;;; ----------------------------------------------------

(defun http-get (url &rest initargs)
  "Perform a GET request for a URL, return the response."
  (let ((req (apply #'http-make-request url initargs)))
    (http-follow (http-perform req) :redirect-limit 3)))

;;; ----------------------------------------------------

(defun http-async-get (url reactor &rest initargs)
  "Perform a GET request for a URL, return the response."
  (let ((req (apply #'http-make-request url :reactor reactor initargs)))
    (http-async-follow (http-async-perform req) :redirect-limit 3)))

;;; ----------------------------------------------------

(defun http-head (url &rest initargs)
  "Perform a HEAD request for a URL, return the response."
  (apply #'http-get url :method "HEAD" :read-body nil initargs))

;;; ----------------------------------------------------

(defun http-options (url &rest initargs)
  "Perform an OPTIONS request for a URL, return the response."
  (apply #'http-get url :method "OPTIONS" initargs))

;;; ----------------------------------------------------

(defun http-trace (url &rest initargs)
  "Perform an TRACE request for a URL, return the response."
  (apply #'http-get url :method "TRACE" initargs))

;;; ----------------------------------------------------

(defun http-delete (url &rest initargs)
  "Perform a DELETE request for a URL, return the response."
  (apply #'http-get url :method "DELETE" initargs))

;;; ----------------------------------------------------

(defun http-put (url &rest initargs)
  "Perform a PUT request for a URL, return the response."
  (apply #'http-get url :method "PUT" initargs))

;;; ----------------------------------------------------

(defun http-post (url &rest initargs)
  "Perform a POST request for a URL, return the response."
  (apply #'http-get url :method "POST" initargs))

;;; ----------------------------------------------------

(defun http-patch (url &rest initargs)
  "Perform a PATCH request for a URL, return the response."
  (apply #'http-get url :method "PATCH" initargs))
