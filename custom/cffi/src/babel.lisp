;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; babel.lisp --- CFFI-Babel implementation for String.
;;;
;;; Copyright (C) 2017-2020, James Bielman  <jamesjb@jamesjb.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

;;;# Administrivia

(defpackage #:babel
  (:use #:common-lisp #:cl)
  (:export
   ;; types
   #:unicode-char
   #:unicode-char-code-limit
   #:unicode-string
   #:simple-unicode-string
   #:enc-nul-encoding))

(in-package #:babel)

;;; SIMPLE-BASE-STRING would also be a subtype of SIMPLE-STRING so we
;;; don't use that because on SBCL BASE-CHARs can only hold ASCII.
;;; Also, with (> SPEED SAFETY) (setf (schar base-str n) big-char)
;;; will quietly work, sort of.
;;;
;;; XXX: test this on various lisps.

(defconstant unicode-char-code-limit
  char-code-limit
  "An alias for CL:CHAR-CODE-LIMIT which might be lower than
#x110000 on some Lisps.")

(deftype unicode-char ()
  "This character type can hold any characters whose CHAR-CODEs
are less than UNICODE-CHAR-CODE-LIMIT."
  'character)

(deftype simple-unicode-string ()
  "Alias for (SIMPLE-ARRAY UNICODE-CHAR (*))."
  '(simple-array unicode-char (*)))

(deftype unicode-string ()
  "Alias for (VECTOR UNICODE-CHAR *)."
  '(vector unicode-char *))

(defun enc-nul-encoding (encoding)
  (case encoding 
    (:utf-16   #(0 0)) 
    (:utf-16le #(0 0)) 
    (:utf-16be #(0 0)) 
    (:utf-32   #(0 0 0 0)) 
    (:utf-32le #(0 0 0 0)) 
    (:utf-32be #(0 0 0 0)) 
    (:ucs-2    #(0 0)) 
    (:ucs-2le  #(0 0)) 
    (:ucs-2be  #(0 0)) 
    (t #(0))))
