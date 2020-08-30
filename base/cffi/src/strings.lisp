;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; strings.lisp --- Operations on foreign strings.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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

(in-package #:cffi)

;;;# Foreign String Conversion
;;;
;;; Functions for converting NULL-terminated C-strings to Lisp strings
;;; and vice versa.  The string functions accept an ENCODING keyword
;;; argument which is used to specify the encoding to use when
;;; converting to/from foreign strings.

(defvar *default-foreign-encoding* :utf-8
  "Default foreign encoding.")

(defvar *system-foreign-encoding* 
  #+windows
  (cdr 
    (assoc 
      (ccl:external-call "GetACP" :unsigned-fullword)
        '( ; https://msdn.microsoft.com/en-us/library/dd317756(v=vs.85).aspx
          (936 . :gb2312)
          (950 . :big5)
          (1200 . :utf-16)
          (12000 . :utf-32)
          (12001 . :utf-32BE)
          (20127. :us-ascii)
          (20932 . :EUC-JP)
          (51932 . :euc-jp)
          (51936 . :EUC-CN)
          (51949 . :euc-kr)
          (52936 . :hz-gb-2312)
          (54936 . :GB18030)
          (65001 . :utf-8)
         )))
  #+unix
  (ccl:getenv "LC_CTYPE")
  "System foreign encoding.")

(defun null-terminator-len (encoding)
  (length (babel:enc-nul-encoding encoding)))

;;; Do we want a more a specific error condition here?
(defun check-vector-bounds (vector start end)
  (unless (<= 0 start end (length vector))
    (error "Invalid start (~A) and end (~A) values for vector of length ~A."
           start end (length vector))))

(defmacro with-simple-vector (((v vector) (s start) (e end)) &body body)
  "If VECTOR is a displaced or adjustable array, binds V to the
underlying simple vector, adds an adequate offset to START and
END and binds those offset values to S and E.  Otherwise, if
VECTOR is already a simple array, it's simply bound to V with no
further changes.

START and END are unchecked and assumed to be within bounds.

Note that in some Lisps, a slow copying implementation is
necessary to obtain a simple vector thus V will be bound to a
copy of VECTOR coerced to a simple-vector.  Therefore, you
shouldn't attempt to modify V."
  (ext:with-unique-names (offset)
    `(multiple-value-bind (,v ,offset)
         (ccl::array-data-and-offset ,vector)
       (let ((,s (+ ,start ,offset))
             (,e (+ ,end ,offset)))
         ,@body))))

(defmacro with-checked-simple-vector (((v vector) (s start) (e end)) &body body)
  "Like WITH-SIMPLE-VECTOR but bound-checks START and END."
  (ext:once-only (vector start)
    `(let ((,e (or ,end (length ,vector))))
       (check-vector-bounds ,vector ,start ,e)
       (with-simple-vector ((,v ,vector) (,s ,start) (,e ,e))
         ,@body))))

(defun lisp-string-to-foreign (string buffer bufsize &key (start 0) end offset
                               (encoding *default-foreign-encoding*))
  (check-type string string)
  (when offset
    (setq buffer (inc-pointer buffer offset)))
  (with-checked-simple-vector ((string (coerce string 'babel:unicode-string))
                               (start start) (end end))
    (declare (type simple-string string))
    (let ((nul-len (null-terminator-len encoding)))
      (assert (plusp bufsize))
      (let ((size (min (- bufsize nul-len) 
                       (ccl:string-size-in-octets string :start start :end end 
                                                  :external-format encoding))))
        (ccl::encode-string-to-memory 
          (ccl::ensure-character-encoding encoding) 
          buffer 0 string start end)
        (dotimes (i nul-len)
          (setf (mem-ref buffer :char (+ size i)) 0))))
    buffer))

;;; Expands into a loop that calculates the length of the foreign
;;; string at PTR plus OFFSET, using ACCESSOR and looking for a null
;;; terminator of LENGTH bytes.
(defmacro %foreign-string-length (ptr offset type length)
  (once-only (ptr offset)
    `(do ((i 0 (+ i ,length)))
         ((zerop (mem-ref ,ptr ,type (+ ,offset i))) i)
       (declare (fixnum i)))))

;;; Return the length in octets of the null terminated foreign string
;;; at POINTER plus OFFSET octets, assumed to be encoded in ENCODING,
;;; a CFFI encoding.  This should be smart enough to look for 8-bit vs
;;; 16-bit null terminators, as appropriate for the encoding.
(defun foreign-string-length (pointer &key (encoding *default-foreign-encoding*)
                              (offset 0))
  (ecase (null-terminator-len encoding)
    (1 (%foreign-string-length pointer offset :uint8 1))
    (2 (%foreign-string-length pointer offset :uint16 2))
    (4 (%foreign-string-length pointer offset :uint32 4))))

(defun %get-encoded-string (encoding-name pointer noctets)
  (let* ((encoding (ccl::ensure-character-encoding encoding-name)))
    (multiple-value-bind (nchars nused)
        (funcall (ccl::character-encoding-length-of-memory-encoding-function encoding)
                 pointer
                 noctets
                 0)
      (let* ((string (make-string nchars)))
        (funcall (ccl::character-encoding-memory-decode-function encoding)
                 pointer
                 nused
                 0
                 string)
        (values string nused)))))

(defun foreign-string-to-lisp (pointer &key (offset 0) count
                               (max-chars (1- array-total-size-limit))
                               (encoding *default-foreign-encoding*))
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (unless (null-pointer-p pointer)
    (let ((count (min (or count
                      (foreign-string-length
                       pointer :encoding encoding :offset offset)) max-chars)))
      (assert (plusp max-chars))
      (%get-encoded-string encoding pointer count))))

;;;# Using Foreign Strings

(defun foreign-string-alloc (string &key (encoding *default-foreign-encoding*)
                             (null-terminated-p t) (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (with-checked-simple-vector ((string (coerce string 'babel:unicode-string))
                               (start start) (end end))
    (declare (type simple-string string))
    (let* ((count (ccl:string-size-in-octets string :start start :end end 
                                             :external-format encoding))
           (nul-length (if null-terminated-p
                           (null-terminator-len encoding)
                           0))
           (length (+ count nul-length))
           (ptr (foreign-alloc :char :count length)))
      (ccl::encode-string-to-memory 
        (ccl::ensure-character-encoding encoding)
        ptr 0 string start end)
      (dotimes (i nul-length)
        (setf (mem-ref ptr :char (+ count i)) 0))
      (values ptr length))))

(defun foreign-string-free (ptr)
  "Free a foreign string allocated by FOREIGN-STRING-ALLOC."
  (foreign-free ptr))

(defmacro with-foreign-string ((var-or-vars lisp-string &rest args) &body body)
  "VAR-OR-VARS is not evaluated and should be a list of the form
\(VAR &OPTIONAL BYTE-SIZE-VAR) or just a VAR symbol.  VAR is
bound to a foreign string containing LISP-STRING in BODY.  When
BYTE-SIZE-VAR is specified then bind the C buffer size
\(including the possible null terminator\(s)) to this variable."
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(multiple-value-bind (,var ,@(when size-var (list size-var)))
         (foreign-string-alloc ,lisp-string ,@args)
       (unwind-protect
            (progn ,@body)
         (foreign-string-free ,var)))))

(defmacro with-foreign-strings (bindings &body body)
  "See WITH-FOREIGN-STRING's documentation."
  (if bindings
      `(with-foreign-string ,(first bindings)
         (with-foreign-strings ,(rest bindings)
           ,@body))
      `(progn ,@body)))

(defmacro with-foreign-pointer-as-string
    ((var-or-vars size &rest args) &body body)
  "VAR-OR-VARS is not evaluated and should be a list of the form
\(VAR &OPTIONAL SIZE-VAR) or just a VAR symbol.  VAR is bound to
a foreign buffer of size SIZE within BODY.  The return value is
constructed by calling FOREIGN-STRING-TO-LISP on the foreign
buffer along with ARGS." ; fix wording, sigh
  (destructuring-bind (var &optional size-var)
      (ensure-list var-or-vars)
    `(with-foreign-pointer (,var ,size ,size-var)
       (progn
         ,@body
         (values (foreign-string-to-lisp ,var ,@args))))))

;;;# Automatic Conversion of Foreign Strings

(define-foreign-type foreign-string-type ()
  (;; CFFI encoding of this string.
   (encoding :initform nil :initarg :encoding :reader encoding)
   ;; Should we free after translating from foreign?
   (free-from-foreign :initarg :free-from-foreign
                      :reader fst-free-from-foreign-p
                      :initform nil :type boolean)
   ;; Should we free after translating to foreign?
   (free-to-foreign :initarg :free-to-foreign
                    :reader fst-free-to-foreign-p
                    :initform t :type boolean))
  (:actual-type :pointer)
  (:simple-parser :string))

;;; describe me
(defun fst-encoding (type)
  (or (encoding type) *default-foreign-encoding*))

;;; Display the encoding when printing a FOREIGN-STRING-TYPE instance.
(defmethod print-object ((type foreign-string-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~S" (fst-encoding type))))

(defmethod translate-to-foreign ((s string) (type foreign-string-type))
  (values (foreign-string-alloc s :encoding (fst-encoding type))
          (fst-free-to-foreign-p type)))

(defmethod translate-to-foreign (obj (type foreign-string-type))
  (cond
    ((pointerp obj)
     (values obj nil))
    ;; FIXME: we used to support UB8 vectors but not anymore.
    ;; ((typep obj '(array (unsigned-byte 8)))
    ;;  (values (foreign-string-alloc obj) t))
    (t (error "~A is not a Lisp string or pointer." obj))))

(defmethod translate-from-foreign (ptr (type foreign-string-type))
  (unwind-protect
       (values (foreign-string-to-lisp ptr :encoding (fst-encoding type)))
    (when (fst-free-from-foreign-p type)
      (foreign-free ptr))))

(defmethod free-translated-object (ptr (type foreign-string-type) free-p)
  (when free-p
    (foreign-string-free ptr)))

(defmethod expand-to-foreign-dyn-indirect
    (value var body (type foreign-string-type))
  (ext:with-gensyms (str)
    (expand-to-foreign-dyn
     value
     str
     (list 
      (expand-to-foreign-dyn-indirect str var body (parse-type :pointer)))
     type)))

;;;# STRING+PTR

(define-foreign-type foreign-string+ptr-type (foreign-string-type)
  ()
  (:simple-parser :string+ptr))

(defmethod translate-from-foreign (value (type foreign-string+ptr-type))
  (list (call-next-method) value))
